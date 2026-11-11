"""Extract the data tables from .usp project files (v10, v11 and v16 files tested).

A .usp file is a "BlockFile": 1024-byte blocks chained into named
streams. The streams hold objects serialized as
    u16 len + class name, u16 version   (repeated down the base classes to "USB")
followed by the fields: in sequence in v10 files, behind a per-class table of
field offsets from v11 on. Strings are u16 len + bytes, integers little-endian,
data values float32 with -99 marking a missing value.

Usage:  python usp_extract.py FILE_OR_FOLDER [...] [-o OUTDIR]
Writes, per project, one CSV per dataset (+ an .xlsx if openpyxl is installed)
and the project's audit trail.
"""
import argparse, csv, math, os, re, struct, sys

# ---------------------------------------------------------------- container

BLOCK = 1024
HDR = 19  # u32 block no, u8 flag, "BlockFile\0" or "Block   2\0", u32 next block
# directory entry: name[257], ctime[26], mtime[26], u32 first block, u32 size (+ u32 in "Block 2")
ENTRY = {b'BlockFile\0': 317, b'Block   2\0': 321}


def _chain(data, start):
    out, seen, cur = [], set(), start
    while cur and cur not in seen and cur * BLOCK <= len(data):
        seen.add(cur)
        out.append(cur)
        cur, = struct.unpack_from('<I', data, (cur - 1) * BLOCK + 15)
    return out


def _payload(data, blk):
    return data[(blk - 1) * BLOCK + HDR:blk * BLOCK]


def read_streams(path):
    """Return {stream name: bytes}. Deleted entries (size 0) are skipped."""
    data = open(path, 'rb').read()
    entry = ENTRY.get(data[5:15])  # v10-12 / v13+
    if not entry:
        raise ValueError('not a .usp BlockFile')
    streams = {}
    # each directory block holds 3 entries followed by an "UmCompoundFile" trailer
    for blk in _chain(data, 1):
        raw = _payload(data, blk)
        for off in range(0, 3 * entry, entry):
            e = raw[off:off + entry]
            name = e[:257].split(b'\0', 1)[0].decode('cp1252')
            start, size = struct.unpack_from('<II', e, 309)
            if not name or not size:
                continue
            body = bytearray()
            for b in _chain(data, start):
                p = _payload(data, b)
                used, = struct.unpack_from('<H', p, 0)
                body += p[2:2 + used]
            streams[name] = bytes(body[:size])
    return streams

# ------------------------------------------------------------ serialization

TYPES = {'float': '<f', 'double': '<d', 'unsigned long': '<I', '__int32': '<i',
         'long': '<i', 'int': '<i', 'short': '<h', 'unsigned short': '<H',
         'char': '<b', 'unsigned char': '<B', 'bool': '<?'}


class Reader:
    def __init__(self, buf, pfx=False):
        self.b, self.p = buf, 0
        self.pfx = pfx  # v13+: data strings start with an encoding byte

    def _un(self, fmt):
        v = struct.unpack_from(fmt, self.b, self.p)
        self.p += struct.calcsize(fmt)
        return v[0] if len(v) == 1 else v

    def u8(self): return self._un('<B')
    def u16(self): return self._un('<H')
    def u32(self): return self._un('<I')
    def i32(self): return self._un('<i')

    def raw(self):
        n = self.u16()
        self.p += n
        return self.b[self.p - n:self.p]

    def str(self):
        s = self.raw()
        if self.pfx and s:
            if s[0] != 0:
                raise ValueError('unknown string encoding %d at %d' % (s[0], self.p))
            s = s[1:]
            try:
                return s.split(b'\0', 1)[0].decode('utf-8')
            except UnicodeDecodeError:
                pass
        return s.split(b'\0', 1)[0].decode('cp1252', 'replace')

    def classes(self):
        """Read the class-name chain of one object, down to the USB root."""
        names = []
        while True:
            names.append(self.raw().decode('ascii'))
            self.u16()  # version
            if names[-1] == 'USB':
                return names
            if len(names) > 8:
                raise ValueError('bad class chain at %d: %r' % (self.p, names))

    def at(self, field):
        """A reader positioned at the start of a field (v11+)."""
        r = Reader(self.b, self.pfx)
        r.p = field[0]
        return r

    def fields(self):
        """v11+ field table, one per class level:
        'xA', u16 slots, u16 used, u16 ids[slots], u32 offsets[slots + 1]  (absolute,
        the last one is the end of the block; unused slots hold garbage).
        v13+ writes 'zA', u16 2, then the same with u64 offsets."""
        wide = self.b[self.p] == ord('z')
        self.p += 4 if wide else 2
        self.pfx |= wide
        slots, used = self.u16(), self.u16()
        ids = struct.unpack_from('<%dH' % slots, self.b, self.p)
        offs = struct.unpack_from('<%d%s' % (slots + 1, 'Q' if wide else 'I'), self.b, self.p + 2 * slots)
        bounds = list(offs[:used]) + [offs[slots]]
        self.p = offs[slots]
        return {ids[i]: (bounds[i], bounds[i + 1]) for i in range(used)}

    def obj(self, cls):
        """Class chain of an object; for v11+ also its field tables, base class first.
        Returns None for the older format, whose fields simply follow."""
        got = self.classes()
        if cls not in got:  # v13+ wraps some classes (NCO/NCV around VO)
            raise ValueError('expected %s, got %r at %d' % (cls, got, self.p))
        if self.b[self.p:self.p + 2] not in (b'xA', b'zA'):
            return None
        levels = []
        for _ in got[:-1]:
            if self.b[self.p:self.p + 2] not in (b'xA', b'zA'):  # v16 files: "PMo" v1 before the PLS level
                self.raw()
                self.u16()
            levels.append(self.fields())
        return levels

    def vector(self, cls='NCIV', with_missing=False):
        """Typed vector: NCIV (index), NCDV (data row, carries a missing count)."""
        lv = self.obj(cls)
        if lv is None:
            fmt = TYPES[self.str()]
            n = self.u32()
            nmiss = self.u32() if with_missing else None
            start = self.p
            self.p += n * struct.calcsize(fmt)
        else:  # no type name any more: count, [missing count], values
            f = lv[0]
            r = self.at(f[0])
            n = r.u32()
            nmiss = self.at(f[1]).u32() if with_missing else None
            start, end = (f.get(2, (0, 0)) if with_missing else (r.p, f[0][1]))  # empty: no values field
            width = (end - start) // n if n else 4
            fmt = {4: '<f' if cls == 'NCDV' else '<I', 8: '<d'}[width]
        vals = [v[0] for v in struct.iter_unpack(fmt, self.b[start:start + n * struct.calcsize(fmt)])]
        return (vals, nmiss) if with_missing else vals

    def strings(self):
        lv = self.obj('NCSV')
        r = self if lv is None else self.at(lv[0][0])
        return [r.str() for _ in range(r.u32())]

    def matrix(self):
        """NCDM: rows of NCDV vectors, each carrying its count of -99 (missing) values."""
        lv = self.obj('NCDM')
        if lv is None:
            self.str()
            nrows, ncols, nmiss = self.u32(), self.u32(), self.u32()
            r = self
        else:
            f = lv[0]
            nrows, ncols, nmiss = (self.at(f[i]).u32() for i in (0, 1, 2))
            if 3 not in f:  # empty matrix (e.g. a model with no components yet)
                return [], nmiss
            r = self.at(f[3])
        rows = []
        for _ in range(nrows):
            vals, row_miss = r.vector('NCDV', with_missing=True)
            if len(vals) != ncols or sum(v == MISSING for v in vals) != row_miss:
                raise ValueError('matrix row %d does not check out' % len(rows))
            rows.append(vals)
        if lv is None:
            self.vector()  # missing count per column
        return rows, nmiss

# ------------------------------------------------------------------ objects

MISSING = -99.0  # the files' internal missing-value marker


def parse_items(buf):
    """ncobservations / ncvariables: id column names and one record per item."""
    r = Reader(buf)
    lv = r.obj('VO')
    if lv is None:
        n, nids = r.u32(), r.u32()
        r.p += nids  # one flag byte per id column
        id_names = [r.str() for _ in range(nids)]
    else:
        f = lv[0]
        n, nids = r.at(f[0]).u32(), r.at(f[1]).u32()
        h = r.at(f[3])
        id_names = [h.str() for _ in range(h.u32())]
        r = r.at(f[4])
    items = []
    for _ in range(n):
        tag = r.str()  # polymorphic records: runtime type name, then the class chain
        p = r.p
        cls = r.classes()
        r.p = p
        lv = r.obj(cls[0])
        if lv is None:
            ids = [r.str() for _ in range(r.u32())]
            item = {'class': tag, 'ids': ids, 'uid': r.u32(), 'formula': ''}
            if 'VIO' in cls:
                r.p += 4
                item['formula'] = r.str() if r.u8() else ''
                r.i32()
            if 'QVI' in cls:
                item['categories'] = r.strings()
        else:  # r is already past the record; read its fields where the tables point
            h = r.at(lv[0][0])
            ids = [h.str() for _ in range(h.u32())]
            item = {'class': tag, 'ids': ids, 'uid': r.at(lv[0][1]).u32(), 'formula': ''}
            if 'QVI' in cls:
                item['categories'] = r.at(lv[2][0]).strings()
        items.append(item)
    return id_names, items


def parse_dataset(buf):
    r = Reader(buf)
    lv = r.obj('NCDS')
    if lv is None:
        r.u8()
        log = r.str()
        if r.u8() != 0:  # the import log is the one string stored with its NUL
            raise ValueError('unexpected dataset header')
        name = r.str()
        obs_uids = r.vector()
        var_uids = r.vector()
    else:
        f = lv[0]
        log, name = r.at(f[1]).str(), r.at(f[4]).str()
        obs_uids, var_uids = r.at(f[5]).vector(), r.at(f[6]).vector()
        r = r.at(f[7])
    rows, nmiss = r.matrix()
    if len(obs_uids) != len(rows) or any(len(row) != len(var_uids) for row in rows):
        raise ValueError('dataset shape mismatch')
    return {'name': name, 'log': log, 'obs_uids': obs_uids, 'var_uids': var_uids,
            'rows': [[None if v == MISSING else v for v in row] for row in rows], 'missing': nmiss}


# ------------------------------------------------------------------- models
#
# ncmodel<N> holds a PCA or PLS model: PModel (base) then the PCA/PLS level.
# Field ids, v11 (v10 writes the same fields in order, without tables):
#   PModel  f0 model no, f1 components used (A), f2 computed, f3 obs uids, f4 X terms,
#           f5 obs residual SS, f6 SSX per component (0..A), f7 SSX per variable,
#           f8 Q2, f9 Q2cum, f11 eigenvalues, f19/f20 cross-validation rounds,
#           f26 (v11+) each observation's cross-validation group, from 0 (checked against the
#           groups the writing program exports, on 128 models)
#   PLS     f0 P, f1 W, f2 C, f3 U, f4 T, f5 Y terms, f6 SSY per component
#   PCA     f0 P, f1 T
# v16 adds an OnMo level (f0 computed) and moves P/T to PModel f28/f29; its PLS
# level is W f0, C f1, U f2, Y terms f3, and SSY is no longer stored.

def term(r):
    """One CT object: (var uids, lags, category numbers), as tuples so it can key a dict.
    A qualitative variable expands into one 0/1 term per category; two uids make a cross term."""
    f = r.obj('CT')
    if f is None:  # v10: three optional vectors
        vecs = [r.vector() if r.u8() else [] for _ in range(3)]
    else:
        vecs = [r.at(f[0][i]).vector() if i in f[0] else [] for i in range(3)]
    return tuple(tuple(v) for v in vecs)


def columnterms(r):
    """X or Y block terms."""
    lv = r.obj('ColumnTerms')
    s = r if lv is None else r.at(lv[0][0])
    terms = []
    for _ in range(s.u32()):
        if lv is None or s.u8():  # v11+: pointers, each with a present flag
            terms.append(term(s))
    return terms


def _v10_skip_to_matrix(r):
    """v10: jump over the cross-validation and option fields to the next matrix."""
    p = r.b.find(b'\x04\x00NCDM\x02\x00\x03\x00USB\x01\x00', r.p)
    if p < 0:
        raise ValueError('model layout not recognised')
    r.p = p


def _parse_model(buf):
    r = Reader(buf)
    q = r.p
    kind = r.classes()[0]
    r.p = q
    if kind not in ('PLS', 'PCA'):
        raise ValueError('unsupported model type %s' % kind)
    lv = r.obj('PModel')
    m = {'type': kind, 'version': 10 if lv is None else 11 if len(lv) == 2 else 13,
         'yterms': [], 'C': [], 'U': [], 'W': [], 'ssy': None, 'ssyv': None,
         'q2v': None, 'q2vcum': None, 'cvP': None, 'cvW': None, 'cvC': None, 'cvgroups': None}
    opt = lambda: r.matrix()[0] if r.u8() else None  # v10 pointer: present flag + matrix
    if lv is None:  # v10
        r.u32()
        m['A'], m['computed'] = r.u32(), r.u32()
        m['obs'] = r.vector()
        r.u8()
        m['xterms'] = columnterms(r)
        m['dmodx'] = r.matrix()[0]
        m['ssx'] = r.vector('NCDV', True)[0]
        m['ssxv'] = r.matrix()[0]
        m['q2'], m['q2cum'], _, m['eig'] = (r.vector('NCDV', True)[0] for _ in range(4))
        for _ in range(4):
            r.vector('NCDV', True)
        r.vector()
        r.u8(), r.u8()
        cv = []  # cross-validation rounds (T, then P): one matrix per component, a row per round
        for _ in range(2):
            cv.append([r.matrix()[0] for _ in range(r.u32())] if r.u8() else None)
        m['cvP'] = cv[1]
        if r.u8():
            r.vector()
        r.vector()
        _v10_skip_to_matrix(r)  # u32 + an option block without a class chain
        if kind == 'PLS':
            m['P'], m['W'], m['C'], m['U'], m['T'] = (r.matrix()[0] for _ in range(5))
            r.u8()
            m['yterms'] = columnterms(r)
            m['ssy'] = r.vector('NCDV', True)[0]
            m['ssyv'] = r.matrix()[0]
            opt()  # SS per cross-validation round
        else:
            m['P'], m['T'] = (r.matrix()[0] for _ in range(2))
        m['q2v'], m['q2vcum'] = opt(), opt()  # per Y variable (PLS) or X variable (PCA)
        if kind == 'PLS':  # cross-validation rounds of W and C
            m['cvW'], m['cvC'] = ([r.matrix()[0] for _ in range(r.u32())] for _ in range(2))
        return m
    f = lv[0]
    at = lambda fld: r.at(fld)

    def rounds(fld):  # pointer vector of per-component cross-validation matrices
        s = r.at(fld)
        return [s.matrix()[0] if s.u8() else None for _ in range(s.u32())]

    m['cvP'] = rounds(f[20]) if 20 in f else None
    m['cvgroups'] = at(f[26]).vector() if 26 in f else None
    m['A'] = at(f[1]).u32()
    m['obs'] = at(f[3]).vector()
    m['xterms'] = columnterms(at(f[4]))
    m['ssx'] = at(f[6]).vector('NCDV', True)[0]
    m['ssxv'] = at(f[7]).matrix()[0]
    m['q2'], m['q2cum'], m['eig'] = (at(f[i]).vector('NCDV', True)[0] for i in (8, 9, 11))
    g = lv[-1]
    if len(lv) == 2:  # v11
        m['computed'] = at(f[2]).u32()
        m['dmodx'] = at(f[5]).matrix()[0]
        if kind == 'PLS':
            m['P'], m['W'], m['C'], m['U'], m['T'] = (at(g[i]).matrix()[0] for i in range(5))
            m['yterms'] = columnterms(at(g[5]))
            m['ssy'] = at(g[6]).vector('NCDV', True)[0]
            m['ssyv'] = at(g[7]).matrix()[0]
            m['cvW'], m['cvC'] = (rounds(g[i]) if i in g else None for i in (11, 12))
            q2 = (9, 10)
        else:
            m['P'], m['T'] = at(g[0]).matrix()[0], at(g[1]).matrix()[0]
            q2 = (2, 3)
    else:  # v13+
        m['computed'] = at(lv[1][0]).u32()
        m['dmodx'] = at(lv[1][1]).matrix()[0]
        m['P'], m['T'] = at(f[28]).matrix()[0], at(f[29]).matrix()[0]
        if kind == 'PLS':
            m['W'], m['C'], m['U'] = (at(g[i]).matrix()[0] for i in range(3))
            m['yterms'] = columnterms(at(g[3]))
            m['cvW'], m['cvC'] = (rounds(g[i]) if i in g else None for i in (6, 4))
            q2 = (8, 9)
        else:
            q2 = (2, 3)
    if all(i in g for i in q2):
        m['q2v'], m['q2vcum'] = (at(g[i]).matrix()[0] for i in q2)
    return m


def parse_model(buf):
    m = _parse_model(buf)
    for k in 'PWCUT':  # an observation with no usable X value gets the missing marker as score
        m[k] = [[None if v == MISSING else v for v in row] for row in m[k]]
    return m


def parse_scaling(buf):
    """ncpspipe<N>: [{term: (scale, centre)}] for the X block then the Y block.
    A scaled value is (x - centre) * scale; scale is 1/SD for unit variance."""
    blocks = []
    for m in re.finditer(rb'\x0c\x00ScaleSegment..\x03\x00USB', buf, re.S):  # the class chain, after its tag
        r = Reader(buf, pfx=b'zA' in buf[:64])
        r.p = m.start()
        lv = r.obj('ScaleSegment')
        seg = {}
        if lv is None:
            for _ in range(r.u32()):
                scale, centre = r._un('<f'), r._un('<f')
                seg[term(r)] = (scale, centre)
        else:
            s = r.at(lv[0][0])
            for _ in range(s.u32()):
                if s.u8():  # PSS: f0 scale, f1 centre, f2 CT
                    f = s.obj('PSS')[0]
                    seg[term(s.at(f[2]))] = (s.at(f[0])._un('<f'), s.at(f[1])._un('<f'))
        blocks.append(seg)
    return blocks


def compose_scaling(blocks):
    """{term: (scale, centre)} over all the segments of a pipe. A term can be scaled by several
    segments in turn: first its own scaling (UV, centring, Pareto, times its modifier), then, for
    block scaling, 1/sqrt(K) with centre 0. Each segment computes (x - centre) * scale on the
    previous one's output, so the whole is (x - C) * S with S the product of the scales and
    C = c1 + c2 / s1 + c3 / (s1 s2) + ..."""
    out = {}
    for seg in blocks:
        for t, (s, c) in seg.items():
            if t not in out:
                out[t] = (s, c)
                continue
            S, C = out[t]
            out[t] = (S * s, C + c / S if S else C)
    return out


def parse_cvgroups(buf):
    """ncCVgroups<N> (v11+), the model's cross-validation setting: 'method' (0 the writing
    program's default, observations in turn; 3 by an observation ID), 'option' (2: the
    observations with the same ID value in the same group), 'id' (the ID's place among the
    dataset's IDs, -1 for none), 'requested' and 'groups' (the number of groups asked for and
    obtained). Field meanings read from a project whose groups were also exported; None if absent."""
    if not buf:
        return None
    r = Reader(buf)
    r.raw()  # 'CVG', then its version
    r.u16()
    if buf[r.p:r.p + 2] not in (b'xA', b'zA'):
        return None
    f = r.fields()
    fmt = {2: '<H', 4: '<i', 8: '<q'}
    val = lambda k: struct.unpack_from(fmt[f[k][1] - f[k][0]], buf, f[k][0])[0] if k in f and f[k][1] - f[k][0] in fmt else None
    return {'method': val(0), 'option': val(1), 'id': val(2), 'requested': val(3), 'groups': val(4)}


def parse_transforms(buf):
    """ncpspipe<N> TransformSegment (seen only in v16 files): {term: (type, params)}.
    Type 7 is the power transform (C1*x + C2)^C3, params (?, C1, C2, C3)."""
    out = {}
    for m in re.finditer(rb'\x10\x00TransformSegment..\x03\x00USB', buf, re.S):
        r = Reader(buf, pfx=b'zA' in buf[:64])
        r.p = m.start()
        lv = r.obj('TransformSegment')
        if lv is None:
            continue
        terms = columnterms(r.at(lv[0][0]))
        s = r.at(lv[0][1])
        specs = []
        for _ in range(s.u32()):
            if s.u8():
                f = s.obj('TS')[0]
                a, e = f.get(1, (0, 0))
                specs.append((s.at(f[0]).u16(), struct.unpack_from('<%df' % ((e - a) // 4), buf, a)))
        out.update(zip(terms, specs))
    return out


def transform_label(name, spec):
    typ, prm = spec
    if typ == 7 and len(prm) == 4:
        _, c1, c2, c3 = prm
        if c3 == 1 and c1 == 1 and c2 == 0:
            return name
        inner = name if (c1, c2) == (1, 0) else '(%g*%s%+g)' % (c1, name, c2)
        return inner if c3 == 1 else '%s^%g' % (inner, c3)
    return '%s [transform %d %s]' % (name, typ, ' '.join('%g' % p for p in prm))


def parse_workset(buf):
    """ncwsdata<N>: the scaled [X, Y] the model was fitted on (Y absent for PCA), -99 = missing."""
    r = Reader(buf)
    lv = r.obj('PSData')
    mats = []
    if lv is None:  # v10: X block then Y block, each a BlockData starting with its matrix
        for _ in range(2):
            p = buf.find(b'\x04\x00NCDM\x02\x00\x03\x00USB\x01\x00', r.p)
            if p < 0:
                break
            r.p = p
            mats.append(r.matrix()[0])
    else:
        for fid in (0, 1):
            if fid in lv[0]:
                s = r.at(lv[0][fid])
                g = s.obj('BlockData')[0]
                if 0 in g:
                    mats.append(s.at(g[0]).matrix()[0])
    return mats


def _inverse(M):
    """Gauss-Jordan inverse of a small square matrix (lists)."""
    n = len(M)
    a = [list(map(float, row)) + [float(i == j) for j in range(n)] for i, row in enumerate(M)]
    for c in range(n):
        piv = max(range(c, n), key=lambda i: abs(a[i][c]))
        a[c], a[piv] = a[piv], a[c]
        d = a[c][c]
        a[c] = [v / d for v in a[c]]
        for i in range(n):
            if i != c and a[i][c]:
                f = a[i][c]
                a[i] = [v - f * w for v, w in zip(a[i], a[c])]
    return [row[n:] for row in a]


def term_name(t, var_by):
    uids, lags, cats = t
    parts = []
    for i, vu in enumerate(uids):
        v = var_by.get(vu)
        name = v['ids'][0] if v else '#%d' % vu
        if i < len(cats) and v and v.get('categories') and 1 <= cats[i] <= len(v['categories']):
            name += '(%s)' % v['categories'][cats[i] - 1]
        if i < len(lags) and lags[i]:
            name += '(lag %d)' % lags[i]
        parts.append(name)
    return '*'.join(parts)


def parse_modelinfo(buf):
    """ncmodelinfo<N>: (name, title) as the user typed them, e.g. ('M3', 'sans interaction')."""
    r = Reader(buf)
    lv = r.obj('ModelInfo')
    if lv is None:  # v10: u32, NCIV, NCSV, 4 flag bytes, then the two strings
        r.u32()
        r.vector()
        r.strings()
        r.p += 4
        return r.str(), r.str()
    f = lv[0]
    return tuple(r.at(f[i]).str() if i in f else '' for i in (9, 10))


def _betacf(a, b, x):
    """Continued fraction of the incomplete beta function (Numerical Recipes)."""
    qab, qap, qam = a + b, a + 1, a - 1
    tiny = 1e-300
    c, d = 1.0, 1 - qab * x / qap
    d = 1 / (d if abs(d) > tiny else tiny)
    h = d
    for m in range(1, 1000):
        m2 = 2 * m
        for aa in (m * (b - m) * x / ((qam + m2) * (a + m2)), -(a + m) * (qab + m) * x / ((a + m2) * (qap + m2))):
            d = 1 + aa * d
            d = 1 / (d if abs(d) > tiny else tiny)
            c = 1 + aa / c
            c = c if abs(c) > tiny else tiny
            h *= d * c
        if abs(d * c - 1) < 1e-14:
            break
    return h


def _f_cdf(f, d1, d2):
    """P(F <= f) for an F(d1, d2) variable: the regularised incomplete beta I_x(d1/2, d2/2)."""
    if f <= 0:
        return 0.0
    a, b, x = d1 / 2, d2 / 2, d1 * f / (d1 * f + d2)
    front = math.exp(math.lgamma(a + b) - math.lgamma(a) - math.lgamma(b) + a * math.log(x) + b * math.log1p(-x))
    if x < (a + 1) / (a + b + 2):
        return front * _betacf(a, b, x) / a
    return 1 - front * _betacf(b, a, 1 - x) / b


def f_quantile(p, d1, d2):
    lo, hi = 0.0, 1.0
    while _f_cdf(hi, d1, d2) < p:
        hi *= 2
    for _ in range(200):
        mid = (lo + hi) / 2
        lo, hi = (mid, hi) if _f_cdf(mid, d1, d2) < p else (lo, mid)
    return (lo + hi) / 2


def _coefs(W, P, C):
    """W* = W(P'W)^-1 (K x A) and CoeffCS = W*C' (K x M) from A x K weights/loadings, A x M C."""
    A, K, M = len(W), len(W[0]), len(C[0])
    inv = _inverse([[sum(P[a][k] * W[b][k] for k in range(K)) for b in range(A)] for a in range(A)])
    Ws = [[sum(W[a][k] * inv[a][b] for a in range(A)) for b in range(A)] for k in range(K)]
    return Ws, [[sum(Ws[k][b] * C[b][j] for b in range(A)) for j in range(M)] for k in range(K)]


def _jackknife(rounds):
    """95% confidence half-width from cross-validation rounds (one value per round):
    t(0.975, G - 1) * sqrt((G - 1) / G * sum((v_g - mean)^2)), the jack-knife estimate."""
    G = len(rounds)
    if G < 2 or any(v is None for v in rounds):
        return None
    mean = sum(rounds) / G
    return f_quantile(0.95, 1, G - 1) ** 0.5 * ((G - 1) / G * sum((v - mean) ** 2 for v in rounds)) ** 0.5


def _aligned(rounds, ref, A):
    """Per-round A x K matrices from per-component CV matrices, signs flipped to match ref."""
    G = len(rounds[0])
    out = []
    for g in range(G):
        rows = []
        for a in range(A):
            v = [None if x == MISSING else x for x in rounds[a][g]]
            dot = sum(x * y for x, y in zip(v, ref[a]) if x is not None and y is not None)
            rows.append([None if x is None else (x if dot >= 0 else -x) for x in v])
        out.append(rows)
    return out


def dcrit(N, K, A, C=1, p=0.95):
    """Normalised critical DModX: sqrt of the F quantile with
    DF_mod = (N - C - A)(K - A) and DF_obs = (min(K, 100, DF_mod) - A) / (N / (N - C - A))."""
    dfm = max((N - C - A) * (K - A), 1)
    m_ = min(K, 100, dfm)
    corr = N / max(N - C - A, 1)
    dfo = ((m_ + (K - dfm) ** 0.5 - A) if K > dfm else (m_ - A)) / corr
    return f_quantile(p, max(dfo, 1), dfm) ** 0.5


def mpow(m, A):
    """Modelling power per X term, 1 - SV_k / SV0_k (residual over initial SD of the variable)."""
    N, ssxv = len(m['obs']), m['ssxv']
    if not ssxv or len(ssxv) <= A or N <= A + 1:
        return None
    return [1 - (ssxv[A][k] / (N - A - 1)) ** 0.5 / (ssxv[0][k] / (N - 1)) ** 0.5 if ssxv[0][k] else 0.0
            for k in range(len(ssxv[0]))]


def model_report(m, scaling, ws, var_by, transforms=None, qual=None):
    """The usual model results, from what is stored (scores, loadings, SS, Q2, DModX)
    plus the standard formulas (W*, coefficients, VIP, normalised DModX, T2, limits),
    restricted to the A components kept in the model."""
    A = min(m['A'], len(m['T']))
    pls = m['type'] == 'PLS'
    N, K = len(m['obs']), len(m['xterms'])
    ssx, ssy, ssyv = m['ssx'], m['ssy'], m['ssyv']
    Ys = [[None if v == MISSING else v for v in row] for row in ws[1]] if pls and len(ws) > 1 else None
    if pls and not ssy and Ys:  # v13+: deflate the stored scaled Y with T and C
        F, M_ = Ys, len(m['yterms'])
        ssyv = [[sum(row[j] ** 2 for row in F if row[j] is not None) for j in range(M_)]]
        for a in range(len(m['T'])):
            t, c = m['T'][a], m['C'][a]
            F = [row if t[i] is None else [None if v is None else v - t[i] * c[j] for j, v in enumerate(row)]
                 for i, row in enumerate(F)]
            # like the program that wrote the file, observations left without a score drop out of the residual SS
            ssyv.append([sum(row[j] ** 2 for i, row in enumerate(F) if t[i] is not None and row[j] is not None)
                         for j in range(M_)])
        ssy = [sum(v) for v in ssyv]
    comps = []
    for a in range(1, A + 1):
        row = {'R2X': (ssx[a - 1] - ssx[a]) / ssx[0], 'R2X(cum)': 1 - ssx[a] / ssx[0],
               'Eigenvalue': m['eig'][a - 1], 'Q2': m['q2'][a - 1], 'Q2(cum)': m['q2cum'][a - 1]}
        if pls and ssy:
            row['R2Y'] = (ssy[a - 1] - ssy[a]) / ssy[0]
            row['R2Y(cum)'] = 1 - ssy[a] / ssy[0]
        comps.append(row)
    xs = ys = compose_scaling(scaling)  # expanded terms get their own segment; block scaling a second one
    tr = transforms or {}  # transformed terms are named as such: their coefficients are per transformed unit
    name = lambda t: transform_label(term_name(t, var_by), tr[t]) if t in tr else term_name(t, var_by)
    xnames = [name(t) for t in m['xterms']]
    ynames = [name(t) for t in m['yterms']]
    M_ = len(ynames)
    sx = [xs.get(t) for t in m['xterms']]
    sy = [ys.get(t) for t in m['yterms']]

    # PLS-DA: Y is the stored 0/1 class dummies; each observation's class is the 1 in its observed Y
    obs_y = None
    if pls and Ys and all(sy):
        obs_y = [[None if v is None else v / sy[j][0] + sy[j][1] for j, v in enumerate(row)] for row in Ys]
    classes, class_note = None, ''
    if obs_y and ynames and all(re.fullmatch(r'\$M\d+\.DA\d+', y) for y in ynames):
        classes = [max(range(M_), key=lambda j: -1 if row[j] is None else row[j]) + 1 for row in obs_y]
        labels = {j + 1: 'class %d' % (j + 1) for j in range(M_)}
        for var, by_obs in (qual or {}).items():  # name classes after a variable that matches them exactly
            seen = {}
            if all(by_obs.get(o) is not None and seen.setdefault(c, by_obs[o]) == by_obs[o]
                   for o, c in zip(m['obs'], classes)) and len(set(seen.values())) == len(seen) == M_:
                labels = {c: '%s=%s' % (var, lab) for c, lab in seen.items()}
                class_note = 'PLS-DA classes match variable %s' % var
                break
        ynames = ['%s (%s)' % (y, labels[j + 1]) for j, y in enumerate(ynames)]
        classes = [labels[c] for c in classes]

    vrows = [{'Term': n, 'Block': 'X'} for n in xnames]
    for a in range(A):
        for k in range(K):
            vrows[k]['p%d' % (a + 1)] = m['P'][a][k]
    cv_ok = lambda r: r and len(r) >= A and all(x is not None and len(x) > 1 for x in r[:A])
    if A and cv_ok(m['cvP']) and not pls:  # PCA: jack-knife of p from the cross-validation rounds
        Pg = _aligned(m['cvP'], m['P'], A)
        for a in range(A):
            for k in range(K):
                vrows[k]['p%d CI' % (a + 1)] = _jackknife([r[a][k] for r in Pg])
    if m['ssxv'] and A:
        for k in range(K):
            v0 = m['ssxv'][0][k]
            vrows[k]['R2V(cum)'] = 1 - m['ssxv'][A][k] / v0 if v0 else None
    if not pls and m['q2vcum'] and A:
        for k in range(K):
            vrows[k]['Q2V(cum)'] = m['q2vcum'][A - 1][k]
    const = None
    if pls and A:
        W, P, C = m['W'][:A], m['P'][:A], m['C'][:A]
        Ws, coef_cs = _coefs(W, P, C)
        d = [ssy[a] - ssy[a + 1] for a in range(A)] if ssy else None
        raw = all(sx) and all(sy)
        ci = {}  # jack-knife half-widths from the cross-validation rounds of W, P and C
        if cv_ok(m['cvP']) and cv_ok(m['cvW']) and cv_ok(m['cvC']):
            Wg = _aligned(m['cvW'], W, A)
            flip = [[1 if Wg[g][a] == [None if x == MISSING else x for x in m['cvW'][a][g]] else -1
                     for a in range(A)] for g in range(len(Wg))]
            Pg = [[[None if x == MISSING else x * flip[g][a] for x in m['cvP'][a][g]] for a in range(A)]
                  for g in range(len(Wg))]
            Cg = [[[None if x == MISSING else x * flip[g][a] for x in m['cvC'][a][g]] for a in range(A)]
                  for g in range(len(Wg))]
            per = []
            for g in range(len(Wg)):
                try:
                    per.append(_coefs(Wg[g], Pg[g], Cg[g]))
                except (TypeError, ZeroDivisionError):
                    per = None
                    break
            for k in range(K):
                for a in range(A):
                    ci['p', a, k] = _jackknife([r[a][k] for r in Pg])
                    if per:
                        ci['w*', a, k] = _jackknife([r[0][k][a] for r in per])
                for j in range(M_):
                    if per:
                        ci['b', k, j] = _jackknife([r[1][k][j] for r in per])
            for a in range(A):
                for j in range(M_):
                    ci['c', a, j] = _jackknife([r[a][j] for r in Cg])
        for k in range(K):
            row = vrows[k]
            if d:
                row['VIP'] = (K * sum(W[a][k] ** 2 * d[a] for a in range(A)) / sum(d)) ** 0.5
            for j, y in enumerate(ynames):
                row['CoeffCS %s' % y] = coef_cs[k][j]
                row['CoeffCS %s CI' % y] = ci.get(('b', k, j))
                if raw:  # x_s = (x - cx) * sx, y_s = (y - cy) * sy  =>  y = cy + sum(b_s * sx / sy * (x - cx))
                    row['Coeff %s' % y] = coef_cs[k][j] * sx[k][0] / sy[j][0]
                    if ci.get(('b', k, j)) is not None:
                        row['Coeff %s CI' % y] = ci['b', k, j] * sx[k][0] / sy[j][0]
            for b in range(A):
                row['w*%d' % (b + 1)] = Ws[k][b]
                row['w*%d CI' % (b + 1)] = ci.get(('w*', b, k))
                row['w%d' % (b + 1)] = W[b][k]
                row['p%d CI' % (b + 1)] = ci.get(('p', b, k))
        if raw:
            const = {'Term': 'Constant', 'Block': 'X'}
            for j, y in enumerate(ynames):
                const['Coeff %s' % y] = sy[j][1] - sum(vrows[k]['Coeff %s' % y] * sx[k][1] for k in range(K))
        for j, y in enumerate(ynames):
            row = {'Term': y, 'Block': 'Y'}
            for a in range(A):
                row['c%d' % (a + 1)] = C[a][j]
                row['c%d CI' % (a + 1)] = ci.get(('c', a, j))
            if ssyv and ssyv[0][j]:
                row['R2V(cum)'] = 1 - ssyv[A][j] / ssyv[0][j]
            if m['q2vcum']:
                row['Q2V(cum)'] = m['q2vcum'][A - 1][j]
            vrows.append(row)
    for row, sc in zip(vrows, sx + sy):
        if sc:
            row['Centre'], row['Scale (1/SD)'] = sc[1], sc[0]
    if const:
        vrows.insert(K, const)
    both = lambda names: [h for n in names for h in (n, n + ' CI')]
    order = ['Term', 'Block', 'VIP'] + both('CoeffCS %s' % y for y in ynames) + both('Coeff %s' % y for y in ynames)
    order += ['R2V(cum)', 'Q2V(cum)', 'RMSEE'] + both('p%d' % (a + 1) for a in range(A))
    order += both('w*%d' % (a + 1) for a in range(A)) + ['w%d' % (a + 1) for a in range(A)]
    order += both('c%d' % (a + 1) for a in range(A)) + ['Centre', 'Scale (1/SD)']
    header = [h for h in order if any(row.get(h) is not None for row in vrows)]

    obs_cols = {'t%d' % (a + 1): m['T'][a] for a in range(A)}
    if pls:
        obs_cols.update({'u%d' % (a + 1): m['U'][a] for a in range(A)})
    if classes:
        obs_cols['Class'] = classes
    limits = {}
    w, sw0, rep_dmodx = None, None, 'DModX'
    scored = [i for i in range(N) if A and m['T'][0][i] is not None]
    if A and len(scored) > A + 1:
        # Hotelling's T2 = sum over components of t^2 / s_t^2, s_t^2 the score variance (N - 1)
        Nt = len(scored)
        var = [sum(m['T'][a][i] ** 2 for i in scored) / (Nt - 1) for a in range(A)]
        obs_cols['T2'] = [None if m['T'][0][i] is None else sum(m['T'][a][i] ** 2 / var[a] for a in range(A))
                          for i in range(N)]
        for p in (0.95, 0.99):  # as a v17 report prints it (checked against one)
            limits['T2Crit(%d%%)' % round(p * 100)] = A * (Nt - 1) / (Nt - A) * f_quantile(p, A, Nt - A)
    if A and K > A and N > A + 1 and m.get('dmodx') and len(m['dmodx']) > A:
        # stored: absolute DModX = sqrt(SS_i / (K_i - A) * N / (N - A - 1)) per component;
        # normalised by s0 = sqrt(SSX / ((N - A - 1)(K - A))), the model's pooled residual SD
        s0 = (ssx[A] / ((N - A - 1) * (K - A))) ** 0.5
        obs_cols['DModX'] = [None if v == MISSING else v / s0 for v in m['dmodx'][A]]
        obs_cols['DModX (absolute)'] = [None if v == MISSING else v for v in m['dmodx'][A]]
        limits['DCrit(0.05)'] = dcrit(N, K, A)
        w = mpow(m, A)
        if w and ws:  # v13+ default: residuals weighted by the variables' modelling power
            v = (N / (N - A - 1)) ** 0.5
            res = []
            for i, row in enumerate(ws[0]):
                if m['T'][0][i] is None:
                    res.append(None)
                    continue
                e = [None if x == MISSING else (x - sum(m['T'][a][i] * m['P'][a][k] for a in range(A))) * w[k]
                     for k, x in enumerate(row)]
                res.append([x for x in e if x is not None])
            sw0 = (sum(x * x for e in res if e for x in e) / ((N - A - 1) * (K - A))) ** 0.5 or None
            if sw0:
                obs_cols['DModX (weighted)'] = [
                    None if not e or len(e) <= A else (sum(x * x for x in e) / (len(e) - A)) ** 0.5 * v / sw0
                    for e in res]
        rep_dmodx = 'DModX (weighted)' if m['version'] >= 13 and 'DModX (weighted)' in obs_cols else 'DModX'
    if pls and A and all(sy):
        for j, y in enumerate(ynames):  # fitted values back in the units of the data
            fit = [None if m['T'][0][i] is None
                   else sy[j][1] + sum(m['T'][a][i] * m['C'][a][j] for a in range(A)) / sy[j][0]
                   for i in range(N)]
            if obs_y:
                obs_cols['%s observed' % y] = [row[j] for row in obs_y]
            obs_cols['%s fitted' % y] = fit
            if obs_y:
                res = [None if o is None or f is None else o - f for o, f in zip(obs_cols['%s observed' % y], fit)]
                obs_cols['%s residual' % y] = res
                n = sum(v is not None for v in res)
                if n > A + 1:  # RMSEE = sqrt(SS residuals / (N - A - 1)), matches the reported one to 5 decimals
                    rmsee = (sum(v * v for v in res if v is not None) / (n - A - 1)) ** 0.5
                    limits.setdefault('RMSEE', []).append(rmsee)
                    for row in vrows:
                        if row['Term'] == y and row['Block'] == 'Y':
                            row['RMSEE'] = rmsee
    return {'A': A, 'computed': len(m['T']), 'components': comps, 'xnames': xnames, 'ynames': ynames,
            'var_header': header, 'var_rows': [[row.get(h) for h in header] for row in vrows],
            'obs_cols': obs_cols, 'limits': limits, 'note': class_note, 'type': m['type'], 'vars': vrows,
            'dmodx_default': rep_dmodx, 'mpow': w, 'sw0': sw0}


def fmt_float(v):
    """Shortest decimal that round-trips the stored float32 (407.2, not 407.200012)."""
    if v == int(v) and abs(v) < 2 ** 24:
        return int(v)
    for prec in range(6, 10):
        s = '%.*g' % (prec, v)
        if struct.pack('<f', float(s)) == struct.pack('<f', v):
            return float(s)
    return v


def tables(path):
    """Yield (dataset name, header, rows, info) for every dataset in a project (path or streams)."""
    st = read_streams(path) if isinstance(path, str) else path
    obs_ids, obs = parse_items(st['ncobservations'])
    _, vars_ = parse_items(st['ncvariables'])
    obs_by = {o['uid']: o for o in obs}
    var_by = {v['uid']: v for v in vars_}
    for key in sorted((k for k in st if k.startswith('ncdataset')), key=lambda k: int(k[9:] or 0)):
        ds = parse_dataset(st[key])
        cols = [var_by[u] for u in ds['var_uids']]
        header = list(obs_ids) + [c['ids'][0] for c in cols]
        aux = []  # PLS-DA class columns kept beside the dataset ($M<n>.DA<k>)
        try:
            aux = [a for a in parse_auxiliary(st.get('ncauxiliarydata' + key[9:], b''))
                   if len(a[2]) == len(ds['rows'])] if st.get('ncauxiliarydata' + key[9:]) else []
        except Exception:
            aux = []
        header += [var_by[uid]['ids'][0] if uid in var_by else '$M%d.DA?' % mo for mo, uid, _ in aux]
        out = []
        for uid, row in zip(ds['obs_uids'], ds['rows']):
            ids = obs_by[uid]['ids'] if uid in obs_by else [''] * len(obs_ids)
            cells = []
            for c, v in zip(cols, row):
                if v is not None and c.get('categories'):
                    k = int(v)
                    v = c['categories'][k - 1] if 1 <= k <= len(c['categories']) else v
                elif v is not None:
                    v = fmt_float(v)
                cells.append(v)
            out.append(list(ids) + cells + [None if a[2][len(out)] is None else fmt_float(a[2][len(out)])
                                            for a in aux])
        info = {'dataset': ds['name'], 'stream': key, 'missing': ds['missing'], 'log': ds['log'],
                'formulas': {c['ids'][0]: c['formula'] for c in cols if c['formula']}}
        if aux:
            info['log'] += '\n\nLast %d columns: PLS-DA class membership (0/1) stored with this dataset.' % len(aux)
        yield ds['name'] or key, header, out, info, st.get('simaudit.xml', b'')


def models(st):
    """Yield (model number, type, title, report or None, note) for every model in a project."""
    obs_ids, obs = parse_items(st['ncobservations'])
    _, vars_ = parse_items(st['ncvariables'])
    obs_by = {o['uid']: o for o in obs}
    var_by = {v['uid']: v for v in vars_}
    qual = {}  # qualitative variable -> {obs uid: label}, to name PLS-DA classes
    cols = {}  # var uid -> {obs uid: value}, for prediction sets
    predsets = []
    for k in sorted(st):
        if re.fullmatch(r'ncpredset(data)?\d+', k):
            try:
                predsets.append(parse_predset(st[k]))
            except Exception:
                pass
    for k in st:
        if k.startswith('ncdataset'):
            ds = parse_dataset(st[k])
            for j, vu in enumerate(ds['var_uids']):
                cols.setdefault(vu, {}).update((o, row[j]) for o, row in zip(ds['obs_uids'], ds['rows']))
                cats = var_by.get(vu, {}).get('categories')
                if cats:
                    qual.setdefault(var_by[vu]['ids'][0], {}).update(
                        {o: cats[int(row[j]) - 1] for o, row in zip(ds['obs_uids'], ds['rows'])
                         if row[j] is not None and 1 <= row[j] <= len(cats)})
    for key in sorted((k for k in st if re.fullmatch(r'ncmodel\d+', k)), key=lambda k: int(k[7:])):
        n = key[7:]
        title = ''
        try:
            title = parse_modelinfo(st['ncmodelinfo' + n])[1] if 'ncmodelinfo' + n in st else ''
            title = '' if title == 'Untitled' else title.strip()  # the default title
        except Exception:
            pass
        try:
            m = parse_model(st[key])
            if not m['T']:
                yield n, m['type'], title, None, 'not fitted'
                continue
            ws = parse_workset(st['ncwsdata' + n]) if 'ncwsdata' + n in st else []
            pipe = st.get('ncpspipe' + n, b'')
            rep = model_report(m, parse_scaling(pipe), ws, var_by, parse_transforms(pipe), qual)
        except Exception as e:  # one unreadable model should not cost the others
            yield n, '?', title, None, 'could not be read (%s: %s)' % (type(e).__name__, e)
            continue
        rep['obs_header'] = list(obs_ids)
        rep['obs_ids'] = [obs_by[o]['ids'] if o in obs_by else [''] * len(obs_ids) for o in m['obs']]
        rep['predictions'] = []
        used = {vu for t in m['xterms'] for vu in t[0]}
        for ps in predsets:  # a set predicts with every model whose X variables it covers
            if not used <= set(ps['vars']) or not m['A']:
                continue
            try:
                pr = predict(m, parse_scaling(pipe), parse_transforms(pipe), cols, ps['obs'], rep['mpow'], rep['sw0'])
            except Exception:
                pr = None
            if pr:
                inset = set(m['obs'])
                rows = []
                for o, p in zip(ps['obs'], pr):
                    ids = obs_by[o]['ids'] if o in obs_by else [''] * len(obs_ids)
                    ts = p['t'] or [None] * m['A']
                    yobs = [term_value(t, o, cols, parse_transforms(pipe)) for t in m['yterms']]
                    rows.append([_id(v) for v in ids] + ['yes' if o in inset else 'no'] + ts
                                + [p.get('DModX'), p.get('DModX (weighted)'), p.get('T2')] + [v for pair in zip(p.get('y') or [None] * len(yobs), yobs)
                                                                    for v in pair])
                header = (list(obs_ids) + ['In model'] + ['t%d' % (a + 1) for a in range(m['A'])]
                          + ['DModX (normalised)', 'DModX (weighted)', 'T2'] + [h for y in rep['ynames'] for h in ('%s predicted' % y, '%s observed' % y)])
                rep['predictions'].append((ps['name'] or 'prediction set', header, rows))
        yield n, m['type'], title, rep, rep['note']

# ---------------------------------------------- auxiliary data, prediction sets
#
# ncauxiliarydata<N>: columns kept beside dataset N, in practice the PLS-DA class
#   dummies $M<n>.DA<k> (0/1 for every dataset row).
#   v10: u32 count, then per column u32 model, u32 var uid, 8 zero bytes, NCDV.
#   v11: f0 = u32 count, then per column u32 model + a field table: f0 var uid, f5 NCDV.
# ncpredset<N> (v10 NCPredSet): NCDM (a copy of the values), NCIV var uids, NCIV obs
#   uids, NCIV flags, u8, str name.  ncpredsetdata<N> (v16 NCPREDICTIONDS): f1 var uids,
#   f2 obs uids, f6 name. No predictions are stored; they are recomputed from the model.

def parse_auxiliary(buf):
    """[(model number, var uid, values)], one value per row of the matching dataset."""
    r = Reader(buf)
    lv = r.obj('AuxiliaryData')
    out = []
    if lv is None:
        for _ in range(r.u32()):
            model, uid = r.u32(), r.u32()
            r.p += 8
            out.append((model, uid, r.vector('NCDV', True)[0]))
    elif 0 in lv[0]:
        s = r.at(lv[0][0])
        for _ in range(s.u32()):
            model = s.u32()
            f = s.fields()
            out.append((model, s.at(f[0]).u32(), s.at(f[5]).vector('NCDV', True)[0]))
    return [(mo, uid, [None if v == MISSING else v for v in vals]) for mo, uid, vals in out]


def parse_predset(buf):
    """{'name', 'obs': obs uids, 'vars': var uids}."""
    r = Reader(buf)
    q = r.p
    cls = r.classes()[0]
    r.p = q
    lv = r.obj(cls)
    if lv is None:
        r.matrix()
        vars_, obs = r.vector(), r.vector()
        r.vector()
        name = r.str() if r.p < len(buf) and r.u8() else ''
        return {'name': name, 'obs': obs, 'vars': vars_}
    f = lv[0]
    return {'name': r.at(f[6]).str() if 6 in f else '', 'obs': r.at(f[2]).vector(), 'vars': r.at(f[1]).vector()}


def term_value(t, o, cols, transforms):
    """Value of a model term for observation o, transformed as in the model (None if missing)."""
    uids, lags, cats = t
    if lags:
        return None
    v = 1.0
    for i, vu in enumerate(uids):
        x = cols.get(vu, {}).get(o)
        if x is None:
            return None
        if i < len(cats) and cats[i]:
            x = 1.0 if int(x) == cats[i] else 0.0
        v *= x
    if t in transforms:
        typ, prm = transforms[t]
        if typ != 7 or len(prm) != 4:
            return None
        base = prm[1] * v + prm[2]
        if base < 0 and prm[3] != int(prm[3]) or base == 0 and prm[3] < 0:
            return None
        v = base ** prm[3]
    return v


def predict(m, scaling, transforms, cols, obs, w=None, sw0=None):
    """Project observations on a model (missing values skipped):
    t_a = sum(e*w) / sum(w^2) over available variables, then e -= t_a * p_a.
    Returns per observation: scores, fitted Y (data units), DModX (normalised), T2."""
    A, K = m['A'], len(m['xterms'])
    pls = m['type'] == 'PLS'
    sc = compose_scaling(scaling)
    if not all(t in sc for t in m['xterms'] + m['yterms']):
        return None
    N = len(m['obs'])
    s0 = (m['ssx'][A] / ((N - A - 1) * (K - A))) ** 0.5 if K > A and N > A + 1 else None
    scored = [i for i in range(N) if m['T'][0][i] is not None]
    var = [sum(m['T'][a][i] ** 2 for i in scored) / (len(scored) - 1) for a in range(A)]
    out = []
    for o in obs:
        e = []
        for t in m['xterms']:
            v = term_value(t, o, cols, transforms)
            e.append(None if v is None else (v - sc[t][1]) * sc[t][0])
        ts = []
        for a in range(A):
            w = m['W'][a] if pls else m['P'][a]
            den = sum(w[k] ** 2 for k in range(K) if e[k] is not None)
            if not den:
                ts = None
                break
            ta = sum(e[k] * w[k] for k in range(K) if e[k] is not None) / den
            ts.append(ta)
            e = [None if x is None else x - ta * m['P'][a][k] for k, x in enumerate(e)]
        row = {'t': ts}
        if ts is not None:
            ki = sum(x is not None for x in e)
            if s0 and ki > A:
                row['DModX'] = (sum(x * x for x in e if x is not None) / (ki - A)) ** 0.5 / s0
                if w and sw0:
                    row['DModX (weighted)'] = (sum((x * w[k]) ** 2 for k, x in enumerate(e) if x is not None)
                                               / (ki - A)) ** 0.5 / sw0
            row['T2'] = sum(ts[a] ** 2 / var[a] for a in range(A))
            if pls:
                row['y'] = [sc[t][1] + sum(ts[a] * m['C'][a][j] for a in range(A)) / sc[t][0]
                            for j, t in enumerate(m['yterms'])]
        out.append(row)
    return out

# ------------------------------------------------------------ dendrograms
#
# v16 hierarchical clustering: "Dendro_<N>_<d>_<hash>" (N observations of a model,
# clustered on its scores t1..td). "Dendro" v1 / "USB" v1, then the tree in pre-order,
# left child first, 17 bytes per node:
#     internal: u8 0, f32 pos, f32 height, u64 leaves below
#     leaf:     u8 1, f32 pos, u32 1, u64 observation number (1-based, model order)
# Heights are Ward merge costs n_a*n_b/(n_a+n_b)*|c_a - c_b|^2 on the scores, so they add up
# to the total SS of t1..td; that is how a tree is matched to its model. No cut is stored.

def parse_dendro(buf, name=''):
    """{'leaves': N, 'components': d, 'order': leaves left to right, 'merges': [(left, right,
    height, size)], children before parents; leaves are 1..N, merge i is N + 1 + i}."""
    r = Reader(buf)
    if r.classes()[0] != 'Dendro':
        raise ValueError('not a Dendro stream')
    nodes, p = [], r.p  # (is_leaf, height, size, obs)
    while p < len(buf):
        if buf[p] == 0:
            h, n = struct.unpack_from('<fQ', buf, p + 5)
            nodes.append((False, h, n, None))
        elif buf[p] == 1:
            nodes.append((True, 0.0, 1, struct.unpack_from('<Q', buf, p + 9)[0]))
        else:
            raise ValueError('bad dendrogram node at %d' % p)
        p += 17
    N = sum(n[0] for n in nodes)
    merges, order, stack = [], [], []
    for i, (leaf, h, n, obs) in enumerate(nodes):
        if not leaf:
            stack.append([i, []])
            continue
        order.append(obs)
        cid = obs
        while stack:
            stack[-1][1].append(cid)
            if len(stack[-1][1]) < 2:
                break
            j, (a, b) = stack.pop()
            merges.append((a, b, nodes[j][1], nodes[j][2]))
            cid = N + len(merges)
    if stack or len(merges) != N - 1 or sorted(order) != list(range(1, N + 1)):
        raise ValueError('inconsistent dendrogram')
    m = re.match(r'Dendro_(\d+)_(\d+)_', name)
    return {'leaves': N, 'components': int(m.group(2)) if m else None, 'order': order, 'merges': merges}


def cut(dendro, k):
    """Cluster number (1..k, biggest first) per leaf when the tree is cut into k clusters."""
    N, merges = dendro['leaves'], dendro['merges']
    keep = sorted(range(len(merges)), key=lambda i: merges[i][2])[:N - k]  # Ward heights are monotone
    parent = list(range(N + len(merges) + 1))

    def root(x):
        while parent[x] != x:
            parent[x] = parent[parent[x]]
            x = parent[x]
        return x
    for i in sorted(keep):
        a, b = merges[i][:2]
        parent[root(a)] = root(N + 1 + i)
        parent[root(b)] = root(N + 1 + i)
    groups = {}
    for leaf in range(1, N + 1):
        groups.setdefault(root(leaf), []).append(leaf)
    label = {}
    for c, members in enumerate(sorted(groups.values(), key=len, reverse=True), 1):
        for leaf in members:
            label[leaf] = c
    return [label[i] for i in range(1, N + 1)]


def dendrograms(st, items):
    """[(model number, dendrogram)] for the trees that cluster a current model's scores;
    trees left over from models since refitted or deleted match none and are skipped."""
    out = []
    for k in sorted(x for x in st if x.startswith('Dendro_')):
        try:
            d = parse_dendro(st[k], k)
        except ValueError:
            continue
        total = sum(h for _, _, h, _ in d['merges'])
        for n, _, rep in items:
            T = [rep['obs_cols'].get('t%d' % (a + 1)) for a in range(d['components'] or 0)]
            if len(rep['obs_ids']) != d['leaves'] or not T or None in T:
                continue
            ss = 0.0
            for row in T:
                v = [x for x in row if x is not None]
                ss += sum((x - sum(v) / len(v)) ** 2 for x in v)
            if abs(ss - total) <= 1e-4 * total:
                out.append((n, d))
    return out

# ------------------------------------------------------------------- plots

def _cols(rep, prefix):
    return [rep['obs_cols'][k] for k in rep['obs_cols'] if re.fullmatch(prefix + r'\d+', k)]


def plot_model(fig, label, rep):
    """The usual model plots on one page: fit summary, scores (with the Hotelling T2 95%
    ellipse), loadings, DModX and T2 with their limits; for PLS also coefficients with
    jack-knife intervals, VIP and observed vs fitted (first Y)."""
    pls, A, lim = rep['type'] == 'PLS', rep['A'], rep['limits']
    xs = rep['vars']
    X = [v for v in xs if v['Block'] == 'X' and v['Term'] != 'Constant']
    Y = [v for v in xs if v['Block'] == 'Y']
    ids = [str(i[0]) for i in rep['obs_ids']]
    T = _cols(rep, 't')
    n = len(ids)
    axes = fig.subplots(2, 4 if pls else 3).ravel()
    fig.suptitle(label, fontsize=11)
    small = dict(fontsize=6)

    ax = axes[0]  # summary of fit
    comps = rep['components']
    keys = ['R2X(cum)', 'R2Y(cum)', 'Q2(cum)'] if pls else ['R2X(cum)', 'Q2(cum)']
    width = 0.8 / len(keys)
    for i, k in enumerate(keys):
        ax.bar([a + i * width for a in range(len(comps))], [c.get(k) or 0 for c in comps], width, label=k)
    ax.set_xticks([a + 0.4 - width / 2 for a in range(len(comps))], ['Comp %d' % (a + 1) for a in range(len(comps))])
    ax.axhline(0, color='k', lw=0.5)
    ax.legend(**small)
    ax.set_title('Summary of fit', fontsize=9)

    ax = axes[1]  # scores
    cls = rep['obs_cols'].get('Class')
    if A >= 2:
        pts = [(a, b, i) for i, (a, b) in enumerate(zip(T[0], T[1])) if a is not None and b is not None]
        groups = sorted(set(cls)) if cls else [None]
        for gname in groups:
            sel = [(a, b) for a, b, i in pts if cls is None or cls[i] == gname]
            ax.scatter([p[0] for p in sel], [p[1] for p in sel], s=8, label=gname)
        if cls and len(groups) <= 12:
            ax.legend(**small)
        if len(pts) > 3:  # T2 ellipse for the first two components
            Nt = len(pts)
            crit = 2 * (Nt - 1) / (Nt - 2) * f_quantile(0.95, 2, Nt - 2)
            s1 = (sum(p[0] ** 2 for p in pts) / (Nt - 1) * crit) ** 0.5
            s2 = (sum(p[1] ** 2 for p in pts) / (Nt - 1) * crit) ** 0.5
            th = [i * math.pi / 90 for i in range(181)]
            ax.plot([s1 * math.cos(t) for t in th], [s2 * math.sin(t) for t in th], 'k--', lw=0.7)
        if n <= 60:
            for a, b, i in pts:
                ax.annotate(ids[i], (a, b), fontsize=5)
        ax.set_xlabel('t1'); ax.set_ylabel('t2')
    elif A == 1:
        ax.plot(range(1, n + 1), [v if v is not None else float('nan') for v in T[0]], '.', ms=3)
        ax.set_xlabel('observation'); ax.set_ylabel('t1')
    ax.axhline(0, color='grey', lw=0.5); ax.axvline(0, color='grey', lw=0.5)
    ax.set_title('Scores', fontsize=9)

    ax = axes[2]  # loadings
    key = 'w*%d' if pls else 'p%d'
    if A >= 2:
        ax.scatter([v[key % 1] for v in X], [v[key % 2] for v in X], s=10, label='X')
        if pls:
            ax.scatter([v['c1'] for v in Y], [v['c2'] for v in Y], s=14, c='r', label='Y (c)')
        if len(X) + len(Y) <= 60:
            for v in X:
                ax.annotate(v['Term'], (v[key % 1], v[key % 2]), fontsize=5)
            for v in Y if pls else []:
                ax.annotate(v['Term'], (v['c1'], v['c2']), fontsize=5, color='r')
        ax.set_xlabel(key % 1); ax.set_ylabel(key % 2)
        ax.axhline(0, color='grey', lw=0.5); ax.axvline(0, color='grey', lw=0.5)
    elif A == 1:
        ax.bar(range(len(X)), [v[key % 1] for v in X])
        ax.set_xticks(range(len(X)), [v['Term'] for v in X], rotation=90, fontsize=5)
    ax.set_title('Loadings (w*c)' if pls else 'Loadings', fontsize=9)

    ax = axes[3]  # DModX
    dkey = rep.get('dmodx_default', 'DModX')
    d = rep['obs_cols'].get(dkey)
    if d:
        ax.plot(range(1, n + 1), [v if v is not None else float('nan') for v in d], lw=0.6)
        if 'DCrit(0.05)' in lim:
            ax.axhline(lim['DCrit(0.05)'], color='r', ls='--', lw=0.8, label='DCrit(0.05)')
            ax.legend(**small)
    ax.set_xlabel('observation')
    ax.set_title('DModX (normalised%s)' % (', weighted by modelling power' if dkey != 'DModX' else ''), fontsize=9)

    ax = axes[4]  # Hotelling's T2
    t2 = rep['obs_cols'].get('T2')
    if t2:
        ax.plot(range(1, n + 1), [v if v is not None else float('nan') for v in t2], lw=0.6)
        for k, c in (('T2Crit(95%)', 'orange'), ('T2Crit(99%)', 'r')):
            if k in lim:
                ax.axhline(lim[k], color=c, ls='--', lw=0.8, label=k)
        ax.legend(**small)
    ax.set_xlabel('observation'); ax.set_title("Hotelling's T2", fontsize=9)

    if not pls:
        ax = axes[5]  # per-variable fit
        r2 = [v.get('R2V(cum)') or 0 for v in X]
        q2 = [v.get('Q2V(cum)') or 0 for v in X]
        ax.bar([i - 0.2 for i in range(len(X))], r2, 0.4, label='R2VX(cum)')
        ax.bar([i + 0.2 for i in range(len(X))], q2, 0.4, label='Q2VX(cum)')
        ax.set_xticks(range(len(X)), [v['Term'] for v in X], rotation=90, fontsize=5)
        ax.legend(**small)
        ax.set_title('Fit per variable', fontsize=9)
        return
    y0 = rep['ynames'][0] if rep['ynames'] else None
    ax = axes[5]  # coefficients with jack-knife intervals, first Y
    if y0 and A:
        b = [v.get('CoeffCS %s' % y0) or 0 for v in X]
        e = [v.get('CoeffCS %s CI' % y0) for v in X]
        ax.bar(range(len(X)), b, yerr=[x if x is not None else 0 for x in e], capsize=2,
               error_kw=dict(lw=0.6))
        ax.set_xticks(range(len(X)), [v['Term'] for v in X], rotation=90, fontsize=5)
        ax.axhline(0, color='k', lw=0.5)
        ax.set_title('CoeffCS %s (95%% jack-knife)' % y0, fontsize=8)
    ax = axes[6]  # VIP, sorted
    vip = sorted(((v.get('VIP') or 0, v['Term']) for v in X), reverse=True)
    ax.bar(range(len(vip)), [x for x, _ in vip])
    ax.axhline(1, color='r', ls='--', lw=0.6)
    ax.set_xticks(range(len(vip)), [t for _, t in vip], rotation=90, fontsize=5)
    ax.set_title('VIP', fontsize=9)
    ax = axes[7]  # observed vs fitted, first Y
    obs, fit = rep['obs_cols'].get('%s observed' % y0), rep['obs_cols'].get('%s fitted' % y0)
    if obs and fit:
        pts = [(f, o) for o, f in zip(obs, fit) if o is not None and f is not None]
        ax.scatter([p[0] for p in pts], [p[1] for p in pts], s=8)
        lo = min(min(p) for p in pts) if pts else 0
        hi = max(max(p) for p in pts) if pts else 1
        ax.plot([lo, hi], [lo, hi], 'k--', lw=0.6)
        ax.set_xlabel('fitted'); ax.set_ylabel('observed')
        if lim.get('RMSEE'):
            ax.set_title('%s  (RMSEE %.4g)' % (y0, lim['RMSEE'][0]), fontsize=8)


def plot_dendrogram(fig, label, d, ids):
    N = d['leaves']
    x = {leaf: i for i, leaf in enumerate(d['order'])}
    y = {leaf: 0.0 for leaf in x}
    ax = fig.subplots()
    for i, (a, b, h, _) in enumerate(d['merges']):
        ax.plot([x[a], x[a], x[b], x[b]], [y[a], h, h, y[b]], color='k', lw=0.4)
        x[N + 1 + i], y[N + 1 + i] = (x[a] + x[b]) / 2, h
    if N <= 80:
        ax.set_xticks(range(N), [str(ids[leaf - 1][0]) for leaf in d['order']], rotation=90, fontsize=5)
    else:
        ax.set_xticks([])
        ax.set_xlabel('%d observations' % N)
    ax.set_ylabel('Ward merge height')
    ax.set_title(label, fontsize=11)


def write_plots(items, path, base, dendros=()):
    """items: (model number, title, report). One PDF page per model, then the dendrograms."""
    import matplotlib
    matplotlib.use('Agg')
    import matplotlib.pyplot as plt
    from matplotlib.backends.backend_pdf import PdfPages
    with PdfPages(path) as pdf:
        for n, title, rep in items:
            fig = plt.figure(figsize=(16.5, 8.3))
            label = '%s - M%s (%s, %d comp.)%s' % (base, n, rep['type'], rep['A'], ' - ' + title if title else '')
            try:
                plot_model(fig, label, rep)
                fig.tight_layout(rect=(0, 0, 1, 0.96))
            except Exception as e:  # a plot problem should not lose the other pages
                fig.clf()
                fig.text(0.1, 0.5, '%s\nplots failed: %s' % (label, e))
            pdf.savefig(fig)
            plt.close(fig)
        reps = {n: (title, rep) for n, title, rep in items}
        for n, d in dendros:
            title, rep = reps[n]
            fig = plt.figure(figsize=(16.5, 8.3))
            plot_dendrogram(fig, '%s - M%s hierarchical clustering (Ward on t1..t%d)%s'
                            % (base, n, d['components'], ' - ' + title if title else ''), d, rep['obs_ids'])
            fig.tight_layout()
            pdf.savefig(fig)
            plt.close(fig)

# ------------------------------------------------------------------- output


def safe(name):
    return ''.join('_' if ch in '<>:"/\\|?*' else ch for ch in name).strip() or 'dataset'


def _num(v):
    return fmt_float(v) if isinstance(v, float) and v == v else v


def _id(v):
    """IDs are stored as text; let Excel see plain integers (years...) as numbers, keep 01195 as text."""
    return int(v) if isinstance(v, str) and v.isdigit() and (v == '0' or v[0] != '0') else v


def write_models(st, path):
    """One workbook: overview, R2/Q2 per component, then variables and observations per model."""
    import openpyxl
    wb = openpyxl.Workbook()
    ov = wb.active
    ov.title = 'models'
    lim = ['DCrit(0.05)', 'T2Crit(95%)', 'T2Crit(99%)']
    ov.append(['Model', 'Title', 'Type', 'Observations', 'X terms', 'Y', 'Components', 'Computed',
               'R2X(cum)', 'R2Y(cum)', 'Q2(cum)', 'RMSEE'] + lim + ['Note'])
    cp = wb.create_sheet('components')
    keys = ['R2X', 'R2X(cum)', 'Eigenvalue', 'R2Y', 'R2Y(cum)', 'Q2', 'Q2(cum)']
    cp.append(['Model', 'Component'] + keys)
    items = []
    for n, kind, title, rep, note in models(st):
        if rep is None:
            ov.append(['M' + n, title, kind] + [None] * (9 + len(lim)) + [note])
            continue
        items.append((n, title, rep))
        last = rep['components'][-1] if rep['components'] else {}
        rmsee = rep['limits'].get('RMSEE')
        ov.append(['M' + n, title, kind, len(rep['obs_ids']), len(rep['xnames']), ', '.join(rep['ynames']),
                   rep['A'], rep['computed'], last.get('R2X(cum)'), last.get('R2Y(cum)'), last.get('Q2(cum)'),
                   rmsee[0] if rmsee and len(rmsee) == 1 else None]
                  + [rep['limits'].get(k) for k in lim] + [note])
        for a, c in enumerate(rep['components'], 1):
            cp.append(['M' + n, a] + [c.get(k) for k in keys])
        wv = wb.create_sheet('M%s variables' % n)
        wv.append(rep['var_header'])
        for row in rep['var_rows']:
            wv.append([_num(v) for v in row])
        wo = wb.create_sheet('M%s observations' % n)
        wo.append(rep['obs_header'] + list(rep['obs_cols']))
        for i, ids in enumerate(rep['obs_ids']):
            wo.append([_id(v) for v in ids] + [_num(col[i]) for col in rep['obs_cols'].values()])
        for w in (wv, wo):
            w.freeze_panes = 'B2'
        for name, header, rows in rep['predictions']:
            wp = wb.create_sheet(('M%s pred %s' % (n, safe(name)))[:31])
            wp.append(header)
            for row in rows:
                wp.append([_num(v) for v in row])
            wp.freeze_panes = 'B2'
    dendros = dendrograms(st, items)
    for n, d in dendros:
        rep = dict((i[0], i[2]) for i in items)[n]
        N, ids = d['leaves'], rep['obs_ids']
        pos = {leaf: i + 1 for i, leaf in enumerate(d['order'])}
        cuts = {k: cut(d, k) for k in range(2, 7) if k < N}
        wc = wb.create_sheet('M%s clusters t1-%d' % (n, d['components']))
        wc.append(rep['obs_header'] + ['Dendrogram order'] + ['%d clusters' % k for k in cuts])
        for i in range(N):
            wc.append([_id(v) for v in ids[i]] + [pos[i + 1]] + [cuts[k][i] for k in cuts])
        wd = wb.create_sheet('M%s dendrogram t1-%d' % (n, d['components']))
        wd.append(['Step', 'Joins', 'With', 'Height (Ward, t1..t%d)' % d['components'], 'Size'])
        name = lambda x: str(ids[x - 1][0]) if x <= N else 'step %d' % (x - N)
        for i, (a, b, h, size) in enumerate(d['merges'], 1):
            wd.append([i, name(a), name(b), h, size])
        for w in (wc, wd):
            w.freeze_panes = 'A2'
    for w in (ov, cp):
        w.freeze_panes = 'A2'
    wb.save(path)
    return items, dendros


def write_project(path, outdir):
    base = os.path.splitext(os.path.basename(path))[0]
    os.makedirs(outdir, exist_ok=True)
    st = read_streams(path)
    try:
        import openpyxl
        wb = openpyxl.Workbook()
        wb.remove(wb.active)
    except ImportError:
        wb = None
    written, audit = [], b''
    for name, header, rows, info, audit in tables(st):
        csv_path = os.path.join(outdir, '%s - %s.csv' % (base, safe(name)))
        with open(csv_path, 'w', newline='', encoding='utf-8-sig') as f:
            w = csv.writer(f)
            w.writerow(header)
            w.writerows(['' if v is None else v for v in row] for row in rows)
        written.append(csv_path)
        if wb is not None:
            ws = wb.create_sheet(safe(name)[:31])
            ws.append(header)
            for row in rows:
                ws.append([_id(v) for v in row])
            ws.freeze_panes = 'A2'
            wi = wb.create_sheet(('info ' + safe(name))[:31])
            for line in info['log'].replace('\r', '').split('\n'):
                wi.append([line])
            for var, formula in info['formulas'].items():
                wi.append(['formula', var, formula])
        print('  %-30s %d obs x %d vars, %d missing' % (name, len(rows), len(header), info['missing']))
    if wb is not None and written:
        xlsx = os.path.join(outdir, base + '.xlsx')
        wb.save(xlsx)
        written.append(xlsx)
    if audit:
        a = os.path.join(outdir, base + ' - audit.xml')
        open(a, 'wb').write(audit)
        written.append(a)
    for k, v in st.items():  # reports stored in the project (MHTML: open with Word or Edge)
        if k.lower().endswith('.mht') and v:
            rp = os.path.join(outdir, '%s - %s' % (base, k))
            open(rp, 'wb').write(v)
            written.append(rp)
    if wb is not None and any(re.fullmatch(r'ncmodel\d+', k) for k in st):
        mx = os.path.join(outdir, base + ' - models.xlsx')
        items, dendros = write_models(st, mx)
        print('  %d model(s)%s' % (len(items), ', %d dendrogram(s)' % len(dendros) if dendros else ''))
        written.append(mx)
        try:
            import matplotlib  # noqa: F401  (plots are optional)
        except ImportError:
            items = []
        if items:
            pdf = os.path.join(outdir, base + ' - plots.pdf')
            write_plots(items, pdf, base, dendros)
            written.append(pdf)
    return written


def main(argv=None):
    ap = argparse.ArgumentParser(description=__doc__.split('\n')[0])
    ap.add_argument('paths', nargs='+', help='.usp files or folders (searched recursively)')
    ap.add_argument('-o', '--out', default='extracted', help='output folder (default: ./extracted)')
    ap.add_argument('-j', '--jobs', type=int, default=1, help='projects to process in parallel (default: 1)')
    a = ap.parse_args(argv)
    files = []  # (usp path, output folder); folders are mirrored under --out
    for p in a.paths:
        if os.path.isdir(p):
            for root, _, names in os.walk(p):
                rel = os.path.relpath(root, p)
                files += [(os.path.join(root, n), os.path.normpath(os.path.join(a.out, rel)))
                          for n in names if n.lower().endswith('.usp')]
        else:
            files.append((p, a.out))
    failed = 0
    if a.jobs > 1:  # projects are independent: one worker process per project
        from concurrent.futures import ProcessPoolExecutor
        with ProcessPoolExecutor(a.jobs) as pool:
            for log, ok in pool.map(_run_one, sorted(files)):
                print(log, end='', flush=True)
                failed += not ok
    else:
        for job in sorted(files):
            log, ok = _run_one(job)
            print(log, end='', flush=True)
            failed += not ok
    print('%d project(s), %d failed' % (len(files), failed))
    return 1 if failed else 0


def _run_one(job):
    """Extract one project, returning (its log text, success)."""
    import contextlib, io
    f, outdir = job
    buf = io.StringIO()
    ok = True
    with contextlib.redirect_stdout(buf):
        print(f)
        try:
            write_project(f, outdir)
        except Exception as e:  # keep going through a folder full of projects
            ok = False
            print('  FAILED: %s: %s' % (type(e).__name__, e))
    return buf.getvalue(), ok


if __name__ == '__main__':
    sys.exit(main())
