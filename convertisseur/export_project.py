"""Export .usp projects to JSON for the R engine.

One file per project holds the data, each model's definition (observations, X and Y terms
with their transform and scaling), the results stored for it, and the statistics
usp_extract derives from them. The R engine refits the models from the definitions and
checks itself against the stored results.

    python export_project.py <.usp files or folders> -o <folder> [-j jobs]
"""
import argparse, json, math, os, re, sys
import xml.etree.ElementTree as ET
import usp_extract as u

FORMAT = 'usp-json/1'


def num(v):
    if v is None or v == u.MISSING:
        return None
    v = float(v)
    return v if math.isfinite(v) else None


def clean(x):
    """JSON-safe copy: tuples to lists, NaN/inf/missing marker to null."""
    if isinstance(x, dict):
        return {str(k): clean(v) for k, v in x.items()}
    if isinstance(x, (list, tuple)):
        return [clean(v) for v in x]
    if isinstance(x, float):
        return num(x)
    if isinstance(x, complex):  # a square root of a negative number in a derived statistic
        return None
    return x


# ------------------------------------------------------------ workset specs
#
# ncwsspec<N> (v11+) WorksetSpec: f3 X BlockSpec, f4 Y BlockSpec.
# BlockSpec f0: u32 count, then (presence byte, MT); MT f0 = NCIV of the term's variable uids.
# BlockSpec f1: u32 count, then (u32 MT index, SS); SS f0 = scaling code (0 centring,
# 1 unit variance), f1 = 2 in every file seen, f2 = modifier (multiplies the weight),
# f3 = user weight (-99 = none), f4 = 1 for block scaling, f5 = the term's block. A block-scaled
# term's weight is also divided by sqrt(K), K the number of block-scaled terms of its block in
# the model: checked on the 2530 terms of a project of 132 models.
# v10 files write the spec without field tables; the scaling is then inferred.

def _end(f):
    return max(e for level in f for (_, e) in level.values())


def _scaling_specs(r, fld):
    """{variable uids: SS fields} for one BlockSpec: 'code', 'f1', 'modifier', 'user',
    'block' (the block's number for a block-scaled term, else None)."""
    g = r.at(fld).obj('BlockSpec')[0]
    if 0 not in g or 1 not in g:
        return {}
    t = r.at(g[0])
    mts = []
    for _ in range(t.u32()):
        if not t.u8():
            mts.append(None)
            continue
        f = t.obj('MT')
        mts.append(tuple(t.at(f[0][0]).vector()))
        t.p = _end(f)
    t = r.at(g[1])
    specs = {}
    for _ in range(t.u32()):
        i = t.u32()
        f = t.obj('SS')
        if i < len(mts) and mts[i] is not None and 0 in f[0]:
            fl = f[0]
            num = lambda k: t.at(fl[k])._un('<f') if k in fl and fl[k][1] - fl[k][0] == 4 else None
            ent = lambda k, w: int.from_bytes(r.b[fl[k][0]:fl[k][1]], 'little') if k in fl and fl[k][1] - fl[k][0] == w else None
            specs[mts[i]] = {'code': t.at(fl[0]).u16(), 'f1': ent(1, 2), 'modifier': num(2), 'user': num(3),
                             'block': ent(5, 4) if ent(4, 2) == 1 else None}
        t.p = _end(f)
    return specs


def parse_wsspec(buf):
    """(X specs, Y specs) as {variable uids: SS fields}; empty for v10."""
    if not buf:
        return {}, {}
    r = u.Reader(buf, pfx=b'zA' in buf[:64])
    lv = r.obj('WorksetSpec')
    if lv is None:
        return {}, {}
    out = []
    for fid in (3, 4):
        try:
            out.append(_scaling_specs(r, lv[0][fid]) if fid in lv[0] else {})
        except Exception:
            out.append({})
    return out[0], out[1]


SCALING_CODES = {0: 'Ctr', 1: 'UV'}


def weighting(spec, sizes):
    """(modifier, block) of a term from its SS fields: what multiplies the weight of its own
    scaling. None for a setting not seen in a checked file (user weight, f1 other than 2 with
    block scaling): the stored weight is then kept as it is."""
    if not spec:
        return 1.0, None
    if spec['user'] is not None and spec['user'] != -99:
        return None
    if spec['block'] is not None and (spec['f1'] != 2 or not sizes.get(spec['block'])):
        return None
    mod = spec['modifier'] if spec['modifier'] is not None else 1.0
    return mod, spec['block']


def infer_scaling(values, weight, centre):
    """Which scaling reproduces the stored weight and centre on these values (the term's
    values over the model's observations, transformed): UV, Par, Ctr, none, or None."""
    v = [x for x in values if x is not None]
    if len(v) < 2 or weight is None or centre is None:
        return None
    mu = sum(v) / len(v)
    sd = (sum((x - mu) ** 2 for x in v) / (len(v) - 1)) ** 0.5
    close = lambda a, b: abs(a - b) <= 1e-4 * max(abs(a), abs(b), 1e-12)
    for name, (c, w) in (('UV', (mu, 1 / sd if sd else 0.0)), ('Par', (mu, sd ** -0.5 if sd else 0.0)),
                         ('Ctr', (mu, 1.0)), ('none', (0.0, 1.0))):
        if close(centre, c) and close(weight, w):
            return name
    return None


# ------------------------------------------------------------------ project

def export(path):
    st = u.read_streams(path)
    obs_names, obs = u.parse_items(st['ncobservations'])
    var_names, vars_ = u.parse_items(st['ncvariables'])
    var_by = {v['uid']: v for v in vars_}
    cols, qual, datasets = {}, {}, []
    for key in sorted((k for k in st if k.startswith('ncdataset')), key=lambda k: int(k[9:] or 0)):
        ds = u.parse_dataset(st[key])
        aux = []
        if st.get('ncauxiliarydata' + key[9:]):
            try:  # PLS-DA class dummies kept beside the dataset
                aux = [a for a in u.parse_auxiliary(st['ncauxiliarydata' + key[9:]]) if len(a[2]) == len(ds['rows'])]
            except Exception:
                aux = []
        var_uids = list(ds['var_uids']) + [a[1] for a in aux]
        rows = [list(row) + [a[2][i] for a in aux] for i, row in enumerate(ds['rows'])]
        for j, vu in enumerate(var_uids):
            cols.setdefault(vu, {}).update((o, row[j]) for o, row in zip(ds['obs_uids'], rows))
            cats = var_by.get(vu, {}).get('categories')
            if cats:
                qual.setdefault(var_by[vu]['ids'][0], {}).update(
                    {o: cats[int(row[j]) - 1] for o, row in zip(ds['obs_uids'], rows)
                     if row[j] is not None and 1 <= row[j] <= len(cats)})
        datasets.append({'name': ds['name'] or key, 'stream': key, 'observations': ds['obs_uids'],
                         'variables': var_uids, 'values': rows,
                         'auxiliary': [{'model': a[0], 'variable': a[1]} for a in aux]})
    for a in {a['variable'] for d in datasets for a in d['auxiliary']} - set(var_by):
        vars_.append({'class': 'auxiliary', 'ids': ['$aux%d' % a], 'uid': a, 'formula': ''})

    models = []
    obs_by = {o['uid']: o for o in obs}
    for key in sorted((k for k in st if re.fullmatch(r'ncmodel\d+', k)), key=lambda k: int(k[7:])):
        n = key[7:]
        try:
            models.append(export_model(st, n, cols, var_by, qual, (obs_by, obs_names, datasets)))
        except Exception as e:  # one unreadable model should not cost the others
            models.append({'number': int(n), 'error': '%s: %s' % (type(e).__name__, e)})

    predsets = []
    for k in sorted(st):
        if re.fullmatch(r'ncpredset(data)?\d+', k):
            try:
                ps = u.parse_predset(st[k])
                predsets.append({'name': ps['name'], 'observations': ps['obs'], 'variables': ps['vars']})
            except Exception:
                pass

    return clean({
        'format': FORMAT, 'source': path,
        'observation_id_names': obs_names, 'variable_id_names': var_names,
        'observations': [{'uid': o['uid'], 'ids': o['ids']} for o in obs],
        'variables': [{'uid': v['uid'], 'ids': v['ids'], 'categories': v.get('categories'),
                       'formula': v.get('formula') or ''} for v in vars_],
        'datasets': datasets, 'models': models, 'prediction_sets': predsets,
        'dendrograms': dendrograms(st, models), 'audit': audit(st),
    })


def partition(labels):
    """The groups of a labelling as lists of positions, whatever the groups' numbers."""
    groups = {}
    for i, label in enumerate(labels):
        groups.setdefault(label, []).append(i)
    return sorted(groups.values())


def cross_validation(m, cvg, obs_by, obs_names, datasets):
    """The model's cross-validation groups, from the group of each observation stored with it
    (v11+; for v10 files, the writing program's default of 7 groups, with which their stored Q2
    reproduce):
    - observations in turn: {'cv_groups': G}, the engine's default;
    - grouped by an observation ID (same value, same group): {'cv': {'by': 'id', 'id': name}},
      when that ID's values over the model's observations give exactly the stored groups;
    - otherwise the stored groups themselves: {'cv': {'by': 'groups', 'observations', 'groups'}}."""
    g = m.get('cvgroups')
    N = len(m['obs'])
    labels = [x + 1 for x in g] if g and len(g) == N and m['T'] else []
    G = len(set(labels))
    if G < 2:  # not fitted yet, or fitted without cross-validation (no Q2 stored): the default
        return {'cv_groups': 7}
    stored = partition(labels)
    for n in ((cvg or {}).get('requested'), G):  # in turn: the number asked for (more than N is N groups)
        if n and n >= 2 and stored == partition([i % n for i in range(N)]):
            return {'cv_groups': n}
    out = {'cv_groups': G}
    if cvg and cvg['method'] == 3:
        ids = [obs_by[o]['ids'] if o in obs_by else [] for o in m['obs']]
        names = [str(x) for x in obs_names]
        column = lambda j: [i[j] if j < len(i) else '' for i in ids]
        # an ID the engine finds by its name: the first of that name, not internal ($ClassID...)
        usable = [j for j in range(len(names)) if (j == 0 or (names[j] and not names[j].startswith('$')))
                  and names.index(names[j]) == j]
        found = [j for j in usable if partition(column(j)) == stored]
        if found:
            # two IDs can group alike (a site's name and its code): the setting names one of
            # them by its place among the IDs of the model's dataset, the 1st and 2nd included
            # (verified on 128 models)
            pick = found[0]
            ds = next((d for d in datasets if set(m['obs']) <= set(d['observations'])), None)
            if ds and cvg['id'] is not None and cvg['id'] >= 0:
                rows = [obs_by[o]['ids'] for o in ds['observations'] if o in obs_by]
                order = [0, 1] + [j for j in range(2, len(names)) if not names[j].startswith('$')
                                  and any(j < len(i) and i[j] for i in rows)]
                if cvg['id'] < len(order) and order[cvg['id']] in found:
                    pick = order[cvg['id']]
            out['cv'] = {'by': 'id', 'id': names[pick]}
            return out
    out['cv'] = {'by': 'groups', 'observations': list(m['obs']), 'groups': labels}
    return out


def export_model(st, n, cols, var_by, qual, observations):
    obs_by, obs_names, datasets = observations
    m = u.parse_model(st['ncmodel' + n])
    name, title = ('M' + n, '')
    if 'ncmodelinfo' + n in st:
        try:
            name, title = u.parse_modelinfo(st['ncmodelinfo' + n])
        except Exception:
            pass
    title = '' if title == 'Untitled' else title.strip()
    pipe = st.get('ncpspipe' + n, b'')
    scaling = u.parse_scaling(pipe)
    weights = u.compose_scaling(scaling)
    transforms = u.parse_transforms(pipe)
    codes = parse_wsspec(st.get('ncwsspec' + n))

    single = {}  # (variable uid, category) -> weights of the plain term, for cross terms
    for t, w in weights.items():
        if len(t[0]) == 1:
            single[t[0][0], t[2][0] if t[2] else 0] = (t, w)

    def values_of(t):
        """The term over the model's observations. A cross term is the product of its factors
        already centred and scaled (terms are expanded from the scaled variables)."""
        uids, lags, cats = t
        if len(uids) == 1:
            return [u.term_value(t, o, cols, transforms) for o in m['obs']]
        prod = [1.0] * len(m['obs'])
        for j, vu in enumerate(uids):
            ft, (fsc, fce) = single.get((vu, cats[j] if j < len(cats) else 0), (None, (None, None)))
            if ft is None:
                return [None] * len(m['obs'])
            for i, o in enumerate(m['obs']):
                v = u.term_value(ft, o, cols, transforms)
                prod[i] = None if v is None or prod[i] is None else prod[i] * (v - fce) * fsc
        return prod

    def terms(ts, specs):
        """The block's terms. Their scaling: its type (UV, Par, Ctr, none: what reproduces the
        stored centre and weight, the modifier and block factor taken out; else the stored code),
        the modifier and the block of block scaling (weight times 1/sqrt(K), K the number of
        terms of the block), and the stored centre and weight (all of it)."""
        sizes = {}
        for t in ts:
            b = (specs.get(tuple(t[0])) or {}).get('block')
            if b is not None:
                sizes[b] = sizes.get(b, 0) + 1
        out = []
        for t in ts:
            uids, lags, cats = t
            sc, ce = weights.get(t, (None, None))
            values = values_of(t)
            spec = specs.get(tuple(uids))
            code = spec['code'] if spec else None
            w = weighting(spec, sizes)
            scaling = {'code': code, 'weight': sc, 'centre': ce}
            if w is None:  # a setting the engine doesn't redo: the stored weight is kept
                scaling['type'] = None
            else:
                mod, block = w
                extra = mod / sizes[block] ** 0.5 if block is not None else mod
                kind = infer_scaling(values, sc / extra if sc is not None and extra else None, ce)
                scaling['type'] = kind or SCALING_CODES.get(code)
                if mod != 1:
                    scaling['modifier'] = mod
                if block is not None:
                    scaling['block'] = block
            tr = transforms.get(t)
            label = u.term_name(t, var_by)
            out.append({'variables': list(uids), 'categories': list(cats), 'lags': list(lags),
                        'name': u.transform_label(label, tr) if tr else label,
                        'transform': {'type': tr[0], 'params': list(tr[1])} if tr else None,
                        'scaling': scaling})
        return out

    out = {'number': int(n), 'name': name or 'M' + n, 'title': title, 'type': m['type'],
           'version_origine': m['version'], 'A': m['A'], 'fitted': bool(m['T']),
           'observations': m['obs'], 'x': terms(m['xterms'], codes[0]), 'y': terms(m['yterms'], codes[1])}
    try:
        cvg = u.parse_cvgroups(st.get('ncCVgroups' + n))
    except Exception:  # the setting only names the groups; the stored groups are what counts
        cvg = None
    out.update(cross_validation(m, cvg, obs_by, obs_names, datasets))
    out['plsda'] = bool(out['y']) and all(re.fullmatch(r'\$M\d+\.DA\d+', t['name']) for t in out['y'])
    solution = {k: m.get(k) for k in ('computed', 'ssx', 'ssxv', 'ssy', 'ssyv', 'q2', 'q2cum', 'eig',
                                   'P', 'T', 'W', 'C', 'U', 'dmodx', 'q2v', 'q2vcum', 'cvP', 'cvW', 'cvC')}
    ws = u.parse_workset(st['ncwsdata' + n]) if 'ncwsdata' + n in st else []
    solution['workset_x'] = ws[0] if ws else None
    solution['workset_y'] = ws[1] if len(ws) > 1 else None
    out['solution'] = solution
    if m['T']:
        try:
            rep = u.model_report(m, scaling, ws, var_by, transforms, qual)
            out['reference'] = {k: rep[k] for k in ('A', 'components', 'xnames', 'ynames', 'vars', 'obs_cols',
                                                    'limits', 'note', 'dmodx_default', 'mpow', 'sw0')}
        except Exception as e:
            out['reference'] = {'error': '%s: %s' % (type(e).__name__, e)}
    return out


def audit(st):
    """The project's audit trail (simaudit.xml): each working session (date, application, user,
    computer) with its events (date, text, details)."""
    data = st.get('simaudit.xml')
    if not data:
        return []
    try:
        root = ET.fromstring(data)
    except ET.ParseError:  # bytes that are not the declared UTF-8
        try:
            root = ET.fromstring(re.sub(r'^<\?xml[^>]*\?>', '', data.decode('latin-1')))
        except ET.ParseError:
            return []
    sessions = []
    for s in root.iter('Session'):
        user = s.find('User')
        sessions.append({
            'date': s.get('Date', ''), 'application': s.get('Application', ''), 'project': s.get('Project', ''),
            'user': user.get('User', '') if user is not None else '',
            'computer': user.get('Computer', '') if user is not None else '',
            'events': [{'date': e.get('Date', ''), 'text': (e.text or '').strip(), 'details': (e.findtext('Details') or '').strip()}
                       for e in s.iter('Event')]})
    return sessions


def dendrograms(st, models):
    """Stored HCA trees, each tied to the model whose scores it clusters (the merge heights of
    a Ward tree add up to the scores' total sum of squares); leftover trees are skipped."""
    out = []
    for k in sorted(x for x in st if x.startswith('Dendro_')):
        try:
            d = u.parse_dendro(st[k], k)
        except ValueError:
            continue
        total = sum(h for _, _, h, _ in d['merges'])
        for mo in models:
            T = (mo.get('solution') or {}).get('T') or []
            c = d['components'] or 0
            if not c or len(T) < c or len(mo['observations']) != d['leaves'] or any(None in r for r in T[:c]):
                continue
            ss = sum(sum((x - sum(r) / len(r)) ** 2 for x in r) for r in T[:c])
            if abs(ss - total) <= 1e-4 * total:
                out.append({'stream': k, 'model': mo['number'], 'components': c, 'leaves': d['leaves'],
                            'order': d['order'], 'merges': d['merges']})
    return out


def _run_one(job):
    src, dst = job
    try:
        data = export(src)
        os.makedirs(os.path.dirname(dst) or '.', exist_ok=True)
        with open(dst, 'w', encoding='utf-8') as f:
            json.dump(data, f, ensure_ascii=False, separators=(',', ':'))
        bad = sum('error' in mo for mo in data['models'])
        return '%s: %d model(s)%s\n' % (src, len(data['models']), ', %d unreadable' % bad if bad else ''), True
    except Exception as e:
        return '%s: FAILED %s: %s\n' % (src, type(e).__name__, e), False


def main(argv=None):
    ap = argparse.ArgumentParser(description=__doc__.split('\n')[0])
    ap.add_argument('paths', nargs='+', help='.usp files or folders (searched recursively)')
    ap.add_argument('-o', '--out', default='exported', help='output folder (default: ./exported)')
    ap.add_argument('-j', '--jobs', type=int, default=1, help='projects to process in parallel')
    a = ap.parse_args(argv)
    jobs = []
    for p in a.paths:
        if os.path.isdir(p):
            for root, _, names in os.walk(p):
                rel = os.path.relpath(root, p)
                jobs += [(os.path.join(root, f), os.path.normpath(os.path.join(a.out, rel, f[:-4] + '.json')))
                         for f in names if f.lower().endswith('.usp')]
        else:
            jobs.append((p, os.path.join(a.out, os.path.splitext(os.path.basename(p))[0] + '.json')))
    failed = 0
    if a.jobs > 1:
        from concurrent.futures import ProcessPoolExecutor
        with ProcessPoolExecutor(a.jobs) as pool:
            results = list(pool.map(_run_one, sorted(jobs)))
    else:
        results = [_run_one(j) for j in sorted(jobs)]
    for log, ok in results:
        print(log, end='')
        failed += not ok
    print('%d project(s), %d failed' % (len(jobs), failed))
    return 1 if failed else 0


if __name__ == '__main__':
    sys.exit(main())
