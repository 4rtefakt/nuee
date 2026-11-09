# Deploying Nuée in an organisation

For IT departments installing Nuée on several Windows computers: how to install it silently,
turn off its update check, what it writes where, and what it sends over the network.

## What gets installed

Nuée is a desktop application (Electron) with its own R (4.6.2) and the R packages it needs, in
one folder. An R already installed on the computer is neither used nor changed. Windows 10 or 11,
64-bit. A per-user installation needs no administrator rights.

## Installing

### Silent installation

The installer, `Nuee-Setup-<version>.exe`, comes from the
[Releases](https://github.com/4rtefakt/nuee/releases) page.

| | Command | Installs in |
|---|---|---|
| For the user running it | `Nuee-Setup-1.0.0.exe /S /currentuser` | `%LOCALAPPDATA%\Programs\Nuée` |
| For every user of the computer (administrator, e.g. run as SYSTEM by SCCM or Intune) | `Nuee-Setup-1.0.0.exe /S /allusers` | `%ProgramFiles%\Nuée` |

`/D=D:\Apps\Nuée` (last, without quotes, even with spaces) installs elsewhere. The installer adds
Start menu and desktop shortcuts, an entry in *Settings › Apps*, and opens `.nuee` projects in
Nuée (ProgID `Nuee.Projet`). To uninstall silently: `"<install folder>\Uninstall Nuée.exe" /S`.
Uninstalling keeps the users' projects and settings.

### From a network share

The build also makes a `Nuee` folder that runs without installation: put it on a share and have
users start `Lancer Nuee.cmd`. Windows can't run the application from a share, so the launcher
copies it to `%LOCALAPPDATA%\Nuee` (the first time in full, then only what changed on the share),
adds shortcuts and an entry in *Settings › Apps*, and starts it. Updating the share updates
everyone at their next start. This copy never checks for new versions.

## Updates

An installed copy asks GitHub at start-up whether a newer version exists: one HTTPS request to
`api.github.com/repos/4rtefakt/nuee/releases/latest`. If there is one, a click in the status bar
downloads the new installer from `github.com/4rtefakt/nuee/releases`, checks its SHA-256 against
the one GitHub gives, and reinstalls silently in the same mode and folder. A per-computer
installation asks for administrator rights to do so. Users can turn the check off in *Help*.

To turn it off for everyone (when you deploy updates yourself), set this policy value; the
update items then disappear from *Help*:

```
reg add "HKLM\SOFTWARE\Policies\Nuee" /v DisableUpdateCheck /t REG_DWORD /d 1 /f
```

Any tool that writes registry values can set it (Group Policy Preferences, Intune). The same
value under `HKCU\SOFTWARE\Policies\Nuee` applies to one user. When both exist, the computer's
value wins.

## Network and privacy

- Nuée sends nothing anywhere except the update check above, when it is on. There is no
  telemetry, no usage statistics, no crash report, and no account or licence server.
- The update check goes through the system's proxy settings, as Chrome does.
- Data stay on the computer. Projects are files saved wherever users choose.
- The calculations run in a local R process. It serves the interface on `127.0.0.1` only (port
  47863, or another free one), so no firewall rule is needed. It answers only Nuée's own windows:
  each request must carry a random token drawn at each start, in a cookie that no other site can
  send, and name 127.0.0.1 as its host. Web pages open in a browser can't use it, and neither can
  other users of a shared computer (Remote Desktop).

## What Nuée writes where

| What | Where |
|---|---|
| The program | `%LOCALAPPDATA%\Programs\Nuée` or `%ProgramFiles%\Nuée` (network-share copy: `%LOCALAPPDATA%\Nuee`) |
| Settings, recent files, interface preferences | `%APPDATA%\Nuee` |
| Recovery copies of projects with unsaved changes, every two minutes | `%APPDATA%\Nuee\secours`, deleted when Nuée closes normally |
| R's temporary files (pasted tables, converted `.usp` projects, console plots) | `%TEMP%\nuee-r-<process>`, deleted when Nuée closes, or at the next start after a crash |
| The installer of a one-click update | `%TEMP%\nuee-mise-a-jour`, deleted at the next start |
| Projects | Wherever users save them (`.nuee`) |

## Projects on network folders

- A save never leaves a half-written project. The file is written next to the old one, then
  takes its place in one step, so a network cut or a full disk leaves the old file intact.
- Projects aren't locked: two people can open the same one. If the file changed on disk since it
  was opened or last saved (a colleague, another window), Nuée asks before overwriting it and
  offers *Save as…* instead.

## Shared computers (Remote Desktop, Citrix)

Each user session runs its own R engine, on its own port, with its own token. Nothing is shared
between sessions.

## R and the users' own R

An installed copy ignores the user's `.Rprofile` and loads the packages that come with Nuée
first. This means a personal R setup can't change the engine's results. Packages from the user's
own R library (same R version) can still be loaded in Nuée's R console, for example
`library(vegan)`.

## Opening files from elsewhere

Opening a file never runs code from it:

- **Projects (`.nuee`):** data and model definitions only. Nothing in them can choose where Nuée
  saves.
- **R files (`.rds`, `.RData`):** must hold tables; anything else is refused.
- **Spectra and text tables:** read with checks on their size and structure.

R code runs only when a user types it: a formula in *Edit › New variable*, or the R console. The
windows run with Electron's sandbox and context isolation and a content security policy, and
can't be navigated to another site.

## Code signing

The installer isn't code-signed yet, so SmartScreen warns the first time it runs. Where AppLocker
or WDAC applies, allow the installer and the installed folder by hash or by path.

## Reporting a problem

*Help › Diagnostic information* gives the versions of Nuée, Electron, R and its packages, to
paste into a report. It includes no paths and nothing personal.
