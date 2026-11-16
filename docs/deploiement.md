# Deploying Nuée in an organisation

For IT departments installing Nuée on several Windows computers: how to install it silently,
the settings for Intune, Configuration Manager, Group Policy and other deployment tools, how to
turn off its update check, what it writes where (the versions it keeps beside projects included),
what it sends over the network and through which proxy, and how to let users sign in to your own
GitLab. Macs and Linux computers: see [macOS](#macos) and [Linux](#linux) at the end.

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
| For the user running it | `Nuee-Setup-1.1.0.exe /S /currentuser` | `%LOCALAPPDATA%\Programs\Nuee` |
| For every user of the computer (administrator, e.g. run as SYSTEM by Configuration Manager or Intune) | `Nuee-Setup-1.1.0.exe /S /allusers` | `%ProgramFiles%\Nuee` |

`/D=D:\Apps\Nuée` (last, without quotes, even with spaces) installs elsewhere. The program is
`Nuee.exe` in that folder. The installer adds Start menu and desktop shortcuts, an entry in
*Settings › Apps*, and opens `.nuee` projects in Nuée (ProgID `Nuee.Projet`). To uninstall
silently: `"<install folder>\Uninstall Nuee.exe" /allusers /S` (or `/currentuser /S`), the
command the entry in *Settings › Apps* also gives (`QuietUninstallString`). Uninstalling keeps
the users' projects and settings. The installer and the uninstaller exit with 0 when they
succeed; no restart is needed.

Installed over Nuée's folder, the installer keeps the shortcuts of the version it replaces, and
with them Nuée pinned to the users' taskbars (installed elsewhere, it makes new ones). Before
1.0, it removed them, which unpinned Nuée, unless given `--updated`.

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
the one GitHub gives, and reinstalls silently in the same mode and folder (all users or this
user, as Nuée's entry in *Settings › Apps* says, wherever its folder is). A per-computer
installation asks for administrator rights to do so. Users can turn the check off in *Help*.

To turn it off for everyone (when you deploy updates yourself), set this policy value; the
update items then disappear from *Help*:

```
reg add "HKLM\SOFTWARE\Policies\Nuee" /v DisableUpdateCheck /t REG_DWORD /d 1 /f /reg:64
```

Any tool that writes registry values can set it: see [Deployment tools](#deployment-tools) for
Intune and Group Policy. Nuée reads the registry's 64-bit view, hence `/reg:64` for a tool that
runs as a 32-bit process. The same value under `HKCU\SOFTWARE\Policies\Nuee` applies to one user.
When both exist, the computer's value wins.

## Deployment tools

The usual choice with these tools is an installation for every user of the computer, run as
SYSTEM. Its values, for version 1.1.0 (put the version you deploy in the file name and the
detection rule):

| | |
|---|---|
| Install command | `Nuee-Setup-1.1.0.exe /S /allusers` |
| Uninstall command | `"%ProgramFiles%\Nuee\Uninstall Nuee.exe" /allusers /S` |
| Exit codes | 0 when it worked; no restart needed |
| Detection | Registry, 64-bit view: key `HKEY_LOCAL_MACHINE\SOFTWARE\Microsoft\Windows\CurrentVersion\Uninstall\3dabb207-3e0d-5aa0-bdfc-da17bd7f0503`, value `DisplayVersion`, a version greater than or equal to `1.1.0` |
| Requirements | Windows 10 or 11, 64-bit (x64); about 550 MB of disk |
| Updating | Run the new version's installer with the same command: it replaces the installed copy in place, and keeps its shortcuts and taskbar pins |

The key's name is Nuée's own and the same for every version (it comes from the application's
ID, `io.github.4rtefakt.nuee`). An installation per user is the same with `/currentuser`, the
folder `%LOCALAPPDATA%\Programs\Nuee` and the key under `HKEY_CURRENT_USER`, run in the user's
context.

### Microsoft Intune

As a *Windows app (Win32)*:

1. **Package** the installer with the
   [Microsoft Win32 Content Prep Tool](https://github.com/microsoft/Microsoft-Win32-Content-Prep-Tool):
   put `Nuee-Setup-1.1.0.exe` alone in a folder, then
   `IntuneWinAppUtil.exe -c <that folder> -s Nuee-Setup-1.1.0.exe -o <output folder>`, and upload
   the `.intunewin` file it writes.
2. **Program:** install command `Nuee-Setup-1.1.0.exe /S /allusers`; uninstall command
   `"%ProgramFiles%\Nuee\Uninstall Nuee.exe" /allusers /S`; install behavior *System*; device
   restart behavior *No specific action*; return codes as they are.
3. **Requirements:** operating system architecture *x64*; minimum operating system, the oldest
   Windows 10 offered.
4. **Detection rules:** *Manually configure detection rules*, rule type *Registry*: key path
   `HKEY_LOCAL_MACHINE\SOFTWARE\Microsoft\Windows\CurrentVersion\Uninstall\3dabb207-3e0d-5aa0-bdfc-da17bd7f0503`,
   value name `DisplayVersion`, detection method *Version comparison*, operator *Greater than or
   equal to*, value `1.1.0`, associated with a 32-bit app on 64-bit clients *No*.
5. **A new version:** add it as a new app, then in its *Supersedence*, add the previous one with
   *Uninstall previous version* set to *No*: its installer updates the installed copy.
6. **Turning the update check off:** a platform script (*Devices › Scripts and remediations ›
   Platform scripts*), with *Run this script using the logged on credentials* set to *No* and
   *Run script in 64 bit PowerShell Host* set to *Yes*:

   ```powershell
   New-Item -Path 'HKLM:\SOFTWARE\Policies\Nuee' -Force | Out-Null
   New-ItemProperty -Path 'HKLM:\SOFTWARE\Policies\Nuee' -Name DisableUpdateCheck -PropertyType DWord -Value 1 -Force | Out-Null
   ```

### Configuration Manager (SCCM, MECM)

An application with a *Script Installer* deployment type:

- **Programs:** installation program `Nuee-Setup-1.1.0.exe /S /allusers`; uninstall program
  `"%ProgramFiles%\Nuee\Uninstall Nuee.exe" /allusers /S`.
- **Detection method:** setting type *Registry*, hive `HKEY_LOCAL_MACHINE`, key
  `SOFTWARE\Microsoft\Windows\CurrentVersion\Uninstall\3dabb207-3e0d-5aa0-bdfc-da17bd7f0503`,
  value `DisplayVersion`, data type *Version*, the rule *Greater than or equals* `1.1.0`; leave
  *This registry key is associated with a 32-bit application on 64-bit systems* unticked.
- **User experience:** installation behavior *Install for system*; logon requirement *Whether or
  not a user is logged on*; installation program visibility *Hidden*.
- **A new version** supersedes the previous one without uninstalling it (*Uninstall* unticked).
- **The update check:** the Group Policy preference below, or a configuration item that sets
  `DisableUpdateCheck`.

### Group Policy

Group Policy's *Software installation* only takes MSI packages, and Nuée's installer is a
program, so install it from a computer startup script (*Computer Configuration › Policies ›
Windows Settings › Scripts › Startup*), with the installer on a share the computers can read
(*Domain Computers*). This one installs 1.1.0 unless that version is already there; change both
version numbers to deploy another:

```bat
@echo off
reg query "HKLM\SOFTWARE\Microsoft\Windows\CurrentVersion\Uninstall\3dabb207-3e0d-5aa0-bdfc-da17bd7f0503" /v DisplayVersion /reg:64 2>nul | findstr /e /c:" 1.1.0" >nul
if errorlevel 1 "\\server\share\Nuee-Setup-1.1.0.exe" /S /allusers
```

To turn the update check off: *Computer Configuration › Preferences › Windows Settings ›
Registry*, a new registry item with action *Update*, hive `HKEY_LOCAL_MACHINE`, key path
`SOFTWARE\Policies\Nuee`, value name `DisableUpdateCheck`, type `REG_DWORD`, value `1`.

### PDQ Deploy and other tools

- **PDQ Deploy:** an *Install* step with `Nuee-Setup-1.1.0.exe`, parameters `/S /allusers`,
  success code `0`, run as *Local System* or as a deploy user who is an administrator.
- **Other tools** (ManageEngine, Ivanti, NinjaOne, Chocolatey for Business, a private WinGet
  source…): the installer is an NSIS one (*nullsoft* for WinGet); the commands, exit codes and
  detection rule above apply as they are.

## Network and privacy

- Nuée sends nothing anywhere except the update check above, when it is on; the map tiles of a
  background map, when a user chooses one (OpenStreetMap, IGN); and, for a project a user shares
  on GitHub or GitLab (from 1.1), its versions, to that repository. There is no telemetry, no
  usage statistics, no crash report, no account of Nuée's own and no licence server.
- All of it goes through the system's proxy settings and certificates, as Chrome does: see
  [Proxies and certificates](#proxies-and-certificates).
- Data stay on the computer. Projects are files saved wherever users choose.
- The calculations run in a local R process. It serves the interface on `127.0.0.1` only (port
  47863, or another free one), so no firewall rule is needed. It answers only Nuée's own windows:
  each request must carry a random token drawn at each start, in a cookie that no other site can
  send, and name 127.0.0.1 as its host. Web pages open in a browser can't use it, and neither can
  other users of a shared computer (Remote Desktop).

## Proxies and certificates

Nuée reaches GitHub and GitLab (signing in, sharing a project, its versions sent and received,
a project opened from a repository) through Chromium's network stack, as it does its update
check: the system's proxy settings (manual, a PAC file or WPAD) and the system's certificate
store apply, so a proxy that inspects TLS works once its root certificate is trusted by the
computer, as in Chrome or Edge. The hosts are `github.com`, `api.github.com` and
`avatars.githubusercontent.com` (the account's picture) for GitHub; `gitlab.com`, or your own
GitLab's host. The page where users approve Nuée opens in their default browser. *Use this
computer's git sign-in*, in the sign-in dialog, asks the git installed on the computer for its
stored sign-in (`git credential fill`), which follows git's own settings.

## What Nuée writes where

| What | Where |
|---|---|
| The program | `%LOCALAPPDATA%\Programs\Nuee` or `%ProgramFiles%\Nuee` (network-share copy: `%LOCALAPPDATA%\Nuee`) |
| Settings, recent files, interface preferences | `%APPDATA%\Nuee` |
| Recovery copies of projects with unsaved changes, every two minutes | `%APPDATA%\Nuee\secours`, deleted when Nuée closes normally |
| R's temporary files (pasted tables, converted `.usp` projects, console plots) | `%TEMP%\nuee-r-<process>`, deleted when Nuée closes, or at the next start after a crash |
| The installer of a one-click update | `%TEMP%\nuee-mise-a-jour`, deleted at the next start |
| Projects | Wherever users save them (`.nuee`) |
| Projects' versions (1.1) | A hidden folder beside the projects, `.nuee-versions`: see [The versions folder](#the-versions-folder) |
| Old versions opened or compared | R's temporary folder above, deleted when Nuée closes |
| Sign-ins and tokens for GitHub and GitLab, encrypted for the user by Windows (DPAPI, through Electron's `safeStorage`) | `%APPDATA%\Nuee\jetons.json` and `jetons-renouvellement.json`; the accounts' names, never a secret, and a self-hosted GitLab's application ID in `%APPDATA%\Nuee\reglages.json` |

## Projects on network folders

- A save never leaves a half-written project. The file is written next to the old one, then
  takes its place in one step, so a network cut or a full disk leaves the old file intact. A
  file another program keeps open (a sync client, an antivirus) is retried for five seconds,
  then left as it was, and Nuée says so; it is never written over in place.
- Projects aren't locked: two people can open the same one. Every 30 seconds, Nuée looks whether
  an open project's file was saved by someone else, and offers to merge their changes into the
  open project, by what changed (models, documents, data); *Save* offers it too, beside
  *Overwrite* and *Save as…*. A project recovered from its recovery copy after a crash, or opened
  before Nuée or its engine restarted, has no copy of the file as it was read to merge from:
  Nuée then asks before overwriting the file, and offers *Save as…* instead.
- The project's versions are kept beside it, on the share: see the next section.

## The versions folder

From 1.1, Nuée keeps versions of each project by itself, in a hidden folder beside the project
file, `.nuee-versions`: one history per project of that folder (`<name>.nuee.git`, a git
repository that Nuée writes itself, with no git to install) and a `.gitignore` of `*`, so that a
git repository of the user's around the folder ignores it. On Windows the folder has the hidden
attribute. Users see it as *File › History*.

- **On network shares:** the folder is made where the project is saved, so whoever can save a
  project there can keep its versions. A project's history is common to everyone who saves it,
  each version signed with the name its author set in *File › History › Settings…*. A lock file
  beside each history (`<name>.nuee.lock`, touched every 30 seconds while held, taken over after
  two minutes) keeps two copies of Nuée from writing it at once. For the versions, nothing else
  is written beside the projects.
- **Its size:** each version holds the whole project file, compressed (the example project to a
  quarter of its size); *File › History* shows the folder's size. A version is kept when a
  project is saved and its last one is an hour old, when it is closed, and before its data are
  updated. Automatic versions never sent anywhere are thinned out: one a day after 30 days, one
  a month after a year. Named versions, versions saved by hand and versions sent to a shared
  repository are all kept.
- **Deleting it** deletes the history of the projects in that folder, and nothing else: the
  projects don't change, and a new history starts at their next save. Users stop new histories
  in *File › History › Settings…* (*Keep versions of my projects*), or for one project (*Keep
  versions of this project*); there is no system-wide policy for it. A project shared on GitHub
  or GitLab loses its link to the repository with the folder; its versions stay in the
  repository.

## Signing in to your institution's GitLab

Users sign in to github.com and gitlab.com with Nuée's own applications there. To let them sign
in to your institution's GitLab too (GitLab 17.3 or later; 17.2 with the
`oauth2_device_grant_flow` feature flag), register Nuée once as an application of that GitLab:

1. *Admin › Applications › New application*: an instance-wide application, for every user.
2. Name `Nuée`; redirect URI `http://127.0.0.1/` (the form requires one; Nuée signs in with the
   device flow, which never uses it); *Trusted* ticked if users needn't approve it themselves;
   *Confidential* **unticked**: Nuée is a desktop application and keeps no secret;
   *Device authorization grant* **ticked**: a new application has it off, and Nuée's sign-in
   needs it.
3. Scopes `api` (making a project, adding a member), `read_user` (the account's name and
   picture) and `write_repository` (sending versions).
4. Give users the *Application ID* (64 hexadecimal digits). They type it once in Nuée's sign-in
   dialog, after the server's name, and Nuée keeps it in their settings. The *Secret* GitLab
   shows is not used: keep it to yourself.

The sign-in's tokens last two hours, as GitLab's OAuth tokens do; Nuée renews them while it
runs. An older GitLab, or one without the application, still works with a personal access token,
with the scopes `api`, `read_user` and `write_repository`.

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

## macOS

- **Installing:** copy `Nuée.app` from the `.dmg` into `/Applications`, by hand or with a
  deployment tool: `Nuee-<version>-arm64.dmg` for Apple silicon (macOS 14 or later),
  `Nuee-<version>-x64.dmg` for Intel (macOS 13 or later). Deleting `Nuée.app` uninstalls it.
- **R:** brought along in `Nuée.app`, as on Windows (CRAN's R 4.6.2 with the packages Nuée
  needs); an R already on the Mac is neither used nor changed. The user's `.Rprofile` is
  ignored, and the packages that come with Nuée are found before those of the user's own
  library.
- **Gatekeeper:** the application is signed ad hoc, not with an Apple Developer ID, and isn't
  notarized. A copy downloaded with a browser is quarantined, and macOS asks once to allow it
  (*System Settings › Privacy & Security › Open Anyway*). A copy put in place by a deployment
  tool, or downloaded with `curl`, isn't quarantined and starts without asking;
  `sudo xattr -dr com.apple.quarantine /Applications/Nuée.app` lifts the quarantine of one
  that is.
- **Updates:** the same check as on Windows, but a click only opens the release's page; the new
  `Nuée.app` replaces the old one. Users can turn the check off in *Help*; there is no
  system-wide policy on macOS.
- **What Nuée writes where:** settings and recovery copies in
  `~/Library/Application Support/Nuee`, R's temporary files in the user's own temporary folder
  (`$TMPDIR/nuee-r-<user id>-<process>`), the GitHub and GitLab sign-ins encrypted with a key
  kept in the user's keychain. Nothing is written inside `Nuée.app`. The network and file rules
  above are the same.

## Linux

- **Installing:** `sudo apt install ./Nuee-<version>-amd64.deb` on Debian, Ubuntu and their
  derivatives, or any tool that installs `.deb` packages. Nuée goes into `/opt/Nuée` for every
  user, with R and the R packages it needs from the distribution, a menu entry, the `.nuee`
  association and, on Ubuntu 24.04 and later, an AppArmor profile for Chromium's sandbox.
  `sudo apt remove nuee` uninstalls it. GitHub's checks install and use this package on
  Ubuntu 22.04 and 24.04 and Debian 12.
- **R:** the computer's R and its packages (only the Windows and macOS builds bring their own
  R). As in R
  itself, packages in a user's own library come before the system's; the user's `.Rprofile`
  is ignored.
- **Updates:** the same check as on Windows, but a click only opens the release's page; the
  new `.deb` installs over the old one. Users can turn the check off in *Help*; there is no
  system-wide policy on Linux.
- **What Nuée writes where:** settings and recovery copies in `~/.config/Nuee`, R's temporary
  files in `/tmp/nuee-r-<user id>-<process>`, the GitHub and GitLab sign-ins encrypted with a
  key kept in the desktop's keyring (GNOME Keyring, KWallet), or kept only until Nuée closes
  without one. The network and file rules above are the same.

## Reporting a problem

*Help › Diagnostic information* gives the versions of Nuée, Electron, R and its packages, to
paste into a report. It includes no paths and nothing personal.
