# Deploying Nuée in an organisation

For IT departments installing Nuée on several Windows computers: how to install it silently,
the settings for Intune, Configuration Manager, Group Policy and other deployment tools, how to
turn off its update check, what it writes where, and what it sends over the network. Linux
computers: see [Linux](#linux) at the end.

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
| For the user running it | `Nuee-Setup-1.0.0.exe /S /currentuser` | `%LOCALAPPDATA%\Programs\Nuee` |
| For every user of the computer (administrator, e.g. run as SYSTEM by Configuration Manager or Intune) | `Nuee-Setup-1.0.0.exe /S /allusers` | `%ProgramFiles%\Nuee` |

`/D=D:\Apps\Nuée` (last, without quotes, even with spaces) installs elsewhere. The program is
`Nuee.exe` in that folder. The installer adds Start menu and desktop shortcuts, an entry in
*Settings › Apps*, and opens `.nuee` projects in Nuée (ProgID `Nuee.Projet`). To uninstall
silently: `"<install folder>\Uninstall Nuee.exe" /allusers /S` (or `/currentuser /S`), the
command the entry in *Settings › Apps* also gives (`QuietUninstallString`). Uninstalling keeps
the users' projects and settings. The installer and the uninstaller exit with 0 when they
succeed; no restart is needed.

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
SYSTEM. Its values, for version 1.0.0 (put the version you deploy in the file name and the
detection rule):

| | |
|---|---|
| Install command | `Nuee-Setup-1.0.0.exe /S /allusers` |
| Uninstall command | `"%ProgramFiles%\Nuee\Uninstall Nuee.exe" /allusers /S` |
| Exit codes | 0 when it worked; no restart needed |
| Detection | Registry, 64-bit view: key `HKEY_LOCAL_MACHINE\SOFTWARE\Microsoft\Windows\CurrentVersion\Uninstall\3dabb207-3e0d-5aa0-bdfc-da17bd7f0503`, value `DisplayVersion`, a version greater than or equal to `1.0.0` |
| Requirements | Windows 10 or 11, 64-bit (x64); about 550 MB of disk |
| Updating | Run the new version's installer with the same command: it replaces the installed copy in place |

The key's name is Nuée's own and the same for every version (it comes from the application's
ID, `io.github.4rtefakt.nuee`). An installation per user is the same with `/currentuser`, the
folder `%LOCALAPPDATA%\Programs\Nuee` and the key under `HKEY_CURRENT_USER`, run in the user's
context.

### Microsoft Intune

As a *Windows app (Win32)*:

1. **Package** the installer with the
   [Microsoft Win32 Content Prep Tool](https://github.com/microsoft/Microsoft-Win32-Content-Prep-Tool):
   put `Nuee-Setup-1.0.0.exe` alone in a folder, then
   `IntuneWinAppUtil.exe -c <that folder> -s Nuee-Setup-1.0.0.exe -o <output folder>`, and upload
   the `.intunewin` file it writes.
2. **Program:** install command `Nuee-Setup-1.0.0.exe /S /allusers`; uninstall command
   `"%ProgramFiles%\Nuee\Uninstall Nuee.exe" /allusers /S`; install behavior *System*; device
   restart behavior *No specific action*; return codes as they are.
3. **Requirements:** operating system architecture *x64*; minimum operating system, the oldest
   Windows 10 offered.
4. **Detection rules:** *Manually configure detection rules*, rule type *Registry*: key path
   `HKEY_LOCAL_MACHINE\SOFTWARE\Microsoft\Windows\CurrentVersion\Uninstall\3dabb207-3e0d-5aa0-bdfc-da17bd7f0503`,
   value name `DisplayVersion`, detection method *Version comparison*, operator *Greater than or
   equal to*, value `1.0.0`, associated with a 32-bit app on 64-bit clients *No*.
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

- **Programs:** installation program `Nuee-Setup-1.0.0.exe /S /allusers`; uninstall program
  `"%ProgramFiles%\Nuee\Uninstall Nuee.exe" /allusers /S`.
- **Detection method:** setting type *Registry*, hive `HKEY_LOCAL_MACHINE`, key
  `SOFTWARE\Microsoft\Windows\CurrentVersion\Uninstall\3dabb207-3e0d-5aa0-bdfc-da17bd7f0503`,
  value `DisplayVersion`, data type *Version*, the rule *Greater than or equals* `1.0.0`; leave
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
(*Domain Computers*). This one installs 1.0.0 unless that version is already there; change both
version numbers to deploy another:

```bat
@echo off
reg query "HKLM\SOFTWARE\Microsoft\Windows\CurrentVersion\Uninstall\3dabb207-3e0d-5aa0-bdfc-da17bd7f0503" /v DisplayVersion /reg:64 2>nul | findstr /e /c:" 1.0.0" >nul
if errorlevel 1 "\\server\share\Nuee-Setup-1.0.0.exe" /S /allusers
```

To turn the update check off: *Computer Configuration › Preferences › Windows Settings ›
Registry*, a new registry item with action *Update*, hive `HKEY_LOCAL_MACHINE`, key path
`SOFTWARE\Policies\Nuee`, value name `DisableUpdateCheck`, type `REG_DWORD`, value `1`.

### PDQ Deploy and other tools

- **PDQ Deploy:** an *Install* step with `Nuee-Setup-1.0.0.exe`, parameters `/S /allusers`,
  success code `0`, run as *Local System* or as a deploy user who is an administrator.
- **Other tools** (ManageEngine, Ivanti, NinjaOne, Chocolatey for Business, a private WinGet
  source…): the installer is an NSIS one (*nullsoft* for WinGet); the commands, exit codes and
  detection rule above apply as they are.

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
| The program | `%LOCALAPPDATA%\Programs\Nuee` or `%ProgramFiles%\Nuee` (network-share copy: `%LOCALAPPDATA%\Nuee`) |
| Settings, recent files, interface preferences | `%APPDATA%\Nuee` |
| Recovery copies of projects with unsaved changes, every two minutes | `%APPDATA%\Nuee\secours`, deleted when Nuée closes normally |
| R's temporary files (pasted tables, converted `.usp` projects, console plots) | `%TEMP%\nuee-r-<process>`, deleted when Nuée closes, or at the next start after a crash |
| The installer of a one-click update | `%TEMP%\nuee-mise-a-jour`, deleted at the next start |
| Projects | Wherever users save them (`.nuee`) |

## Projects on network folders

- A save never leaves a half-written project. The file is written next to the old one, then
  takes its place in one step, so a network cut or a full disk leaves the old file intact. A
  file another program keeps open (a sync client, an antivirus) is retried for five seconds,
  then left as it was, and Nuée says so; it is never written over in place.
- Projects aren't locked: two people can open the same one. If the file changed on disk since it
  was opened or last saved (a colleague, another window), Nuée asks before overwriting it and
  offers *Save as…* instead. This also holds for a project recovered from its recovery copy
  after a crash or an engine restart.

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

## Linux

- **Installing:** `sudo apt install ./Nuee-<version>-amd64.deb` on Debian, Ubuntu and their
  derivatives, or any tool that installs `.deb` packages. Nuée goes into `/opt/Nuée` for every
  user, with R and the R packages it needs from the distribution, a menu entry, the `.nuee`
  association and, on Ubuntu 24.04 and later, an AppArmor profile for Chromium's sandbox.
  `sudo apt remove nuee` uninstalls it. GitHub's checks install and use this package on
  Ubuntu 22.04 and 24.04 and Debian 12.
- **R:** the computer's R and its packages (only the Windows build brings its own R). As in R
  itself, packages in a user's own library come before the system's; the user's `.Rprofile`
  is ignored.
- **Updates:** the same check as on Windows, but a click only opens the release's page; the
  new `.deb` installs over the old one. Users can turn the check off in *Help*; there is no
  system-wide policy on Linux.
- **What Nuée writes where:** settings and recovery copies in `~/.config/Nuee`, R's temporary
  files in `/tmp/nuee-r-<user id>-<process>`. The network and file rules above are the same.

## Reporting a problem

*Help › Diagnostic information* gives the versions of Nuée, Electron, R and its packages, to
paste into a report. It includes no paths and nothing personal.
