<!-- description: Installing Nuée on Windows, macOS, Debian, Ubuntu and other Linux distributions. -->
# Install

Nuée {{version}} runs on Windows 10 and 11 (64-bit), on macOS 14 or later (Apple silicon) and 13
or later (Intel), and on Linux. The files are on GitHub's
[page of version {{version}}]({{telechargement:publication}}); earlier versions on the
[Releases]({{telechargement:toutes}}) page.

## Windows

1. Download [{{fichier:windows}}]({{telechargement:windows}}).
2. Run it and choose to install it for yourself (no administrator rights needed) or for every
   user of the computer.
3. Windows may say it protected your PC, because the installer isn't signed: click *More info*,
   then *Run anyway*.

R is included: there is nothing else to install, and an R already on the computer is neither used
nor changed. Nuée appears in the Start menu; double-clicking a `.nuee` project opens it in Nuée.
It is uninstalled from *Settings › Apps*; your projects stay where you saved them, and your
settings are kept.

### Updates

At start-up, Nuée asks GitHub whether a newer version exists (nothing else is sent). If there is
one, the status bar says so; a click downloads its installer, checks it against the SHA-256
checksum GitHub gives, offers to save your projects, installs it and starts Nuée again. *Help ›
Look for a new version at start-up* turns the check off; *Help › Check for updates* does it at any
time.

### In an organisation

For IT departments: silent installation for one user or the whole computer, the settings for
Intune, Configuration Manager, Group Policy and PDQ Deploy (commands, detection rule), a policy
that turns the update check off for everyone, what Nuée writes where and what it sends over the
network: see [the deployment guide](https://github.com/4rtefakt/nuee/blob/main/docs/deploiement.md).

## macOS

1. Download [{{fichier:macarm64}}]({{telechargement:macarm64}}) for a Mac with Apple silicon
   (macOS 14 or later), or [{{fichier:macx64}}]({{telechargement:macx64}}) for an Intel Mac
   (macOS 13 or later).
2. Open it and drag Nuée into *Applications*.
3. The application isn't signed with an Apple Developer ID, so the first time macOS says it
   couldn't check it: click *Done*, then *Open Anyway* in *System Settings › Privacy & Security*,
   and confirm. Nuée then opens as any other application.

R is included, as on Windows: there is nothing else to install, and an R already on the Mac is
neither used nor changed. Settings are kept in `~/Library/Application Support/Nuee`. At start-up,
Nuée asks GitHub whether a newer version exists; a click in the status bar opens its page, whose
`.dmg` replaces the application. Every change to Nuée is checked on Apple silicon and Intel Macs,
from the `.dmg`, on a Mac without R.

## Linux

On Linux, Nuée uses the R installed on the computer (an R build is tied to its distribution's
libraries, so it can't be carried along as on Windows).

### Debian, Ubuntu and their derivatives

Download [{{fichier:deb}}]({{telechargement:deb}}), then, in a terminal, in the file's folder:

```
sudo apt install ./{{fichier:deb}}
```

apt also installs R and the R packages Nuée needs. Nuée appears in the applications menu. Every
change to Nuée is checked with this package installed on Ubuntu 22.04 and 24.04 and Debian 12,
with their own R.

### Other distributions

First install R from your distribution, then the packages Nuée needs, in R:

```
install.packages(c("httpuv", "jsonlite", "openxlsx", "haven",
  "readxl", "foreign", "nlme", "mgcv", "MASS", "rpart", "cluster",
  "RSQLite", "ranger"))
```

Then either:

- [{{fichier:appimage}}]({{telechargement:appimage}}): make it executable
  (`chmod +x {{fichier:appimage}}`), then run it. It needs FUSE 2 (the `libfuse2` package on most
  distributions). On Ubuntu 24.04 and later, prefer the `.deb`: it installs the AppArmor profile
  Chromium's sandbox needs there, and an AppImage has to be started with `--no-sandbox`.
- [{{fichier:targz}}]({{telechargement:targz}}): unpack it, then run `./nuee`.

### Good to know

- Nuée looks for `Rscript` on the `PATH`, then in `/usr/local/bin`, `/usr/bin` and
  `/opt/R/<version>/bin`; start it with `RSCRIPT=/path/to/Rscript` to use another R.
- With an R older than 4.4 (that of Ubuntu 22.04 or 24.04, or Debian 12), R files (`.rds`,
  `.RData`) don't open, because reading one could run code (CVE-2024-27322): save the table as CSV
  or Excel instead.
- Opening `.usp` projects needs `python3`.
- Settings are kept in `~/.config/Nuee`. At start-up, Nuée asks GitHub whether a newer version
  exists; a click in the status bar opens its page.

## Then

Open a table (*File › Open*, Ctrl+O), or try *Help › Example project*, then follow
[a tutorial](page:tutoriels) or [the guide](page:guide.demarrer).
