; Nuée's addition to electron-builder's installer script (construire.js: nsis.include).
;
; An installation over Nuée's own folder keeps its Start menu and desktop shortcuts, and with
; them Nuée pinned to the taskbar, even when the version before runs it without --updated (0.9.9's
; updater did, main.js). Because the folder can be chosen (allowToChangeInstallationDirectory),
; electron-builder keeps the shortcuts of the version it replaces only with --updated
; (include/installUtil.nsh: setIsTryToKeepShortcuts): another folder would leave them pointing at
; the old one. Its own check already covers that case: it keeps them only when Nuee.exe is in the
; folder being installed to (installSection.nsh, uninstallOldVersion). So, once the pages are
; declared (the folder's among them, assistedInstaller.nsh), the symbol goes, and the macros
; inserted after it (the installation section) always try to keep them.
; (app/tests/essai_mise_a_jour.js checks it on Windows: 0.9.9 installed, then updated as it does.)
!macro customHeader
  !ifdef allowToChangeInstallationDirectory
    !undef allowToChangeInstallationDirectory
  !endif
!macroend
