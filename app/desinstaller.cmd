@echo off
rem Desinstalle Nuee lancee depuis le NAS (Parametres > Applications installees > Nuee) :
rem raccourcis, copie sur cet ordinateur, inscription dans les applications installees.
rem Les projets et les reglages (%APPDATA%\Nuee) sont gardes. Ce fichier est dans le dossier
rem a effacer : il se recopie d'abord dans %TEMP% et continue depuis la copie.
setlocal
if not "%~1"=="copie" goto recopier
set "CIBLE=%LOCALAPPDATA%\Nuee"
:ouverte
tasklist /fi "imagename eq Nuee.exe" | find /i "Nuee.exe" >nul
if not errorlevel 1 (
  echo Nuee est ouverte : fermez-la, puis appuyez sur une touche.
  pause >nul
  goto ouverte
)
echo Desinstallation de Nuee...
powershell -NoProfile -Command "$n = 'Nu' + [char]0xE9 + 'e.lnk'; foreach ($d in [Environment]::GetFolderPath('Desktop'), [Environment]::GetFolderPath('Programs')) { Remove-Item -LiteralPath (Join-Path $d $n) -ErrorAction SilentlyContinue }" >nul 2>&1
rmdir /s /q "%CIBLE%" >nul 2>&1
reg delete "HKCU\Software\Microsoft\Windows\CurrentVersion\Uninstall\Nuee" /f >nul 2>&1
if exist "%CIBLE%" (
  echo Certains fichiers de %CIBLE% n'ont pas pu etre effaces : redemarrez l'ordinateur et effacez ce dossier.
) else (
  echo Nuee est desinstallee. Les projets et les reglages ^(%APPDATA%\Nuee^) sont gardes.
)
echo Pour la reinstaller : relancer "Lancer Nuee.cmd" sur le NAS.
pause
(goto) 2>nul & del "%~f0"

:recopier
copy /y "%~f0" "%TEMP%\desinstaller-nuee.cmd" >nul
rem sans call : la suite se passe dans la copie, ce fichier n'est plus lu
"%TEMP%\desinstaller-nuee.cmd" copie
