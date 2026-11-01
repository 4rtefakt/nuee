@echo off
rem Lance Nuee depuis le NAS.
rem Windows ne peut pas executer l'application directement sur un partage reseau : elle est
rem copiee sur cet ordinateur (la premiere fois en entier, ensuite seulement ce qui a change
rem sur le NAS), puis lancee depuis cette copie. Au lancement, Nuee s'inscrit dans
rem Parametres > Applications installees, d'ou on peut la desinstaller.
setlocal
set "SOURCE=%~dp0application"
set "CIBLE=%LOCALAPPDATA%\Nuee"
if not exist "%CIBLE%\Nuee.exe" (
  echo Premier lancement : copie de Nuee sur cet ordinateur ^(1 a 2 minutes^)...
) else (
  echo Mise a jour de Nuee...
)
robocopy "%SOURCE%" "%CIBLE%" /MIR /MT:8 /NFL /NDL /NJH /NJS /NP /R:1 /W:1 >nul
if %ERRORLEVEL% GEQ 8 (
  if not exist "%CIBLE%\Nuee.exe" (
    echo.
    echo La copie depuis le NAS a echoue. Verifiez que le NAS est accessible.
    pause
    exit /b 1
  )
  rem copie incomplete (application deja ouverte, NAS indisponible) : on lance la version deja copiee
  echo Mise a jour impossible pour l'instant ^(fermez Nuee pour la mettre a jour^).
  timeout /t 3 >nul
)
rem La version precedente s'appelait Analyse multivariee : sa copie et son raccourci partent.
if exist "%LOCALAPPDATA%\AnalyseMultivariee" rmdir /s /q "%LOCALAPPDATA%\AnalyseMultivariee" >nul 2>&1
rem Raccourcis vers ce lanceur, sur le Bureau et dans le menu Demarrer (crees une seule fois) :
rem l'application se lance ensuite de la, toujours a jour avec le NAS.
powershell -NoProfile -Command "$n = 'Nu' + [char]0xE9 + 'e.lnk'; $w = New-Object -ComObject WScript.Shell; foreach ($d in [Environment]::GetFolderPath('Desktop'), [Environment]::GetFolderPath('Programs')) { $l = Join-Path $d $n; if (-not (Test-Path -LiteralPath $l)) { $s = $w.CreateShortcut($l); $s.TargetPath = '%~f0'; $s.WorkingDirectory = '%CIBLE%'; $s.IconLocation = '%CIBLE%\Nuee.exe,0'; $s.Description = 'Nuee : ACP, PLS, PLS-DA, OPLS'; $s.WindowStyle = 7; $s.Save() } }; Remove-Item -LiteralPath (Join-Path ([Environment]::GetFolderPath('Desktop')) 'Analyse multivariee.lnk') -ErrorAction SilentlyContinue" >nul 2>&1
start "" "%CIBLE%\Nuee.exe" --lanceur-nas %*
