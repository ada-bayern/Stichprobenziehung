@echo off

echo Starte ADA Bayern Stichprobenziehung...

REM Prüfen, ob R installiert ist
where R >nul 2>nul
if %ERRORLEVEL% NEQ 0 (
    echo Fehler: R ist nicht installiert. Bitte installieren Sie R (Version 4.1 oder neuer) und starten Sie dieses Skript erneut.
    exit /b 1
)

REM Prüfen, ob app.R im aktuellen Verzeichnis existiert
if not exist "app.R" (
    echo Fehler: Die Datei "app.R" wurde im aktuellen Verzeichnis nicht gefunden.
    exit /b 1
)

REM App starten
Rscript -e "shiny::runApp('.', launch.browser = TRUE)"