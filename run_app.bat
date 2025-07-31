@echo off

set "RAPPDIR=%~dp0"
set "RAPPDIR=%RAPPDIR:\=/%"

echo %RAPPDIR%

cd "%~dp0R-Portable\bin"
Rscript.exe -e "shiny::runApp('%RAPPDIR%app', launch.browser = TRUE)"