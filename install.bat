@echo off
echo Starting installation of ADA Bayern Stichprobenziehung...

:: Check if R is installed
where R >nul 2>nul
if %errorlevel% neq 0 (
    echo Error: R is not installed. Please install R (version 4.1 or later) and rerun this script.
    exit /b 1
)

echo Installing required R packages...
Rscript -e "install.packages(c(
  'shiny', 'shinythemes', 'shinydashboard', 'shinyWidgets', 'DT',
  'tidyverse', 'tinytex', 'sortable', 'lpSolve', 'plotly', 'rmarkdown'
))"

echo Installing TinyTeX for PDF report generation...
Rscript -e "tinytex::install_tinytex()"

echo Installation complete. You can now run the application using the start script.
pause
