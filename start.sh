#!/bin/bash

echo "Starting ADA Bayern Stichprobenziehung..."

# Check for R
if ! command -v R &> /dev/null; then
  echo "Fehler: R ist nicht installiert. Bitte installieren Sie R (Version 4.1 oder neuer) und starten Sie dieses Skript erneut."
  exit 1
fi

# Launch the app
Rscript -e "shiny::runApp('app', launch.browser = TRUE)"