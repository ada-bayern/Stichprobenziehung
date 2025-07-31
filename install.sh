#!/bin/bash

echo "Starte Installation von ADA Bayern Stichprobenziehung..."

# Check for R
if ! command -v R &> /dev/null; then
  echo "Error: R ist nicht installiert. Installieren Sie R (Version 4.1 oder später) und starten Sie den Installer von Neuem."
  exit 1
fi

echo "Installiere benötigte R-Pakete..."
Rscript install_deps.R

echo "Installation vollständig. Klicken Sie auf 'start.sh' zum starten der App."