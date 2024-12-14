#!/bin/bash

echo "Starting ADA Bayern Stichprobenziehung..."

# Check for R
if ! command -v R &> /dev/null; then
  echo "Error: R is not installed. Please install R (version 4.1 or later) and rerun this script."
  exit 1
fi

# Launch the app
Rscript -e "shiny::runApp('.', launch.browser = TRUE)"