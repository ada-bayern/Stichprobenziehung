# ADA Bayern Stichprobenziehung

## Requirements

-   R \>= 4.2

## Set Up

```         
$ sudo apt install libharfbuzz-dev libfribidi-dev libfreetype6-dev libpng-dev libtiff5-dev libjpeg-dev libgdal-dev
```

Open the R console in RStudio or by typing `R` into your cmd-line. To install the required packages, type:

```         
> install.packages(c('shiny', 'ggplot2', 'shinythemes', 'shinydashboard', 'shinyWidgets', 'tidyverse', 'sf', 'tinytex', 'sortable', 'DT', 'lpSolve', 'plotly', 'viridis'))
```

This can take some minutes...

## Run

Run the App by typing the following into your R console:

```         
shiny::runApp()
```

## Data Requirements

-   Cols:

    -   "Bezirk"

    -   

## For Developers

### Project Structure

```         
├── app.R                           # main
├── .gitignore                 
├── README.md
└── modules
    ├── define_layer_modules_old
    ├── define_layer_module         # ?
    ├── tab1_module.R               # intro, manual
    ├── tab2_1_module.R             # deprecated
    ├── tab2_module.R               # ?
    ├── tab3_module.R               # ?
    ├── tab4_module.R               # ?
    ├── tab5_module.R               # ?
    └── tab6_module.R               # ?
```

### TODO

1.  Installationspezifikation und Dokumentation

-   [ ] get example data
-   [ ] installation
-   [ ] test run
-   [ ] documentation

2.  Neuartige Daten annehmen (für C-Verfahren)

-   [ ] generalization

3.  Integration in Behördeninfrastruktur

-   [ ] deployment

## Freitag

-   Folien komplettieren
