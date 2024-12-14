# ADA Bayern Stichprobenziehung

*(English version below)*

Eine Webanwendung zur geschichteten Stichprobenziehung mit Werkzeugen zur Datenexploration, Kategorisierung und Stichprobenerstellung.

---

## Anforderungen

- **R**: Version 4.1 oder neuer.

---

## Einrichtung / Installation

### Voraussetzungen

Für Linux-Nutzer: Stellen Sie sicher, dass die folgenden Bibliotheken auf Ihrem System installiert sind:  

```{bash}
sudo apt install libharfbuzz-dev libfribidi-dev libfreetype6-dev libpng-dev libtiff5-dev libjpeg-dev libgdal-dev
```

### Option 1: R-Konsole

Öffnen Sie eine R-Konsole (in RStudio oder Ihrem Terminal) und führen Sie den folgenden Befehl aus, um alle benötigten R-Pakete zu installieren:

```{R}
install.packages(c(
  'shiny', 'shinythemes', 'shinydashboard', 'shinyWidgets', 'DT',
  'tidyverse', 'tinytex', 'sortable', 'lpSolve', 'plotly', 'rmarkdown'
))
```

> ⚠️ Der Installationsprozess kann einige Minuten dauern.

Um die Erstellung von PDF-Berichten zu ermöglichen, installieren Sie TinyTeX mit folgendem Befehl:

```{R}
tinytex::install_tinytex()
```

### Option 2: Installationsskript

Windows: Doppelklicken Sie auf `install.bat`, um die obigen Schritte automatisch auszuführen.

Linux/Mac: Führen Sie das Skript `install.sh` aus, um die obigen Schritte auszuführen. Beachten Sie die oben genannten Voraussetzungen!

> ⚠️ Der Installationsprozess kann einige Minuten dauern.

---

## Anwendung starten

Das Projekt läuft als Webanwendung in Ihrem Browser. Um das Programm zu starten, folgen Sie einer der untenstehenden Methoden.

### Option 1: R-Konsole

Öffnen Sie Ihre R-Konsole im gleichen Verzeichnis wie `app.R` und starten Sie die App mit folgendem Befehl:

```{R}
shiny::runApp()
```

### Option 2: Kommandozeile

Alternativ können Sie die App über Ihr Terminal oder die CMD-Eingabeaufforderung starten:

```{bash}
R -e "shiny::runApp('app.R', launch.browser = TRUE)"
```

### Option 3: Startskript

Windows: Doppelklicken Sie auf `start.bat`, um die obigen Schritte automatisch auszuführen.

Linux/Mac: Führen Sie das Skript `start.sh` aus, um die obigen Schritte auszuführen.

---

## Für Entwickler

### Projektstruktur

Das Projekt ist wie folgt organisiert:

```
├── README.md                        # Diese Dokumentation
├── app.R                            # Hauptanwendungsdatei
├── report.Rmd                       # Vorlage zur Erstellung von PDF-Berichten
└── modules                          # Anwendungs-Module
    ├── tab0_manual.R                # Benutzerhandbuch (UI und Server)
    ├── tab1_start.R                 # Hochladen von Daten und Auswahl von Spalten
    ├── tab2_dashboard.R             # Datenexploration und Korrelationsanalyse
    ├── tab3_filter.R                # Filtern von Daten zur Erstellung einer Grundgesamtheit
    ├── tab4_categories.R            # Zusammenfassung von Werten in Kategorien für die Schichtung
    ├── tab5_sample.R                # Definition von Stichprobenparametern und Schichtgrößen
    ├── tab6_overview.R              # Ansicht der finalen Stichprobe und Exportmöglichkeiten
    └── helpers                      # Unterstützende Skripte
        ├── tab4_1_define_layer.R    # Sub-UI und Serverlogik für tab4_categories
        ├── tab4_2_select_groups.R   # Kategorisierungsalgorithmus
        ├── tab5_1_strata_sizes.R    # Algorithmus zur Berechnung optimaler Schichtgrößen
        ├── tab6_1_strat_sample.R    # Stichprobenalgorithmus unter Verwendung von Schichtgrößen
        └── utils.R                  # Gemeinsame Hilfsfunktionen
```

---

### Hinweise zur Entwicklung

- Stellen Sie sicher, dass neue Funktionen oder Module der modularen Struktur im Ordner `modules` folgen.
- Testen Sie Ihre Beiträge lokal, indem Sie die Anwendung ausführen (`shiny::runApp()`).
- Aktualisieren Sie diese ReadMe-Datei und die zugehörige Moduldokumentation für neue Funktionen.

---

### Unterstützung

Für Fragen oder Probleme kontaktieren Sie bitte den Projektbetreuer.


---
---


# ADA Bayern Sampling

A web application for stratified sampling, providing tools for data exploration, categorization, and sample generation.

---

## Requirements

- **R**: Version 4.1 or newer.

---

## Set Up / Installation

### Prerequisites

For Linux users: Ensure your system has the following libraries installed:  

```{bash}
sudo apt install libharfbuzz-dev libfribidi-dev libfreetype6-dev libpng-dev libtiff5-dev libjpeg-dev libgdal-dev
```

### Option 1: R Console

Open an R console (in RStudio or your terminal) and execute the following command to install all necessary R packages:

```{R}
install.packages(c(
  'shiny', 'shinythemes', 'shinydashboard', 'shinyWidgets', 'DT'
  'tidyverse', 'tinytex', 'sortable', 'lpSolve', 'plotly', 'rmarkdown'
))
```

> ⚠️ This installation process may take several minutes.

To enable the generation of PDF reports, install TinyTeX by running:

```{R}
tinytex::install_tinytex()
```

### Option 2: Installation Script

Windows: Double-click on `install.bat` to execute the steps above automatically.

Linux/Mac: Run the script `install.sh` to execute the steps above. Be aware of the prerequisites!

> ⚠️ This installation process may take several minutes.

---

## Run the Application

This project runs as a Webapp in your browser. To open the program follow either of the following methods.

### Option 1: R Console

Open your R console in the same directory as `app.R` and run the app by typing the following into your console:

```{R}
shiny::runApp()
```

### Option 2: Command Line

Alternatively, launch the app from your terminal or CMD-line with:

```{bash}
R -e "shiny::runApp('app.R', launch.browser = TRUE)"
```

### Option 3: Starter Script

Windows: Double-click on `start.bat` to execute the steps above automatically.

Linux/Mac: Run the script `start.sh` to execute the steps above.

---

## For Developers

### Project Structure

The project is organized as follows:

```
├── README.md                        # This documentation
├── app.R                            # Main application file
├── report.Rmd                       # Template for generating PDF reports
└── modules                          # Application modules
    ├── tab0_manual.R                # User manual (UI and server)
    ├── tab1_start.R                 # Upload data and select columns
    ├── tab2_dashboard.R             # Data exploration and correlation analysis
    ├── tab3_filter.R                # Filter data to create a population
    ├── tab4_categories.R            # Summarize values into categories for stratification
    ├── tab5_sample.R                # Define sampling parameters and strata sizes
    ├── tab6_overview.R              # View the final sample and download results
    └── helpers                      # Supporting scripts
        ├── tab4_1_define_layer.R    # Sub-UI and server logic for tab4_categories
        ├── tab4_2_select_groups.R   # Categorization algorithm
        ├── tab5_1_strata_sizes.R    # Algorithm for optimal strata size calculation
        ├── tab6_1_strat_sample.R    # Sampling algorithm using strata sizes
        └── utils.R                  # Shared utility functions
```

---

### Contribution Notes

- Ensure that any new features or modules follow the modular structure demonstrated in the `modules` folder.
- Test your contributions locally by running the application (`shiny::runApp()`).
- Update this ReadMe and the relevant module documentation for any new features.

---

### Support

For questions or issues, please contact the project maintainer.

