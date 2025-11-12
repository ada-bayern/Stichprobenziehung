# ADA Stichprobenziehung


Eine Webanwendung zur geschichteten Stichprobenziehung mit Werkzeugen zur Datenexploration, Kategorisierung und Stichprobenerstellung.



## Quickstart für Windows-Nutzer ohne globale R Installation

`ADA_Stichproben_Setup.exe` ist ein ausführbares Installationsprogramm, das alle erforderlichen Schritte für die Installation der Anwendung automatisiert. R und alle benötigten Pakete sind darin enthalten. Nach der Installation können Sie die Anwendung direkt über das Startmenü starten.

---
---

## Anforderungen

**R**: Version 4.1 oder neuer.

---

## Einrichtung / Installation

Führen Sie das Installationsskript `install_deps.R` aus, um alle benötigten R-Pakete zu installieren. Das können Sie fogendermaßen tun:
### Option 1: RStudio
1. Öffnen Sie `install_deps.R` in RStudio.
2. Klicken Sie auf "Run" oder drücken Sie `Ctrl + Enter`, um das Skript auszuführen.

### Option 2: R-Konsole
1. Navigieren Sie zum Verzeichnis, in dem sich `install_deps.R` befindet.
2. Öffnen Sie eine R-Konsole (in RStudio oder Ihrem Terminal).
3. Führen Sie den folgenden Befehl aus:
```{R}
source("install_deps.R")
```

### Option 3 Kommandozeile:
1. Öffnen Sie Ihr Terminal oder die CMD-Eingabeaufforderung.
2. Navigieren Sie zum Verzeichnis, in dem sich `install_deps.R` befindet.
3. Führen Sie den folgenden Befehl aus:

*Linux/Mac:*
```{bash}
Rscript install_deps.R
```

*Windows:*
```{bash}
Rscript.exe install_deps.R
```

### Option 4: Shell-Skript
Führen Sie das Skript `install.bat` (Windows) / `install.sh` (Linux/Mac) mit einem Doppelklick aus, um die obigen Schritte automatisch auszuführen.



> ⚠️ Der Installationsprozess kann einige Minuten dauern.

---

## Anwendung starten

Das Projekt läuft als Webanwendung in Ihrem Browser. Um das Programm zu starten, folgen Sie einer der untenstehenden Methoden.

### Option 1: RStudio
1. Öffnen Sie `app/app.R` in RStudio.
2. Klicken Sie auf "Run App" oder drücken Sie `Ctrl + Shift + Enter`, um die App zu starten.

### Option 2: R-Konsole
1. Navigieren Sie zum Verzeichnis, in dem sich `app.R` befindet.
2. Öffnen Sie eine R-Konsole (in RStudio oder Ihrem Terminal).
3. Führen Sie den folgenden Befehl aus:

```{R}
shiny::runApp('app.R', launch.browser = TRUE)
```

### Option 3: Kommandozeile
1. Öffnen Sie Ihr Terminal oder die CMD-Eingabeaufforderung.
2. Navigieren Sie zum Verzeichnis, in dem sich `app.R` befindet.
3. Führen Sie den folgenden Befehl aus:

*Linux/Mac*
```{bash}
Rscript -e "shiny::runApp('app.R', launch.browser = TRUE)"
```

*Windows*
```{bash}
Rscript.exe -e "shiny::runApp('app/app.R', launch.browser = TRUE)"
```

### Option 4: Startskript
Doppelklicken Sie auf `start.bat` (Windows) / `start.sh` (Linux/Mac), um die obigen Schritte automatisch auszuführen.

---

## Für Entwickler

Der Quellcode der App ist modular aufgebaut und in verschiedene Module unterteilt, die jeweils eine spezifische Funktionalität der Anwendung bereitstellen. Er liegt im Verzeichnis `app`.

### Projektstruktur

Der Code ist wie folgt organisiert:

```
app/
├── README.md                        # Diese Dokumentation
├── app.R                            # Hauptanwendungsdatei
├── report.Rmd                       # Vorlage zur Erstellung von PDF-Berichten
└── modules/                         # Anwendungs-Module
    ├── tab0_manual.R                # Benutzerhandbuch (UI und Server)
    ├── tab1_start.R                 # Hochladen von Daten und Auswahl von Spalten
    ├── tab2_dashboard.R             # Datenexploration und Korrelationsanalyse
    ├── tab3_filter.R                # Filtern von Daten zur Erstellung einer Grundgesamtheit
    ├── tab4_categories.R            # Zusammenfassung von Werten in Kategorien für die Schichtung
    ├── tab5_sample.R                # Definition von Stichprobenparametern und Schichtgrößen
    ├── tab6_overview.R              # Ansicht der finalen Stichprobe und Exportmöglichkeiten
    └── helpers/                     # Unterstützende Skripte
        ├── manual.R                 # Einstellungen und Texte für alle Infomodule
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
