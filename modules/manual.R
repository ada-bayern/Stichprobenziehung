# manual.R

#' UI-Funktion für die Bedienungsanleitung
#'
#' Diese Funktion erstellt die Benutzeroberfläche für die Bedienungsanleitung.

library(shiny)


# TODO: finish

manual_ui <- function(id) {
  ns <- NS(id)
  fluidPage(
    titlePanel("Bedienungsanleitung"),

    # Inhalt der Bedienungsanleitung
    fluidRow(
      column(
        width = 12,
        #h2("Übersicht der App-Struktur"),
        p("Die ADA Bayern Stichprobenziehung-App ist eine Shiny-Anwendung, die
           Archivare bei der Bearbeitung juristischer Datensätze unterstützt.
           Die App bietet Funktionen zum Hochladen, Erkunden, Filtern und
           Stratifizieren von Daten, um eine fundierte Stichprobenziehung
           durchzuführen. Die Benutzerführung erfolgt über ein Dashboard mit
           mehreren Modulen, die jeweils spezifische Aufgaben abdecken. Mit
           Hilfe des „Weiter“-Buttons wird der Nutzer Schritt für Schritt durch
           die verschiedenen Phasen des Prozesses geführt."),

        h3("1. Daten hochladen und vorverarbeiten"),
        p("Laden Sie die Daten in das System, indem Sie die entsprechenden
           Schaltflächen im Upload-Bereich der App verwenden. Stellen Sie
           sicher, dass die Daten das erwartete Format haben."),
        tags$ul(
          tags$li("Gehen Sie zum Tab 'Daten hochladen'."),
          tags$li("Wählen Sie Ihre Datei im CSV-Format aus und laden Sie sie
                   hoch."),
          tags$li("Die hochgeladenen Daten werden im Modul angezeigt und sind
                   für die weiteren Schritte der Analyse verfügbar."),
          tags$li("Nach dem Hochladen können im Dropdown-Menü „Spaltenauswahl“
                   bestimmte Spalten für die weitere Analyse ausgewählt werden.
                   Die Vorschau wird entsprechend angepasst")
        ),
        p("Hinweise:"),
        tags$ul(
          tags$li("Achten Sie darauf, dass die Spaltennamen und Datenformate
                   korrekt sind."),
          tags$li("Das aktuelle Dateigrößenlimit beträgt **160MB**")
        ),

        h3("2. Datenübersicht"),
        p("Im Tab 'Datenübersicht' können Sie Variablen (Spalten) aus Ihrem
           Datensatz auswählen und grundlegende Zusammenfassungen sowie
           Verteilungen anzeigen."),
        tags$ul(
          tags$li("Wählen Sie unter 'Hauperkmal' die gewünschte Spalte aus, die
                   analysiert werden soll."),
          tags$li("Wählen Sie unter 'Gruppierungsmerkmal' die gewünschte Spalte
                   aus, deren Zusammenhang mit dem Hauptmerkmal Sie besser
                   verstehen möchten."),
          tags$li("Optional können Sie mit dem 'Filter'-Dropdown spezifische
                   Werte des Gruppierungsmerkmals filtern."),
          tags$li("Aktivieren Sie die 'Log-Skala'-Schaltfläche, um eine
                   logarithmische Skala auf die y-Axchse des oberen Plots
                   anzuwenden, falls die Daten logarithmisch besser darstellbar
                   sind.")
        ),
        p("Hinweise:"),
        tags$ul(
          tags$li("Die Verteilung der ausgewählten Variablen wird in einem Plot
                   angezeigt."),
          tags$li("Darunter wird in einem weiteren Plot der Zusammenhang mit dem
                   Gruppierungsmerkmal dargestellt."),
          tags$li("Ein tabellarischer Überblick der ausgewählten Spalte
                   erscheint unten im Bereich 'Zusammenfassung'."),
          tags$li("Nutzen Sie dieses Dashboard, um zentrale Einblicke in die
                   Struktur und Verteilung der Daten zu gewinnen, bevor Sie zur
                   Datenauswahl oder weiteren Analysemodulen übergehen.")
        ),

        h3("3. Auswahl der Daten"),
        p("Im Tab 'Datenauswahl' können Sie Variablen (Spalten) aus Ihrem
           Datensatz auswählen und Werte, die Sie in Ihrer Stichprobe nicht
           benötigen herausfiltern."),
        tags$ul(
          tags$li("Wählen Sie unter 'Hauperkmal' die gewünschte Spalte aus, die
                   analysiert werden soll."),
          tags$li("Optional können Sie mit dem 'Filter'-Dropdown spezifische
                   Werte filtern."),
          tags$li("Drücken Sie 'Filter unwiderruflich anwenden', um den Filter
                   auf den Datensatz anzuwenden."),
          tags$li("Aktivieren Sie die 'Log-Skala'-Schaltfläche, um eine
                   logarithmische Skala auf die y-Axchse des oberen Plots
                   anzuwenden, falls die Daten logarithmisch besser darstellbar
                   sind.")
        ),
        p("Hinweise:"),
        tags$ul(
          tags$li("Die Verteilung der ausgewählten Variablen wird in einem Plot
                   angezeigt."),
          tags$li("Darunter wird in einem weiteren Plot der Zusammenhang mit dem
                   Gruppierungsmerkmal dargestellt."),
          tags$li("Ein tabellarischer Überblick der ausgewählten Spalte
                   erscheint unten im Bereich 'Zusammenfassung'."),
          tags$li("Nutzen Sie dieses Dashboard, um zentrale Einblicke in die
                   Struktur und Verteilung der Daten zu gewinnen, bevor Sie zur
                   Datenauswahl oder weiteren Analysemodulen übergehen.")
        ),

        h3("4. Kategorien erstellen und Schichten definieren"),
        p("Im Tab 'Variablen auswählen und Kategorien erstellen' haben Sie die
           Möglichkeit, Variablen auszuwählen und basierend auf bestimmten
           Kriterien Kategorien oder Schichten (Strata) zu definieren."),
        tags$ul(
          tags$li("Klicken Sie auf 'Schicht hinzufügen', um eine neue
                   Schichtungsebene zu erstellen."),
          tags$li("Ein neues Tab mit Konfigurationseinstellungen für die
                   Schichtung wird automatisch erstellt."),
          tags$li("Wählen Sie die Spalte aus, die als Grundlage für die
                   Schichtung dienen soll."),
          tags$li("Definieren Sie die Kategorien innerhalb dieser Spalte durch
                   Auswahl spezifischer Werte oder Intervalle (numerisch oder
                   kategorisch)."),
          tags$li("Wiederholen Sie den Vorgang, falls mehrere Schichtungen
                   erforderlich sind.")
        ),
        p("Hinweise:"),
        tags$ul(
          tags$li("Jede Schichtung wird in einem separaten Tab angezeigt und
                   kann einzeln bearbeitet werden."),
          tags$li("# TODO: Definieren Sie, wie numerische Intervalle und
                   kategoriale Gruppen für die Schichten erstellt werden."),
          tags$li("Beachten Sie, dass jede Spalte nur in einer Schichtungsebene
                   verwendet werden kann.")
        ),

        h3("4. Crosstab für Schichtungen erstellen"),
        p("Im Sidebar-Bereich dieses Moduls können Sie Kreuztabellen zwischen
           zwei ausgewählten Schichten erstellen."),
        tags$ul(
          tags$li("Wählen Sie in den Dropdown-Menüs '1. Spalte auswählen' und
                   '2. Spalte auswählen' die beiden Schichtungen aus, die Sie
                   vergleichen möchten."),
          tags$li("Die resultierende Kreuztabelle wird angezeigt und zeigt die
                   Häufigkeitsverteilung der ausgewählten Kategorien.")
        ),
        p("Hinweise:"),
        tags$ul(
          tags$li("Verwenden Sie diese Funktion, um Beziehungen und Muster
                   zwischen verschiedenen Schichtungen zu untersuchen."),
          tags$li("# TODO: Erläutern Sie, wie diese Analyse verwendet werden
                   kann, um bestimmte Fragestellungen zu beantworten.")
        ),

        h3("5. Voreinstellungen und Datenvorgaben laden"),
        p("Falls Sie Voreinstellungen für bestimmte Schichtungen oder
           Datenkategorien gespeichert haben, können Sie diese im Modul laden
           und anwenden."),
        tags$ul(
          tags$li("Wählen Sie im Menü 'Voreinstellungen' die gewünschten
                   Presets aus."),
          tags$li("Die App lädt automatisch die gespeicherten Schichtungen und
                   Kategorien."),
          tags$li("Sie können die geladenen Schichtungen wie oben beschrieben
                   weiter anpassen.")
        ),
        p("Hinweise:"),
        tags$ul(
          tags$li("# TODO: Erklären Sie, wie Presets erstellt und gespeichert
                   werden können.")
        ),

        h2("Hinweise zur Bedienung"),
        p("# TODO: Fügen Sie hier eine Beschreibung hinzu, die allgemeine Tipps
           zur Bedienung und Interpretation der Ergebnisse enthält. Diese
           Hinweise können auch bekannte Einschränkungen oder bekannte Bugs
           betreffen.")
      )
    )
  )
}

#' Server-Funktion für die Bedienungsanleitung (aktuell keine Logik benötigt)
manual_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    # TODO?
  })
}
