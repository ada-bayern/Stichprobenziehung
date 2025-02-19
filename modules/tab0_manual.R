#' Module: Manual for the Shiny App

library(shiny)

source("modules/helpers/utils.R")

manual_ui <- function(id) {
  fluidPage(
    titlePanel("Bedienungsanleitung"),

    # Inhalt der Bedienungsanleitung
    fluidRow(column(12,
      tags$b("English version below"),
      h2("Hinweise zur Bedienung"),
      tags$ul(
        tags$li("Ein Klick auf die 'Weiter'-Schaltfläche bringt Sie zum
                nächsten Abschnitt, sofern, Sie in der Datenbearbeitung so
                weit sind."),
        tags$li("Wenn Sie im letzten Abschnitt ('Überblick') angelangt sind,
                können Sie mit einem Klick auf die Schaltfläche 'Schließen'
                die App beenden. Außerdem können Sie jederzeit den Browser-Tab
                schließen, um das Programm zu beenden, allerdings werden Ihre
                Fortschritte NICHT gespeichert!"),
        tags$li("Mit einem Klick auf die drei Balken in der grünen Kopfzeile
                wird das Menü angezeigt. Hier können Sie zwischen den
                einzelnen Abschnitten hin- und hernavigieren und auch diese
                Anleitung aufrufen.")
      ),

      h2("Übersicht der App-Struktur"),
      p("Die ADA Bayern Stichprobenziehung-App ist eine Shiny-Anwendung, die
          Archivare bei der Bearbeitung juristischer Datensätze unterstützt.
          Die App bietet Funktionen zum Hochladen, Erkunden, Filtern und
          Stratifizieren von Daten, um eine fundierte Stichprobenziehung
          durchzuführen. Die Benutzerführung erfolgt über ein Dashboard mit
          mehreren Abschnitten, die jeweils spezifische Aufgaben abdecken. Mit
          Hilfe des „Weiter“-Buttons werden Sie Schritt für Schritt durch
          die verschiedenen Phasen des Prozesses geführt."),

      h3("1. Upload"),
      h4("CSV-Daten hochladen und verwalten"),
      p("Dieser Abschnitt ermöglicht das Hochladen und Verwalten von CSV-
        Dateien. Nutzer*innen können Dateien hochladen, Importoptionen
        konfigurieren und die Daten vor der Analyse oder Verarbeitung in
        der Anwendung prüfen."),
      tags$ul(
        tags$li("Navigieren Sie zum Tab 'Daten-Upload'."),
        tags$li("Wählen Sie Ihre CSV-Datei aus und laden Sie sie hoch, indem
                Sie die Schaltfläche 'CSV-Datei auswählen' verwenden."),
        tags$li("Nutzen Sie das Optionsfeld, um anzugeben, ob die erste
                Zeile der Datei als Kopfzeile verwendet wird."),
        tags$li("Wählen Sie den entsprechenden Trennzeichen aus Komma,
                Semikolon oder Tabulator."),
        tags$li("Wählen Sie den passenden Dezimaltrennzeichen.")
      ),
      p("Datenvorschau und weitere Hinweise:"),
      tags$ul(
        tags$li("Nachdem die Datei ausgewählt ist, wird eine Vorschau der
                Daten angezeigt. Nutzen Sie dies, um die Korrektheit der
                Daten sicherzustellen."),
        tags$li("Die Vorschau zeigt standardmäßig die ersten fünf Zeilen der
                Datei."),
        tags$li("Im erscheinenden Bereich 'Spaltenauswahl' können Sie den
                Datentyp der einzelnen Spalten anpassen oder die Spalte
                entfernen. Dabei können nur numerische Spalten in kategorische
                umgewandelt, also jeder Zahl eine Kategorie zugeordnet, werden.
                Das sollte i.d.R. nur getan werden, wenn die Ordnung der Werte
                nicht relevant ist und es sich um eine überschaubare Menge an
                Werten handelt."),
        tags$li("Klicken Sie auf 'Auswahl hochladen', um die ausgewählten
                Spalten zu speichern. Die gesamten Daten werden nun für
                die weitere Analyse bereitgestellt. In der Datenvorschau
                ist nun sichtbar, dass sich die Anzahl der Reihen verändert
                hat."),
        tags$li("Stellen Sie sicher, dass die Dateigröße das Limit von 500 MB
                nicht überschreitet und dass das Format korrekt ist, um
                erfolgreiche Datenverarbeitung zu gewährleisten."),
        tags$li("Wenn Sie beim Upload Probleme mit Sonderzeichen haben liegt
                dies vermutlich an der Encodierung der CSV-Datei. Der einfachste
                Weg dieses Problem zu lösen, ist die Datein in der UTF-8-
                Enkodierung abzuspeichern, z.B. mit Excel."),
        tags$li("Optional können Sie eine RDS-Datei mit den Einstellungen
                der letzten Stichprobenziehung laden. Dieses Feature unterliegt
                allerdings noch Fehlern, vor allem im Abschnitt 'Kategorien'.")
      ),

      h3("2. Datenansicht"),
      h4("Dashboard zur Datenübersicht und Visualisierung"),
      p("Dieser Abschnitt bietet ein Dashboard für die Datenvisualisierung und
        -exploration mithilfe von für interaktiven Plots univariate
        (eine Variable) und bivariate (zwei Variablen im Zusammenhang)
        Datenverteilungen. Nutzer*innen können Variablen zur
        Visualisierung auswählen, Filter anwenden und
        Zusammenfassungsstatistiken der Daten einsehen."),
      tags$ul(
        tags$li("Navigieren Sie zu den Reitern im Seitenpanel, um Haupt- und
                Gruppierungsmerkmale auszuwählen."),
        tags$li("Wählen Sie ein 'Hauptmerkmal' aus, um dessen Verteilung zu
                analysieren."),
        tags$li("Optional: Wählen Sie ein 'Gruppierungsmerkmal', um
                bivariate Analysen durchzuführen."),
        tags$li("Verwenden Sie die integrierten Filteroptionen für
                ausgewählte Variablen, um die Ansicht und die Analyse auf
                spezifische Datenpunkte zu beschränken."),
        tags$li(paste("Sichtbare Rückmeldungen (Warnungen) werden angezeigt,
                      wenn Variablen mehr als", MAX_VAL, " kategorialen Werten
                      haben. Durch einen Klick auf fortfahren, werden Anzeigen
                      und Analysen trotzdem ausgeführt. Das kann allerdings
                      Performanzeinbußen nach sich ziehen."))
      ),
      p("Diagramme und Zusammenfassungen:"),
      tags$ul(
        tags$li("Schauen Sie sich das Verteilungsdiagramm für das
                ausgewählte 'Hauptmerkmal' im Hauptpanel an. Nutzen Sie den
                'Log-Skala'-Schalter, um die Logarithmierung der y-Achse zu
                aktivieren. Dies ist hilfreich, wenn eine Merkmalsausprägung
                sich so sehr von den anderen unterscheidet, dass kleinere
                Unterschiede kaum noch erkennbar sind."),
        tags$li("Das bivariate Diagramm darunter ermöglicht die Erkundung von
                Beziehungen zwischen dem 'Hauptmerkmal' (x-Achse) und dem
                'Gruppierungsmerkmal' (y-Achse)."),
        tags$li("Die zusammenfassende Tabelle zeigt Statistiken des
                ausgewählten Hauptmerkmals."),
        tags$li("Durch die Filteroption des Gruppierungsmerkmals, hat man
                die Möglichkeit sich Verteilungen über nur einen Teil des
                Datensatzes zu erschließen.")
      ),

      h3("3. Datenfilterung"),
      h4("Bestimmung der Grundgesamtheit"),
      p("Dieser Abschnitt dient der interaktiven Auswahl und Filterung von
          Daten. Nutzer*innen können Merkmale auswählen, dazugehörige Filter
          anwenden und erhalten eine Übersicht über Datenverteilungen und
          Zusammenfassungsstatistiken."),
      tags$ul(
        tags$li("Wählen Sie ein Merkmal aus der Dropdown-Liste im Seitenpanel
                aus, um damit zu arbeiten."),
        tags$li("Legen Sie Filter für die Merkmale fest. Für numerische
                Merkmale können Sie einen Wertebereich definieren, während
                für kategoriale Merkmale spezifische Werte ausgewählt werden
                können."),
        tags$li("Klicken Sie auf 'Filter anwenden', um die gesetzten Filter
                auf die Daten anzuwenden. Die gefilterten Daten werden sofort
                aktualisiert."),
        tags$li("Eine Übersicht über die aktuell angewandten Filter wird im
                'Aktuelle Filter'-Abschnitt angezeigt. Einzelne Filter können
                durch die entsprechenden Schaltflächen entfernt werden."),
        tags$li("Alle Filter in dieser Übersich werde konjunktiv, also per UND
                verknüpft. Das bedeutet, dass die Daten nur angezeigt werden,
                wenn alle Filter zutreffen."),
        tags$li("Verwenden Sie 'Alle Filter zurücksetzen', um alle Filter zu
                entfernen und zur Ausgangsdatenansicht zurückzukehren.")
      ),
      p("ODER-Klauseln als Sonderfall:"),
      tags$ul(
        tags$li("Es ist auch möglich Filter nicht konjunktiv (UND), sondern
                disjunktiv annzuwenden. Dafür können Sie mehrere Filter zu einer
                ODER-Klausel hinzufügen. Die Filter werden dann auf die Daten
                angewendet, wenn mindestens einer der Filter zutrifft."),
        tags$li("Klicken Sie dafür auf 'Filter zu ODER-Klausel hinzufügen'.
                Die gefilterten Daten werden dabei NICHT sofort aktualisiert."),
        tags$li("Dabei öffnet sich eine zuvor versteckte Übersicht
                ('ODER-Klausel'), in der Sie die Filter sehen und bearbeiten
                können. Einzelne Filter können durch die entsprechenden
                Schaltflächen entfernt werden."),
        tags$li("Geben Sie Ihrer Klausel einen sinnvolle wiedererkennbare
                Bezeichnung. Diese wird auch in den Stichprobeneinstellungen
                gespeichert."),
        tags$li("Verwenden Sie 'ODER-Klausel als Filter anwenden', um die
                ODER-Klausel zur Filteransicht hinzuzufügen. Die gefilterten
                Daten werden dabei sofort aktualisiert und die ODER-Klausel-
                Übersicht wird wieder versteckt."),
        tags$li("Verwenden Sie 'ODER-Klausel zurücksetzen', um alle Filter aus
                der ODER-Klausel zu entfernen und zur vorherigen Ansicht
                zurückzukehren. Die ODER-Klausel-Übersicht wird dabei wieder
                versteckt."),
        tags$li("BUG: Aktuell wird der erste Filter der yu einer ODER-Klausel
                hinzugefügt wird, nicht korrekt angezeigt. Dieser wird jedoch
                korrekt angewendet. Ab dem zweiten Filter funktioniert die
                Anzeige korrekt.")
      ),
      p("Merkmalsverteilung und Zusatzfunktionen:"),
      tags$ul(
        tags$li("Betrachten Sie die Merkmalsverteilung im Hauptbereich. Mit
                dem 'Log-Skala'-Schalter lässt sich die y-Achse
                logarithmieren, um die Ansicht zu verbessern (s. 2.
                Datenansicht)."),
        tags$li("Die zusammengefassten Statistiken basieren auf den aktuell
                gespeicherten Filtern und dem ausgewählten Filter der gerade
                bearbeitet wird."),
        tags$li(paste("Eine Warnmeldung erscheint, falls die Anzahl der
                      kategorialen Werte für ein Merkmal", MAX_VAL,
                      "überschreitet."))
      ),

      h3("4. Kategorien"),
      h4("Erstellung von Kategorisierung für die Stichprobenziehung"),
      p("Dieses Modul ermöglicht die Auswahl von Variablen und die Erstellung
        von Kategorien, für welche später Parameter zur Stichprobenziehung
        festgelegt werden können. Nutzer*innen können Schichten zur
        Stratifizierung konfigurieren, hinzufügen und entfernen.
        Diese Schichten ermöglichen es, Daten basierend auf definierten
        Parametern zu kategorisieren und die Ergebnisse in einer Kreuztabelle
        anzuzeigen."),
      tags$ul(
        tags$li("Klicken Sie auf die Schaltfläche 'Schicht hinzufügen', um
                eine neue Schicht zur Stratifizierung hinzuzufügen."),
        tags$li("Konfigurieren Sie jede Schicht, indem Sie entsprechende
                UI-Anpassungen vornehmen, um ihre Parameter genau
                festzulegen."),
        tags$li("Entfernen Sie eine Schicht über die bereitgestellte
                Entfernungsschaltfläche, falls sie nicht mehr benötigt
                wird."),
        tags$li("Sehen Sie sich die dynamische Kreuztabelle an, die je zwei
                Merkmale anhand ihrer Kategorien ins Verhältnis setzt, indem
                Sie unter '1. Spalte auswählen' und '2. Spalte auswählen' aus
                den verfügbaren Stratifikationsschichten wählen.")
      ),
      p("Zusätzliche Hinweise:"),
      tags$ul(
        tags$li("Nutzer können die erstellten Schichten durch die generierten
                Schaltflächen nahtlos durchstöbern. Um eine Schicht
                übersichtlich anzupassen sollte sie jedoch zunächst gelöscht
                und dann neu erstellt und angewendet werden."),
        tags$li("Fügen Sie notwendige Schichten hinzu oder entfernen Sie
                überflüssige Schichten, um die datengestützte Analyse zu
                optimieren."),
        tags$li("Die Kreuztabelle auf der rechten Seite bietet eine leichtere
                Visualisierung des Zusammenhangs zwischen verschiedenen
                kategorialen Daten."),
        tags$li("Numerische Merkmale werden anhand von Intervallen zu
                Kategorien zusammengefasst. Dafür können die Grenzwerte in
                Textfeldern festgelegt werden."),
        tags$li("Bei kategorialen Merkmale Können per 'Drag and Drop'
                Merkmalsausprägungen den Überkategorien zugeordnet werden.
                Es wird empfohlen die Kategorien zu benennen, um die Übersicht
                zu wahren."),
        tags$li("Beachten Sie, dass jede Spalte nur in einer Schichtungsebene
                verwendet werden kann."),
        tags$li("Sollten Sie keine Schicht definieren, wird in den nöchsten
                Schritten eine Zufallsstichprobe gezogen.")
      ),

      h3("5. Stichprobe"),
      h4("Bestimmung der Parameter zur Stichprobenziehung"),
      p("Dieser Abschnitt ergmöglicht eine stratifizierte Stichprobenziehung.
        Nutzer*innen können die Stichprobengröße, sowie
        Auswahlwahrscheinlichkeiten, bzw. Stichprobenanteile für einzelne
        zuvor definierten Kategorien festlegen und Stichprobenparameter in
        Bezug auf Stratifizierungskriterien berechnen."),
      tags$ul(
        tags$li("Geben Sie die gewünschte Stichprobengröße im Seitenpanel
                ein."),
        tags$li("Verwenden Sie die Reiter unter 'Wahrscheinlichkeitsart', um
                Wahrscheinlichkeiten pro Kategorie entweder als Verhältnis
                zur Gesamtstichprobe oder als Anteil innerhalb jeder Kategorie
                festzulegen."),
        tags$li("Nutzen Sie den Schieberegler, um die Anteile der
                Endstichprobe, bzw. Auswahlwahrscheinlichkeiten anzupassen."),
        tags$li("Klicken Sie auf 'Proportional zum Datensatz', um
                Wahrscheinlichkeiten proportional zur Verteilung im Datensatz
                zu setzen. Je nach ausgewählter Wahrscheinlichkeitsart,
                springen die Regler nun in die Position, in der sie einen
                proportionalen Stichprobenanteil produzieren würden."),
        tags$li("Drücken Sie die Schaltfläche 'Stichprobe generieren', um die
                Berechnung basierend auf den festgelegten Parametern
                durchzuführen. Dies können Sie beliebig wiederholen, bis Ihnen
                das Ergebnis passt."),
        tags$li("Die resultierende Zusammenfassung der Stichprobe wird im
                Hauptbereich (rechts) angezeigt.")
      ),
      p("Wichtige Hinweise:"),
      tags$ul(
        tags$li("Ein Warnhinweis wird angezeigt, wenn die Summe der
                Stichprobenanteile nicht 1 beträgt. In einem solchen Fall
                werden die Anteile standardisiert, um das Verhältnis zwischen
                den Kategorien zu bewahren."),
        tags$li("Bei prozentual eingestellter Auswahlwahrscheinlichkeit und
                zu hoher benötigter Stichprobengröße wird ein Hinweis
                eingeblendet, dass diese nicht eingehalten werden kann. In
                diesem Fall bleibt jedes gezogene Exemplar Teil der
                Kategorie, Allerdings wird nicht jedes Exemplar der Kategorie
                auch gezogen."),
        tags$li("Die tatsächliche Stichprobenziehung passiert erst im nächsten
                Schritt.In diesem Abschnitt werden lediglich die
                Stichprobenanteile einzelner Strata (Kategorienkombinationen)
                berechnet"),
        tags$li("Sollten im Abschnitt zuvor keine Schichten definiert worden
                sein, kann in diesem Abschnitt nur die Stichprobengröße als
                Parameter definiert werden. Klicken Sie trotzdem auf
                'Stichprobe generieren', sodass Ihnen eine einreihige Tablle
                angezeigt wird.")
      ),

      h3("6. Überblick"),
      h4("Finale Stichprobenziehung und Download der Stichprobe und
          Dokumentation"),
      p("Dieser Bereich dient dazu den finalen Stichprobenprozess auszulösen,
        einen PDF-Bericht, die Stichprobenparameter als RDS-Datei, sowie die
        Stichprobe in unterschiedlichen Formaten als CSV herunterzuladen."),
      tags$ul(
        tags$li("Autor- und Datensatz-ID können eingegeben werden, um denn
                DF-Bericht korrekt zu kennzeichnen."),
        tags$li("Verschiedene Schaltflächen stehen zur Verfügung, um Berichte
                und Datenproben im PDF-, CSV- oder RDS-Format
                herunterzuladen."),
        tags$li("Die Schaltfläche 'Stichprobe ziehen' startet den endgültigen
                Stichprobenprozess, indem die aktuellen Einstellungen
                angewendet werden."),
        tags$li("Eine Tabelle zeigt die Stichprobendaten an, die auf der
                Grundlage Ihrer Auswahl bereitgestellt werden.")
      ),
      p("Verfügbare Funktionen:"),
      tags$ul(
        tags$li("Laden Sie die Dokumentation im PDF-Format herunter, indem Sie
                auf 'Dokumentation als PDF laden' klicken."),
        tags$li("Laden Sie die Stichprobe als CSV herunter, indem Sie die
                entsprechende Schaltfläche verwenden."),
        tags$li("Laden Sie den markierten Datensatz als CSV herunter, um den
                gesamten Datensatz mit einer Extraspalte zu erhalten, welche
                mit 'ja' und 'nein' anzeigt ob ein Exemplar in der Stichprobe
                vertreten ist."),
        tags$li("Laden Sie detaillierte Stichprobeninformationen im RDS-Format
                herunter, die Angaben über Spalten, Filter, Strata und
                Stichprobengröße enthalten.")
      ),
      hr(),
      hr(),
      h1("English Version"),
      h2("Instructions for Use"),
      tags$ul(
        tags$li("Clicking the 'Next' button will take you to the next section,
                provided you have made sufficient progress in data
                processing."),
        tags$li("When you reach the last section ('Overview'), you can exit the
                app by clicking the 'Schließen' button. Alternatively, you can 
                close the browser tab at any time to exit the program; however,
                your progress WILL NOT be saved!"),
        tags$li("Clicking the three bars in the green header displays the menu.
                Here, you can navigate between sections and access this guide.")
      ),

      h2("Overview of the App Structure"),
      p("The ADA Bayern Sampling App is a Shiny application that assists
        archivists in processing legal datasets. The app provides functionality
        for uploading, exploring, filtering, and stratifying data to perform
        informed sampling. Navigation is facilitated through a dashboard divided
        into multiple sections, each addressing specific tasks. You are guided
        step-by-step through the different phases of the process using the
        'Weiter' button."),

      h3("1. Upload"),
      h4("Upload and Manage CSV Data"),
      p("This section allows users to upload and manage CSV files. Users can
        upload files, configure import options, and preview data before analysis
        or processing."),
      tags$ul(
        tags$li("Navigate to the 'Data Upload' tab."),
        tags$li("Select your CSV file and upload it using the
                'CSV-Datei auswählen' button."),
        tags$li("Use the options field to specify whether the first row of the
                file should be used as the header."),
        tags$li("Choose the appropriate delimiter from comma, semicolon, or
                tab."),
        tags$li("Choose the appropriate decimal point.")
      ),
      p("Data Preview and Additional Notes:"),
      tags$ul(
        tags$li("After selecting a file, a preview of the data is displayed. Use
                this to ensure the data is correct."),
        tags$li("By default, the preview shows the first five rows of the
                file."),
        tags$li("In the appearing 'Column Selection' area, you can adjust the
                data type of individual columns or remove the column. Only
                numeric columns can be converted to categorical, meaning each
                number is assigned a category. This should generally only be
                done if the order of the values is not relevant and the number
                of values is manageable."),
        tags$li("Click 'Auswahl hochladen' to save the selected columns. The
                complete data is now available for further analysis. The data
                preview will show a change in the number of rows."),
        tags$li("Ensure the file size does not exceed the 500 MB limit and that
                the format is correct for successful processing.")
      ),

      h3("2. Data View"),
      h4("Dashboard for Data Overview and Visualization"),
      p("This section provides a dashboard for data visualization and
        exploration, using interactive plots for univariate (one variable) and
        bivariate (two variables) distributions. Users can select variables to
        visualize, apply filters, and view summary statistics."),
      tags$ul(
        tags$li("Navigate through the side panel tabs to select main and
                grouping features."),
        tags$li("Select a 'Hauptmerkmal' to analyze its distribution."),
        tags$li("Optional: Select a 'Gruppierungsmerkmal' for bivariate
                analysis."),
        tags$li("Use the integrated filter options for selected variables to
                focus the view and analysis on specific data points."),
        tags$li(paste("Visible feedback (warnings) is displayed if variables
                      have more than", MAX_VAL, "categorical values. Click
                      'Fortfahren' to continue the display and analysis, though
                      it may impact performance."))
      ),
      p("Charts and Summaries:"),
      tags$ul(
        tags$li("View the distribution chart for the selected 'Hauptmerkmal' in
                the main panel. Use the 'Log Scale' switch to enable logarithmic
                scaling of the y-axis, helpful for highlighting small
                differences when one feature value dominates."),
        tags$li("The bivariate chart below allows exploration of relationships
                between the 'Main Feature' (x-axis) and the
                'Gruppierungsmerkmal' (y-axis)."),
        tags$li("The summary table shows statistics for the selected main
                feature."),
        tags$li("Filter options for the grouping feature allow users to explore
                distributions for subsets of the data.")
      ),

      h3("3. Data Filtering"),
      h4("Defining the Population"),
      p("This section enables interactive data selection and filtering. Users
        can choose features, apply filters, and view distributions and summary
        statistics."),
      tags$ul(
        tags$li("Select a feature from the dropdown list in the side panel."),
        tags$li("Set filters for the selected feature. For numeric features,
                define a value range, while categorical features allow specific
                value selection."),
        tags$li("Click 'Apply Filters' to apply the filters to the data. The
        filtered data updates immediately."),
        tags$li("An overview of the applied filters is displayed in the
                'Aktuelle Filter' section. Individual filters can be removed
                using their respective buttons."),
        tags$li("Use 'Alle Filter zurücksetzen' to remove all filters and return
                to the original data view.")
      ),
      p("OR Clauses as a Special Case:"),
      tags$ul(
        tags$li("It is also possible to apply filters disjunctively (OR) instead
                of conjunctively (AND). You can add multiple filters to an OR
                clause. The filters are then applied to the data if at least one
                of the filters is met."),
        tags$li("Click on 'Add Filter to OR Clause'. The filtered data will NOT
                be updated immediately."),
        tags$li("A previously hidden overview ('OR Clause') will open, where you
                can view and edit the filters. Individual filters can be removed
                using the corresponding buttons."),
        tags$li("Give your clause a meaningful and recognizable name. This will
                also be saved in the sampling settings."),
        tags$li("Use 'Apply OR Clause as Filter' to add the OR clause to the
                filter view. The filtered data will be updated immediately, and
                the OR clause overview will be hidden again."),
        tags$li("Use 'Reset OR Clause' to remove all filters from the OR clause
                and return to the previous view. The OR clause overview will be
                hidden again."),
        tags$li("BUG: Currently, the first filter added to an OR clause is not
                displayed correctly. However, it is applied correctly. From the
                second filter onwards, the display works correctly.")
      ),
      p("Feature Distribution and Additional Functions:"),
      tags$ul(
        tags$li("Examine the feature distribution in the main area. Use the
                'Log-Skala' switch to logarithmically scale the y-axis for
                better visualization."),
        tags$li("The summary statistics reflect the currently applied filters
                and the selected filter being adjusted."),
        tags$li(paste("A warning is displayed if the number of categorical
                      values for a feature exceeds", MAX_VAL, "."))
      ),

      h3("4. Categories"),
      h4("Creating Categories for Sampling"),
      p("This module allows users to select variables and create categories
        for which sampling parameters can later be defined. Users can configure,
        add, and remove stratification layers, categorizing data based on
        defined parameters, with results displayed in a crosstab."),
      tags$ul(
        tags$li("Click 'Schicht hinzufügen' to add a new stratification
                layer."),
        tags$li("Configure each layer using the provided UI adjustments to
                specify parameters."),
        tags$li("Remove a layer using the delete button if it is no longer
                needed."),
        tags$li("View the dynamic crosstab that relates two features by
                selecting '1. Spalte' and '2. Spalte' from the available
                stratification layers.")
      ),
      p("Additional Notes:"),
      tags$ul(
        tags$li("Users can navigate between created layers using generated
                buttons. To clearly adjust a layer, it is recommended to delete
                and recreate it."),
        tags$li("Add necessary layers or remove superfluous layers to optimize
                data analysis."),
        tags$li("The crosstab on the right provides easier visualization of
                relationships between categorical data."),
        tags$li("Numeric features are categorized into intervals with
                configurable bounds."),
        tags$li("For categorical features, assign feature values to categories
                using 'Drag and Drop.' Naming categories is recommended for
                clarity."),
        tags$li("Each column can only be used in one stratification level."),
        tags$li("If no layers are defined, a random sample will be drawn in the
                next steps.")
      ),

      h3("5. Sampling"),
      h4("Defining Sampling Parameters"),
      p("This section enables stratified sampling. Users can specify sample
        size, probabilities, or proportions for predefined categories and
        calculate sampling parameters based on stratification criteria."),
      tags$ul(
        tags$li("Enter the desired sample size in the side panel."),
        tags$li("Use the tabs under 'Wahrscheinlichkeitsart' to specify
                probabilities per category as either a ratio to the total
                sample or a proportion within each category."),
        tags$li("Use the slider to adjust sample proportions or
                probabilities."),
        tags$li("Click 'Proportional zum Datensatz' to set probabilities
                proportional to the data distribution. Depending on the selected
                probability type, sliders adjust accordingly to produce
                proportional sampling."),
        tags$li("Press 'Stichprobe generieren' to calculate based on the defined
                parameters. Repeat as needed until satisfied."),
        tags$li("The summary of the resulting sample is displayed in the main
                area (right).")
      ),
      p("Important Notes:"),
      tags$ul(
        tags$li("A warning appears if the sum of sample proportions is not 1. In
                such cases, proportions are standardized to maintain category
                relationships."),
        tags$li("If the percentage-based selection probability is too high, an
                alert is displayed. In this case, not all instances within a
                category may be included."),
        tags$li("The actual sampling occurs in the next step. This section
                calculates category proportions."),
        tags$li("If no layers were defined in the previous step, only the sample
                size can be defined. Click 'Generate Sample' to view a single-
                row table.")
      ),

      h3("6. Overview"),
      h4("Final Sampling and Downloading Sample and Documentation"),
      p("This section triggers the final sampling process and allows downloading
        the sample parameters and sample data in various formats."),
      tags$ul(
        tags$li("Enter author and dataset ID to correctly label the PDF
                report."),
        tags$li("Buttons are available to download reports and samples in PDF,
                CSV, or RDS format."),
        tags$li("Click 'Generate Sample' to finalize sampling using current
                settings."),
        tags$li("A table shows the sampled data based on your selections.")
      ),
      p("Available Functions:"),
      tags$ul(
        tags$li("Download documentation as a PDF by clicking
                'Dokumentation als PDF laden'"),
        tags$li("Download the sample as a CSV file using the corresponding
                button."),
        tags$li("Download the marked dataset as a CSV file, including an extra
                column indicating whether an instance is part of the sample
                ('ja' or 'nein')."),
        tags$li("Download detailed sample information as an RDS file, including
                column, filter, strata, and sample size details.")
      )
    ))
  )
}

#' Server-Funktion für die Bedienungsanleitung (aktuell keine Logik benötigt)
manual_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    # Nothing
  })
}
