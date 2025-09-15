INTRO_OPTIONS <- list(
  "showBullets" = FALSE,
  "showProgress" = TRUE,
  "showStepNumbers" = FALSE,
  "nextLabel" = "Weiter",
  "prevLabel" = "Zurück",
  "skipLabel" = "Überspringen",
  "doneLabel" = "Fertig",
  "exitOnOverlayClick" = TRUE,
  "exitOnEsc" = TRUE
)

MANUAL <- list(
  "start" = c(
    "Übersicht der App-Struktur:\nDie ADA Bayern Stichprobenziehung-App ist eine Shiny-Anwendung, die Archivare bei der Bearbeitung juristischer Datensätze unterstützt. Die App bietet Funktionen zum Hochladen, Erkunden, Filtern und Stratifizieren von Daten, um eine fundierte Stichprobenziehung durchzuführen.",
    "Die Benutzerführung erfolgt über ein Dashboard mit mehreren Abschnitten, die jeweils spezifische Aufgaben abdecken. Mit Hilfe des „Weiter“-Buttons werden Sie Schritt für Schritt durch die verschiedenen Phasen des Prozesses geführt.",
    "Allgemeine Hinweise:\nEin Klick auf die 'Weiter'-Schaltfläche bringt Sie zum nächsten Abschnitt, sofern, Sie in der Datenbearbeitung so weit sind.",
    "Allgemeine Hinweise:\nWenn Sie im letzten Abschnitt ('Überblick') angelangt sind, können Sie mit einem Klick auf die Schaltfläche 'Schließen' die App beenden. Außerdem können Sie jederzeit den Browser-Tab schließen, um das Programm zu beenden, allerdings werden Ihre Fortschritte NICHT gespeichert!",
    "Allgemeine Hinweise:\nMit einem Klick auf die drei Balken in der grünen Kopfzeile wird das Menü angezeigt. Hier können Sie zwischen den einzelnen Abschnitten hin- und hernavigieren",
    "CSV-Daten hochladen und verwalten:\nDieser Abschnitt ermöglicht das Hochladen und Verwalten von CSV-Dateien. Nutzer*innen können Dateien hochladen, Importoptionen konfigurieren und die Daten vor der Analyse oder Verarbeitung in der Anwendung prüfen.",
    "Wählen Sie eine CSV-Datei aus, die Sie hochladen möchten. Stellen Sie sicher, dass die Dateigröße das Limit von 500 MB nicht überschreitet und dass das Format korrekt ist, um erfolgreiche Datenverarbeitung zu gewährleisten.",
    "Hier können Sie die Optionen für den CSV-Import anpassen. Wählen Sie den Separator, ob die erste Zeile Kopfzeilen enthält und das Dezimaltrennzeichen.",
    "Wählen Sie den Typ jeder Spalte aus. Kategorische Spalten werden als solche behandelt, numerische Spalten als Zahlen. Spalten, die nicht benötigt werden, können entfernt werden. Klicken Sie auf 'Auswahl hochladen', um die ausgewählten Spalten zu laden und die Einstellungen zu speichern. Danach wird der komplette Datensatz geladen und angezeigt.",
    "Optional können Sie eine RDS-Datei mit Stichprobeneinstellungen hochladen, um die Spaltenauswahl und Filtereinstellungen zu übernehmen. Diese Datei kann aus einer vorherigen Sitzung stammen. Dieses Feature unterliegt allerdings noch Fehlern, vor allem im Abschnitt 'Kategorien'.",
    "Hier sehen Sie eine Vorschau der hochgeladenen Daten. Zunächst sehen Sie hier nur 5 Zeilen als Übersicht. Überprüfen Sie, ob die Spalten korrekt erkannt wurden und ob die Daten in Ordnung sind. Wenn Sie Probleme mit Sonderzeichen sehen, liegt dies vermutlich an der Enkodierung der CSV-Datei. Der einfachste Weg dieses Problem zu lösen, ist die Datein in der UTF-8-Enkodierung abzuspeichern, z.B. mit Excel."
  ),
  "dashboard" = c(
    "Dashboard zur Datenübersicht und Visualisierung:\nDieser Abschnitt bietet ein Dashboard für die Datenvisualisierung und -exploration mithilfe von für interaktiven Plots für univariate (eine Variable) und bivariate (zwei Variablen im Zusammenhang) Datenverteilungen. Nutzer*innen können Variablen zur Visualisierung auswählen, Filter anwenden und Zusammenfassungsstatistiken der Daten einsehen.",
    "Wählen Sie das Hauptmerkmal (x-Achse) für die Analyse aus. Dies ist die Variable, deren Verteilung Sie untersuchen möchten.",
    "Wählen Sie ein Gruppierungsmerkmal (y-Achse) für die Analyse aus. Dies ist die Variable, nach der die Verteilung des Hauptmerkmals gruppiert wird.",
    "Hier können Sie Filter auf das Gruppierungsmerkmal anwenden. Je nach Typ des Gruppierungsmerkmals (kategorisch oder numerisch) werden unterschiedliche Filteroptionen angezeigt. Durch die Filteroption des Gruppierungsmerkmals, hat man die Möglichkeit sich Verteilungen über nur einen Teil des Datensatzes zu erschließen.",
    "Hier sehen Sie eine Zusammenfassung der Kennwerte des Hauptmerkmals. Diese Tabelle wird aktualisiert, wenn Sie das Hauptmerkmal oder die Filter ändern.",
    "Hier sehen Sie die Verteilung des Hauptmerkmals. Nutzen Sie den 'Log-Skala'-Schalter, um die Logarithmierung der y-Achse zu aktivieren. Dies ist hilfreich, wenn eine Merkmalsausprägung sich so sehr von den anderen unterscheidet, dass kleinere Unterschiede kaum noch erkennbar sind.",
    "Hier sehen Sie die bivariate Verteilung (mit 2 Variablen) des Hauptmerkmals in Bezug auf das Gruppierungsmerkmal. Dies hilft, Zusammenhänge und Muster zwischen den beiden Merkmalen zu erkennen."
  ),
  "filter" = c(
    "Bestimmung der Grundgesamtheit:\nDieser Abschnitt dient der interaktiven Auswahl und Filterung von Daten. Nutzer*innen können Merkmale auswählen, dazugehörige Filter anwenden und erhalten eine Übersicht über Datenverteilungen und Zusammenfassungsstatistiken der gefilterten Daten.",
    "Wählen Sie ein Merkmal aus, um Filter anzuwenden.",
    "Wählen Sie Filter für das ausgewählte Merkmal aus.",
    "Wenden Sie den Filter an oder fügen Sie ihn zu einer ODER-Klausel hinzu. Sie können alle Filter zurücksetzen.",
    "Hier sehen Sie die aktuell angewendeten Filter. Sie können diese entfernen, indem Sie auf den entsprechenden Button klicken. Alle Filter in dieser Übersicht werden konjunktiv, also per UND verknüpft. Das bedeutet, dass die Daten nur angezeigt werden, wenn alle Filter zutreffen.",
    "ODER-Klauseln:\nEs ist auch möglich Filter nicht konjunktiv (UND), sondern disjunktiv annzuwenden. Dafür können Sie mehrere Filter zu einer ODER-Klausel hinzufügen. Die Filter werden dann auf die Daten angewendet, wenn mindestens einer der Filter zutrifft.",
    "ODER-Klauseln:\nKlicken Sie dafür auf 'Filter zu ODER-Klausel hinzufügen'. Die gefilterten Daten werden dabei NICHT sofort aktualisiert. Dabei öffnet sich eine zuvor versteckte Übersicht ('ODER-Klausel'), in der Sie die Filter sehen und bearbeiten können. Einzelne Filter können durch die entsprechenden Schaltflächen entfernt werden.",
    "ODER-Klauseln:\nGeben Sie Ihrer Klausel einen sinnvolle wiedererkennbare Bezeichnung. Diese wird auch in den Stichprobeneinstellungen gespeichert. Verwenden Sie 'ODER-Klausel als Filter anwenden', um die ODER-Klausel zur Filteransicht hinzuzufügen. Die gefilterten Daten werden dabei sofort aktualisiert und die ODER-Klausel-Übersicht wird wieder versteckt. Verwenden Sie 'ODER-Klausel zurücksetzen', um alle Filter aus der ODER-Klausel zu entfernen.",
    "Hier sehen Sie die Kennwerte des ausgewählten Merkmals.",
    "Hier sehen Sie die Verteilung des ausgewählten Merkmals. Sie können die Log-Skala aktivieren, um kleine Unterschiede in der Verteilung besser zu visualisieren."
  ),
  "categories" = c(
    "Erstellung von Kategorisierung für die Stichprobenziehung:\nDieses Modul ermöglicht die Auswahl von Variablen und die Erstellung von Kategorien, für welche später Parameter zur Stichprobenziehung festgelegt werden können. Nutzer*innen können Schichten zur Stratifizierung konfigurieren, hinzufügen und entfernen.",
    "Fügen Sie eine neue Schicht hinzu, um Variablen zu kategorisieren. Jede Schicht kann unterschiedliche Parameter und Kategorien haben. Später können Sie die Schichten anpassen oder entfernen.\nFür jede definierte Kategorie können Sie im nächsten Schritt bestimmen, wie groß ihr Anteil an der Stichprobe sein soll oder zu welchem Anteil sie in die Stichprobe übernommen werden soll.",
    "Hier sehen Sie die definierten Schichten. Klicken Sie auf die Schichtnamen, um die Einstellungen zu ändern. Sie können auch Schichten entfernen, indem Sie auf den entsprechende Knopf klicken.",
    "Wählen Sie zwei Spalten aus, um eine Kreuztabelle zu erstellen. Die Kreuztabelle zeigt die Häufigkeit der Kombinationen der ausgewählten Spalten an.",
    "Nutzer können die erstellten Schichten durch die generierten Schaltflächen nahtlos durchstöbern. Um eine Schicht übersichtlich anzupassen sollte sie jedoch zunächst gelöscht und dann neu erstellt und angewendet werden.",
    "Numerische Merkmale werden anhand von Intervallen zu Kategorien zusammengefasst. Dafür können die Grenzwerte in Textfeldern festgelegt werden.",
    "Bei kategorialen Merkmale Können per 'Drag and Drop' Merkmalsausprägungen den Überkategorien zugeordnet werden. Es wird empfohlen die Kategorien zu benennen, um die Übersicht zu wahren.",
    "Beachten Sie, dass jede Spalte nur in einer Schichtungsebene verwendet werden kann.",
    "Sollten Sie keine Schicht definieren, wird in den nächsten Schritten eine Zufallsstichprobe gezogen."
  ),
  "sample" = c(
    "Bestimmung der Parameter zur Stichprobenziehung:\nDieser Abschnitt ergmöglicht eine stratifizierte Stichprobenziehung. Nutzer*innen können die Stichprobengröße, sowie Auswahlwahrscheinlichkeiten, bzw. Stichprobenanteile für einzelne zuvor definierten Kategorien festlegen und Stichprobenparameter in Bezug auf Stratifizierungskriterien berechnen.",
    "Hier können Sie die Größe der Stichprobe festlegen. Die Größe der Grundgesamtheit wird automatisch aus den hochgeladenen Daten ermittelt und hier angezeigt.",
    "Hier können Sie für jede Schicht festlegen, wie groß der Anteil der einzelnen Kategorien an der Stichprobe sein soll. Dafür können Sie entweder angeben, zu wie viel Prozent diese Kategorie in die Stichprobe übernommen werden soll (Auswahlwahrscheinlichkeit), oder wie groß der Anteil der Kategorie an der Stichprobe sein soll (Stichprobenanteil).",
    "Mit diesem Button setzen Sie einen Algorithmus zur Ziehung der Stichprobe in Gang. Dieser Algorithmus berücksichtigt die definierten Schichten und die festgelegten Anteile der Kategorien. Wenn viele Kategorien (>50) definiert sind, kann die Ziehung der Stichprobe einige Zeit in Anspruch nehmen und das Programm evtl. zum Absturz bringen.",
    "Hier können Sie einstellen, wie groß jedes einzelne Stratum mindestens in der Stichprobe sein soll.",
    "Hier können Sie den einstellen, welche Garantie Sie bei der Stichprobenziehung haben wollen: Garantierte Kategorienanteile, garantierte Stichprobengröße oder ein naives Verfahren ohne Garantie. Es kann manchmal sein, dass eine bestimmte Garantie nicht zu einem Ergebnis führen kann.",
    "Nehmen Sie das naive Verfahren, wenn Sie eine komplexe Stichprobe mit vielen Schichten (ca. über 20) ziehen wollen. Dieses Verfahren ist in der Lage auch bei vielen Schichten in kurzer Zeit eine Stichprobe zu ziehen. Es werden keine Stichprobenanteile garantiert, aber die gezogene Stichprobe wird in etwa die festgelegten Anteile widerspiegeln. Bei wenigen Schichten (ca. unter 20) können Sie eine der beiden Garantien wählen.",
    "Hier sehen Sie die realisierte Stichprobengröße. Diese kann von der ursprünglich festgelegten Größe abweichen, wenn nicht 'Garantierte Stichprobengröße' genutzt wurde.",
    "Hier sehen Sie die Stichprobenanteile, wie sie im nachsten Schritt gezogen werden. Überprüfen Sie, ob die Stichprobe korrekt gezogen wurde und ob sie den Erwartungen entspricht. Sie können die Stichprobe jederzeit neu ziehen. Außerdem können Sie einzelne Werte in der Spalte 'Größe Stichprobe' manuell anpassen.",
    "Einstellung Stichprobenanteil:\nEin Warnhinweis wird angezeigt, wenn die Summe der Stichprobenanteile nicht 1 beträgt. In einem solchen Fall werden die Anteile standardisiert, um das Verhältnis zwischen den Kategorien zu bewahren.",
    "Einstellung Auswahlwahrscheinlichkeit:\nIst die Auswahlwahrscheinlichkeit für eine Kategorie so hoch, dass die Stichprobengröße nicht erreicht werden kann, wird ebenfalls ein Hinweis eingeblendet. In diesem Fall bleibt jedes gezogene Exemplar Teil der Kategorie, allerdings wird nicht jedes Exemplar der Kategorie auch gezogen.",
    "Die tatsächliche Stichprobenziehung passiert erst im nächsten Schritt. In diesem Abschnitt werden lediglich die Stichprobenanteile einzelner Strata (Kategorienkombinationen) berechnet",
    "Sollten im Abschnitt zuvor keine Schichten definiert worden sein, kann in diesem Abschnitt nur die Stichprobengröße als Parameter definiert werden. Klicken Sie trotzdem auf 'Stichprobe generieren', sodass Ihnen eine einreihige Tablle angezeigt wird."
  ),
  "overview" = c(
    "Finale Stichprobenziehung und Download der Stichprobe und Dokumentation:\nDieser Bereich dient dazu den finalen Stichprobenprozess auszulösen, einen HTML-Bericht, die Stichprobenparameter als RDS-Datei, sowie die Stichprobe in unterschiedlichen Formaten als CSV herunterzuladen.",
    "Geben Sie eine Author-ID und eine Datensatz-ID ein, um Ihre Einstellungen wiedererkennbar zu speichern.",
    "Hier können Sie die Dokumentation der Einstellungen als HTML speichern. Diese Dokumentation enthält alle Informationen zu den hochgeladenen Daten, den Einstellungen und der Stichprobe.",
    "Hier können Sie die Einstellungen als RDS-Datei speichern. Diese Datei kann später wieder hochgeladen werden, um die Einstellungen bei einem neuen Vorgang zu übernehmen.",
    "Hier können Sie eine Stichprobe ziehen. Klicken Sie auf den Button, um die Stichprobe zu ziehen. Dazu werden die zuvor ermittelten Häufigkeiten pro Schicht benutzt.",
    "Hier sehen Sie die gezogene Stichprobe. Diese Tabelle zeigt die Stichprobendaten an, die Sie später für Ihre Analyse verwenden können.",
    "Hier können Sie die gezogene Stichprobe als CSV-Datei speichern. Mit dem linken Button laden Sie nur die Stichprobe herunter, mit dem rechten laden Sie den gesamten Datensatz mit einer neuen Spalte 'Stichprobe' (ja/nein) herunter."
  )
)