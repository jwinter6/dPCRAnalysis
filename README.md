# dPCR Analyseplattform (R/Shiny)

Modulare Shiny-Web-App zur Analyse digitaler PCR-Daten mit Fokus auf **Qiagen QIAcuity**.
Roche Digital LightCycler und Bio-Rad QX sind als Importer-Platzhalter vorbereitet.

## Features

- Linke Sidebar-Navigation mit Seiten:
  - Start
  - Daten laden
  - Übersicht
  - Qualität
  - Sample-Analyse
  - Detailanalyse Sample
  - Report (Platzhalter)
  - Export/Import
  - Einstellungen
  - Hilfe
- QIAcuity-CSV-Import (`skip = 1`, Mapping auf internes Standardformat)
- Plausibilitäts- und Validierungschecks mit Report
- Interaktive Übersicht (DT + ggplot2 + plotly)
- Qualitätsseite mit Kennzahlen und zusätzlichen Plots
- Globale Einstellungsseite für Farbpaletten und Plot-Export
- Download-Button `Download Plot` unter jedem ggplot2-Plot
- dpcR-basierter Dichteplot (`dpcr_density`) für λ / positive Moleküle
- Sample-Analyse mit zwei unabhängigen Scatter-Workflows, Thresholds, Tabellen und optionaler Farbgebung nach Dateiname
- Detailanalyse Sample mit `twoddpcr`:
  - globalem Multi-Sample-Filter
  - `Classify`, `Results`, `Summary`
  - `K-means`, `Thresholds`, `Grid`, `K-Nearest Neighbour`
  - `heatPlot`-/`dropletPlot`-Integration
  - Trainingsdatenaufbau aus `Grid` oder vorhandener Results-Klassifikation
  - per-Cluster-Rain-Parameter (`NN`, `NP`, `PN`, `PP`)
  - CSV-/ZIP-/HTML-Export
- Analyse-Persistenz via `.RData` (Export/Import)
- Unit- und Modultests für Import, Validierung, Sample- und `twoddpcr`-Adapterlogik (`testthat`)

## Projektstruktur

```text
.
├─ app.R
├─ R/
│  ├─ app_ui.R
│  ├─ app_server.R
│  ├─ config.R
│  ├─ utils_logging.R
│  ├─ utils_validation.R
│  ├─ utils_device_detect.R
│  ├─ utils_import_export.R
│  ├─ utils_plot_defaults.R
│  ├─ utils_app_settings.R
│  ├─ data_model.R
│  ├─ utils_sample_analysis.R
│  ├─ utils_twoddpcr.R
│  ├─ importers/
│  │  ├─ importer_qiaquity.R
│  │  ├─ importer_roche.R
│  │  ├─ importer_biorad.R
│  ├─ modules/
│  │  ├─ mod_home.R
│  │  ├─ mod_data_load.R
│  │  ├─ mod_overview.R
│  │  ├─ mod_sample_analysis.R
│  │  ├─ mod_sample_detail.R
│  │  ├─ mod_quality.R
│  │  ├─ mod_report.R
│  │  ├─ mod_export_import.R
│  │  ├─ mod_settings.R
│  │  ├─ mod_help.R
├─ tests/
│  ├─ testthat.R
│  ├─ testthat/
│  │  ├─ test-app-settings.R
│  │  ├─ helper-twoddpcr.R
│  │  ├─ test-import-qiaquity.R
│  │  ├─ test-quality-metrics.R
│  │  ├─ test-sample-analysis-module.R
│  │  ├─ test-sample-analysis-utils.R
│  │  ├─ test-sample-detail-module.R
│  │  ├─ test-twoddpcr-adapter.R
│  │  ├─ test-validation.R
├─ Example_Data/
├─ www/
│  ├─ styles.css
├─ README.md
├─ Dockerfile
└─ .dockerignore
```

## Voraussetzungen (lokal)

- R >= 4.2
- Empfohlene Pakete:

```r
install.packages(c(
  "shiny", "shinydashboard", "bslib", "shinyWidgets", "DT", "plotly",
  "ggplot2", "dplyr", "tidyr", "readr", "tibble", "testthat", "dpcR",
  "BiocManager", "rmarkdown"
))
BiocManager::install("twoddpcr", ask = FALSE, update = FALSE)
```

## App starten

```r
shiny::runApp()
```

oder im Terminal:

```bash
Rscript -e "shiny::runApp()"
```

## Daten laden

1. Seite **Daten laden** öffnen
2. QIAcuity-CSV-Dateien hochladen
3. **Analyse starten** klicken
4. Validierungsreport prüfen
5. In **Übersicht** und **Qualität** interaktiv auswerten

Beispieldateien liegen in `Example_Data/` (Fallback: `exampledata/`).

## Einstellungen und Plot-Export

Die Seite **Einstellungen** verwaltet globale Plot-Standards:

- **Farbpalette**
  - Default ist die originale diskrete `ggplot2`-Standardpalette.
  - Zusätzlich stehen vordefinierte Paletten (`Okabe-Ito`, `Dark2`, `Set2`) bereit.
  - Eigene Paletten können über Namen plus Hex-Farben (`#RRGGBB`) angelegt, aktualisiert und gelöscht werden.
- **Plot-Export**
  - Exportparameter gelten für alle Buttons **`Download Plot`** unter ggplot2-Plots.
  - Defaultwerte: `5 x 5`, `96 dpi`, `png`.
  - Unterstützte Formate: `png`, `pdf`, `jpeg`.

Die Einstellungen werden zusammen mit `.RData`-Analysen exportiert und beim Laden wiederhergestellt.

## Sample-Analyse

Die Seite **Sample-Analyse** bietet zwei Scatter-Workflows (`Plot A`, `Plot B`) mit gemeinsamer globaler Farbpalette.

- Unter **Farbe nach** erscheint die Option **Dateiname**, sobald mehr als eine Quelldatei im Datensatz vorhanden ist.
- Damit lassen sich Messreihen aus mehreren CSV-Dateien direkt visuell vergleichen.
- Die gewählte Einfärbung gilt sowohl für die statischen ggplot2-Plots als auch für die daraus erzeugten Plotly-Ansichten.

## Detailanalyse Sample

Die Seite **Detailanalyse Sample** erwartet zwei auswertbare Fluoreszenzkanäle pro
Partition. Der Adapter arbeitet auf dem vorhandenen App-Datenmodell und überführt
die Daten in das von `twoddpcr` erwartete Format:

- `Ch1.Amplitude` = ausgewählter Y-Kanal
- `Ch2.Amplitude` = ausgewählter X-Kanal
- Well-Objekte werden pro `plate_name + sample + well` erzeugt

Verfügbare Abschnitte:

1. **Classify**
   - `K-means Clustering`, `Thresholds`, `Grid`, `K-Nearest Neighbour`
   - `heatPlot` aus `twoddpcr` mit originaler Density-Farbgebung
   - `K-NN` kann Trainingsdaten direkt aus einem `Grid` oder aus einer vorhandenen Results-Klassifikation beziehen
2. **Results**
   - `dropletPlot` aus `twoddpcr`
   - Rain-Behandlung (`Mahalanobis`, `Standard Deviation`)
   - Parameter pro Klasse (`NN`, `NP`, `PN`, `PP`) wie in `shinyVis`
   - ZIP-Export der klassifizierten Amplituden
3. **Summary**
   - `plateSummary`-Tabelle
   - CSV-Export
   - HTML-Report auf Basis der `twoddpcr`-Vorlage

## Tests

```bash
Rscript tests/testthat.R
```

## Docker

### Build

```bash
docker build -t dpcr-analysis .
```

### Run

```bash
docker run --rm -p 3838:3838 dpcr-analysis
```

Dann im Browser: `http://localhost:3838`

## Hinweise

- Export/Import speichert mindestens: `dpcr_data`, `validation_report`, `metadata`, `app_settings`, `custom_palettes`.
- Roche/Bio-Rad-Importer liefern aktuell Platzhalter-Hinweise und können später erweitert werden.
- `twoddpcr` benötigt Zwei-Kanal-Daten. Reine Referenzkanäle ohne Partnerkanal können dort nicht klassifiziert werden.
- Die inhaltliche Dokumentation zur neuen Einstellungsseite liegt zusätzlich in der App unter **Hilfe**.
