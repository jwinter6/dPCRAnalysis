# dPCR Analyseplattform (R/Shiny)

Modulare Shiny-Web-App zur Analyse digitaler PCR-Daten mit Fokus auf **Qiagen QIAcuity**.
Roche Digital LightCycler und Bio-Rad QX sind als Importer-Platzhalter vorbereitet.

## Features

- Linke Sidebar-Navigation mit Seiten:
  - Start
  - Daten laden
  - Übersicht
  - Qualität
  - Sample-Analyse (Platzhalter)
  - Detailanalyse Sample (Platzhalter)
  - Report (Platzhalter)
  - Export/Import
  - Hilfe
- QIAcuity-CSV-Import (`skip = 1`, Mapping auf internes Standardformat)
- Plausibilitäts- und Validierungschecks mit Report
- Interaktive Übersicht (DT + ggplot2 + plotly)
- Qualitätsseite mit Kennzahlen und zusätzlichen Plots
- dpcR-basierter Dichteplot (`dpcr_density`) für λ / positive Moleküle
- Analyse-Persistenz via `.RData` (Export/Import)
- Unit-Tests für Import und Validierung (`testthat`)

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
│  ├─ data_model.R
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
│  │  ├─ mod_help.R
├─ tests/
│  ├─ testthat.R
│  ├─ testthat/
│  │  ├─ test-import-qiaquity.R
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
  "ggplot2", "dplyr", "tidyr", "readr", "tibble", "testthat", "dpcR"
))
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

- Export/Import speichert mindestens: `dpcr_data`, `validation_report`, `metadata`.
- Roche/Bio-Rad-Importer liefern aktuell Platzhalter-Hinweise und können später erweitert werden.
