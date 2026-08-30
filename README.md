# tm.hyslaw

Teal modules and reusable builders for **exploratory Hy's Law monitoring** from CDISC ADaM-style `ADSL` + `ADLB`.

This is a review aid, not a submission TFL. It does not implement alternative-cause exclusion (pre-existing liver disease, obstruction, alcohol). Admiral [ADLBHY](https://pharmaverse.github.io/admiral/main/articles/hys_law.html) derives ADaM flags; [dv.edish](https://github.com/Boehringer-Ingelheim/dv.edish) and [hep-explorer](https://github.com/SafetyGraphics/hep-explorer) are interactive eDISH apps on other stacks. This package sits in between: **table/plot builders with no Shiny**, plus optional **teal** modules.

Peak eDISH maxima are **not** the same as biochemical potential Hy's Law. The scatter uses on-treatment peaks that need not occur on the same day. Red open circles mark concurrent-window cases (optional ALP exclusion). Study-specific `ADDILI` is not an input; the package documents the ADLB algorithm instead.

## Design rule

| Layer | What it answers | Default |
| --- | --- | --- |
| Peak eDISH | On-treatment max ALT or AST / ULN vs max BILI / ULN (may be different days) | Lines at 3x and 2x ULN; operator `>=` |
| mDISH | Same peaks divided by baseline | Same 3x / 2x cuts on the baseline scale |
| Cholestatic screen | Peak ALP / ULN vs peak BILI / ULN | Lines at 2x and 2x ULN |
| Concurrent Hy's Law | BILI meeting the bilirubin cut on or after ALT/AST meeting the AT cut | `window_days = 14` (admiral vignette) |
| ALP exclusion | Concurrent cases with ALP meeting the ALP cut in the same window | On; set `exclude_alp = FALSE` to turn off |
| Threshold operator | `>=` (package default) vs `>` (FDA 2009 / many CSR shells) | `compare = ">="` |

Peak eDISH can look like Hy's Law when the two peaks are weeks apart. Concurrent-window classification is the monitoring rule. `window_days = 7` or `30` can match a SAP. Missing ALP is not imputed.

## Install and run

```r
# from the package root
pkgload::load_all()
shiny::runApp("app.R")
```

Or call the builders without Shiny:

```r
library(tm.hyslaw)

data <- make_synthetic_hyslaw_data()
classified <- classify_hyslaw(data$ADSL, data$ADLB)
build_hyslaw_table(classified = classified)
build_liver_elevation_table(classified = classified)
plot_edish(classified = classified)
plot_edish(classified = classified, which = "mdish")
plot_edish(classified = classified, which = "cholestatic")
hyslaw_listing(classified = classified)
```

## What is calculated

Always (subject-level):

- On-treatment peak ALT / ULN, AST / ULN, ALT or AST / ULN, BILI / ULN, ALP / ULN
- Baseline peaks and peak / baseline (mDISH) when `ABLFL` or `ADY <= 0` is present
- eDISH quadrant: Hy's Law, Temple's corollary, Cholestasis, or Normal or near normal
- Cholestatic ALP-vs-BILI quadrant
- Concurrent Hy's Law flag and dates; concurrent cholestatic flag
- ALP-elevated-in-window, missing-ALP, potential Hy's Law after optional ALP exclusion
- R-ratio and hepatocellular / mixed / cholestatic pattern

The default summary table has two sections: four peak-quadrant counts (ST&F-style maximal abnormalities) and biochemical concurrent counts. `build_liver_elevation_table()` adds nested 3/5/10/20x and 1.5/2x elevations plus the combination row split by ALP. The listing adds R-ratio; symptoms and alternative causes are not in ADLB and are not filled.

## Data dependencies

| Dataset | Required | Recommended |
| --- | --- | --- |
| **ADSL** | `USUBJID`, arm/group (e.g. `TRT01A`) | `SAFFL`, `ITTFL` |
| **ADLB** | `USUBJID`, `PARAMCD`, `AVAL`, ULN (`ANRHI` or Pilot `A1HI`) | `ADT` / `ADY`, `ONTRTFL`, `ABLFL`, `ALP` or `ALKPH` |

`PARAMCD` values: `ALT`, `AST`, `BILI`, and `ALP` (`ALKPH` is accepted). Study-specific `ADDILI` is not required.

## Demo / test data

Unit tests use **synthetic ADLB** so all four quadrants and the peak-vs-window distinction are guaranteed. Alzheimer / Xanomeline public studies rarely contain true Hy's Law cases.

Public ADaM is for integration tests and the demo app only (exploratory; attribute to the data owner; not a submission TFL):

1. [`pharmaverseadam`](https://pharmaverse.github.io/pharmaverseadam/) `adsl` + `adlb` (demo preferred)
2. [CDISC Pilot 01](https://github.com/cdisc-org/sdtm-adam-pilot-project) `adsl.xpt` + `adlbc.xpt` via `source("data-raw/process_data.R")` (gitignored XPTs)
3. `random.cdisc.data` when available as a Suggests fallback in other packages; this demo prefers synthetic if public data are missing

[`pharmaverseadam::adlbhy`](https://pharmaverse.github.io/pharmaverseadam/reference/adlbhy.html) can be compared as an admiral-derived `HYSLAW` parameter. Counts are not required to match this package.

## Related work

- [admiral Hy's Law vignette](https://pharmaverse.github.io/admiral/main/articles/hys_law.html) — ADaM derivation (`ADLBHY`), 14-day window
- [dv.edish](https://boehringer-ingelheim.github.io/dv.edish/) — DaVinci Shiny eDISH / mDISH on pharmaverseadam
- [SafetyGraphics hep-explorer](https://github.com/SafetyGraphics/hep-explorer) — interactive hepatic explorer (not teal)
- [FDA 2009 DILI guidance](https://www.fda.gov/media/116737/download) — confirmed Hy's Law (causality), not this lab screen
- [CDISC ADDILI design slides (2024)](https://www.cdisc.org/sites/default/files/2024-09/2024_CDISC-Slide-ADDILI.pdf) — hepatocellular vs cholestatic screening axes this package follows from ADLB
