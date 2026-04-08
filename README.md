# Covid-19 Era Higher Education Employment Analysis

R script analyzing pandemic-driven employment changes in U.S. higher education using IPUMS-CPS microdata.

## Install

1. Ensure R 4.1+ installed
2. Install dependencies:

```r
install.packages(c("ipumsr", "tidyverse", "scales", "openxlsx"))
```

## Replicating the Data Extract

Data comes from IPUMS CPS Basic Monthly (https://cps.ipums.org). The `.dat.gz` file is too large for GitHub; recreate the extract with the steps below.

1. Create an account at https://cps.ipums.org
2. Start a new extract and select **CPS Basic Monthly** samples
3. Select all monthly samples from **January 2017** through **February 2021** (50 samples)
4. Add the following variables:

| Category | Variables |
|---|---|
| **Household -- Technical** | YEAR, SERIAL, MONTH, HWTFINL |
| **Household -- Linking** | CPSID |
| **Household -- Flag** | ASECFLAG |
| **Person -- Technical** | PERNUM |
| **Person -- Linking** | CPSIDP |
| **Person -- Weights** | WTFINL, COMPWT |
| **Person -- Demographics** | AGE, SEX, RACE, HISPAN |
| **Person -- Education** | EDUC |
| **Person -- Work** | EMPSTAT, OCC, OCC2010, OCC1990, IND, IND1990, CLASSWKR, AHRSWORKT, WHYUNEMP |

5. Do not apply any case-level filters
6. Submit the extract and download the `.dat.gz` and `.xml` files
7. Place both files in `download/cps_00005/`

## Configure

Edit the constants at the top of `code.R`:

```r
API_KEY    <- ""
IPUMS_DDI  <- "download/cps_00005/cps_00005.xml"
EXPORT_DIR <- "exports"
```

## Run

```r
source("code.R")
```

- Reads IPUMS microdata via DDI descriptor
- Classifies records: work status, race, age group, 4 levels of occupation coding (OCC1990)
- Filters to higher-ed workers (IND1990 850/851), ages 16+, employed
- Summarises employment counts by occupation, race, age, and work status
- Computes Feb 2020-2021 net changes by race and age
- Exports a multi-tab xlsx to `exports/`
- Generates 5 PNG charts: POC workforce share, race job losses, occupation trends, age net change, instructor FT/PT

## Outputs

- `exports/higher_ed_employment.xlsx` -- 13-tab workbook with all summary tables
- `exports/chart_poc_workforce_share.png`
- `exports/chart_race_job_losses.png`
- `exports/chart_occ_trend_2020.png`
- `exports/chart_age_net_change.png`
- `exports/chart_instructors_ftpt.png`

### Acknowledgements
- Coded and debugged with the assistance of claude.ai
