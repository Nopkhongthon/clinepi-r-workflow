# A clinical-epidemiology workflow in R

A small, readable workflow for a repeated-measures trial, starting from messy Excel.
It grows chapter by chapter. It now reaches Table 1, ggplot2 and the propensity score (model, weights, balance and the marginal effect of drug B). Mixed models come next.
The data follow a trial of two drugs (A and B) and fasting blood sugar (FBS) over 12 months.
Each script is one stage. The single page `docs/index.html` walks through them with real output. It opens with a double-click, no server needed.

This is a teaching pack. It holds the data, the scripts and the page. It is not an R package.

The data are a mock-up made for teaching. No real patient is involved. See `data/README.md` for the layout of the sheet.

## Read the page online

<https://nopkhongthon.github.io/clinepi-r-workflow/>

## Get the pack

On the GitHub page, click the green Code button, then Download ZIP. Unzip the folder and open it as the project in Positron or RStudio.

## Packages

```r
install.packages(c("dplyr", "tidyr", "readr", "stringr", "lubridate", "readxl", "janitor", "ggplot2", "rstatix", "gtsummary", "gt", "summarytools", "ragg", "cobalt", "survey", "emmeans", "MatchIt", "optmatch", "sandwich", "broom", "forcats", "knitr", "rmarkdown"))
install.packages("epicalc", repos = c("https://medipe.psu.ac.th/epicalc",
                                      "https://cloud.r-project.org"))
```

You also need Quarto to build the page. The file `packages.bib` holds the reference of every package, and the page lists them at the end.

## Run order

Run everything from the project folder.

```sh
Rscript code/1_clean_data.r                 # dirty Excel -> output/fbs_clean.rds
Rscript code/2_basic_explore.r              # tables and plots
Rscript code/3_table1.r                     # Table 1
Rscript code/4_ggplot_basics.r              # ggplot2 lesson, saves output/figures/*.tiff
Rscript code/5_ps_model.r                   # propensity score and weights
Rscript code/6_ps_balance.r                 # balance check
Rscript code/7_ps_effect.r                  # effect of drug B, marginal effect
quarto render                               # build the page into docs/index.html
```

## Folders

- `code/` the scripts, one per stage. Steps are marked `#%%@note step N:`.
- `data/` the Excel sheet and its notes in `data/README.md`.
- `output/` objects the scripts write (not tracked).
- `docs/index.html` the rendered page (everything is inside this one file).
- `_*.qmd` the sections of the page. `index.qmd` joins them.
- `packages.bib` the references of the R packages.
