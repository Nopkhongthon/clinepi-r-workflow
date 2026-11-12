# A clinical-epidemiology workflow in R

A small, readable workflow for a repeated-measures trial, starting from messy Excel.
It grows chapter by chapter. It now reaches Table 1, and mixed models come next.
The data are an invented trial of two drugs (A and B) and fasting blood sugar (FBS)
over 12 months. Each script is one stage. The single page `docs/index.html` walks through them with real output. It opens with a double-click, no server needed.

All data in this repository are simulated. No real patient is involved.

## Packages

```r
install.packages(c("dplyr", "tidyr", "tibble", "readr", "stringr", "lubridate", "readxl", "janitor", "ggplot2", "rstatix", "gtsummary", "gt", "summarytools", "ragg", "knitr", "rmarkdown"))
install.packages("epicalc", repos = "https://medipe.psu.ac.th/epicalc")
```

You also need Quarto (to build the book) and Python 3 with `openpyxl` (to make the dirty Excel file).

## Run order from a fresh clone

Run everything from the repository root.

```sh
Rscript code/0_simulate_data.r              # clean "truth" data -> data/*.csv
python3 data/_source/make_dirty_fbs.py      # dirty Excel -> data/fbs_sim_dirty.xlsx
Rscript code/1_clean_data.r                 # dirty Excel -> output/fbs_clean.rds
Rscript code/2_basic_explore.r              # tables and plots
Rscript code/3_table1.r                     # Table 1
Rscript code/4_ggplot_basics.r              # ggplot2 lesson, saves output/figures/*.tiff
quarto render                               # build the page into docs/index.html
```

## Folders

- `code/` the scripts, one per stage. Steps are marked `#%%@note step N:`.
- `data/` simulated data. `data/_source/` holds the Python script that makes the Excel file dirty.
- `output/` objects the scripts write (not tracked).
- `docs/index.html` the rendered page (everything is inside this one file).
- `_*.qmd` the sections of the page; `index.qmd` joins them.
