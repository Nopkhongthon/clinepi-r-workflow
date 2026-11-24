# Clean the fasting blood sugar (FBS) trial, drug A vs drug B ----------------
# Raw source: one Excel sheet, one row per patient, a date and an FBS column
#             for each visit (month 0, 3, 6, 9, 12), typed by several staff.
# Output: output/fbs_clean.rds, one row per patient per visit (long format),
#         ready for the mixed model.
#
# The data are a mock-up made for teaching. No real patient is involved.
#
# Run the steps in order. Every step changes `df` (or `dfl` once the data are
# long) and leaves it in the environment, so after any step you can look at it
# and see exactly what that one step did. To start again, re-run step 2.

#%%@note step 1: packages, paths and lookups ----------------------------------

# install.packages(c("readxl", "janitor", "readr", "dplyr", "tidyr", "stringr", "lubridate", "summarytools"))

library(readxl)     # the raw data arrives as one Excel sheet
library(janitor)    # clean_names() and the Excel serial date converter
library(readr)      # parse_number() for text that contains a number
library(dplyr)      # verbs for the cleaning chain
library(tidyr)      # pivot_longer(): one row per patient becomes one row per visit
library(stringr)    # matching on the free text fields
library(lubridate)  # date parsing

path_raw   <- "data/fbs_mockup_trial.xlsx"
path_clean <- "output/fbs_clean.rds"

# text that means "no value was recorded". ไม่มา means "did not come".
na_codes <- c("", " ", ".", "-", "N/A", "NA", "na", "ไม่มา")

# Thai month abbreviation to a month number, written as /n/ so that
# "23 มิ.ย. 2568" turns into "23/6/2568". The dots are escaped because
# str_replace_all() reads these names as regular expressions.
thai_month <- c(
  "มี\\.ค\\." = "/3/",  "เม\\.ย\\." = "/4/",  "มิ\\.ย\\." = "/6/",
  "ม\\.ค\\."  = "/1/",  "ก\\.พ\\."  = "/2/",  "พ\\.ค\\."  = "/5/",
  "ก\\.ค\\."  = "/7/",  "ส\\.ค\\."  = "/8/",  "ก\\.ย\\."  = "/9/",
  "ต\\.ค\\."  = "/10/", "พ\\.ย\\."  = "/11/", "ธ\\.ค\\."  = "/12/"
)

# no real visit can fall after this
study_close <- as.Date("2026-06-30")

#%%@note step 2: import the sheet as text -------------------------------------

# Almost every column mixes numbers with words, so readxl is not allowed to
# guess: read the whole sheet as text and give each variable its type later,
# deliberately, in a step you can see.
df <- readxl::read_excel(
  path_raw,
  sheet     = "data",
  col_types = "text",
  na        = na_codes
) |>
  janitor::clean_names()

#%%@note step 3: clean the patient id -----------------------------------------

# the patient number was typed as "32", "004" and "FBS-015"; keep the digits
df <- df |>
  dplyr::mutate(id = as.integer(stringr::str_extract(id, "\\d+")))

#%%@note step 4: drop duplicate records ---------------------------------------

# first the records entered twice, identical in every single field
df <- dplyr::distinct(df)

# Some patients still appear twice, with one value typed differently. The file
# alone cannot say which entry is right, so the rule is: keep the first record.
# dup_id lists them, so the data collector can check the paper form.
dup_id <- df$id[duplicated(df$id)]

df <- dplyr::slice_head(df, n = 1, by = id)

#%%@note step 5: put every free text field on one spelling --------------------

# Squish the spaces and lower the case first, so " M ", "m" and "MALE" are one
# thing before any rule is written.
df <- df |>
  dplyr::mutate(
    dplyr::across(
      c(group, sex),
      function(x) {
        stringr::str_to_lower(stringr::str_squish(x))
      }
    )
  )

#%%@note step 6: treatment arm ------------------------------------------------

# "a", "drug a", "ยา a" and "group a" all end in the letter of the arm
df <- df |>
  dplyr::mutate(
    treatment = dplyr::case_when(
      stringr::str_detect(group, "^(drug |ยา |group )?a$") ~ "A",
      stringr::str_detect(group, "^(drug |ยา |group )?b$") ~ "B"
    ) |>
      factor(levels = c("A", "B"))
  )

#%%@note step 7: sex ----------------------------------------------------------

# coded as numbers, letters and words, in two languages
df <- df |>
  dplyr::mutate(
    sex = dplyr::case_when(
      stringr::str_detect(sex, "^(1|m|male|ชาย)$")    ~ "Male",
      stringr::str_detect(sex, "^(0|f|female|หญิง)$") ~ "Female"
    ) |>
      factor(levels = c("Female", "Male"))
  )

#%%@note step 8: age, BMI and diabetes duration become numbers ----------------

# parse_number() keeps the number and drops the unit typed beside it:
# "62 ปี" -> 62, "27.3 kg/m2" -> 27.3, "5 yr" -> 5.
df <- df |>
  dplyr::mutate(
    dplyr::across(
      c(age, bmi, dm_duration_yr),
      function(x) {
        readr::parse_number(x)
      }
    )
  ) |>
  dplyr::rename(dm_duration = dm_duration_yr)

#%%@note step 9: impossible values --------------------------------------------

# Keep the patients first, so their ids are on record before the values change.
bad_baseline <- df |>
  dplyr::filter(!dplyr::between(age, 18, 100) | bmi > 100 | dm_duration >= 99) |>
  dplyr::select(id, age, bmi, dm_duration)

df <- df |>
  dplyr::mutate(
    # 999 is a missing code, and an adult trial has nobody aged 6
    age = dplyr::if_else(dplyr::between(age, 18, 100), age, NA_real_),
    # a BMI above 100 lost its decimal point: 273 was 27.3
    bmi = dplyr::if_else(bmi > 100, bmi / 10, bmi),
    # 99 years of diabetes is a missing code, not a measurement
    dm_duration = dplyr::if_else(dm_duration >= 99, NA_real_, dm_duration)
  )

#%%@note step 10: wide to long, and back -----------------------------------

# The sheet has a date_m3 and an fbs_m3 column for every visit. A mixed model
# needs one row per visit instead. names_pattern splits "fbs_m3" into the
# value name ("fbs") and the month (3); ".value" makes date and fbs columns.
dfl <- df |>
  dplyr::select(
    id, treatment, age, sex, bmi, dm_duration,
    dplyr::matches("^(date|fbs)_m\\d+$")
  ) |>
  tidyr::pivot_longer(
    cols            = dplyr::matches("^(date|fbs)_m\\d+$"),
    names_pattern   = "(date|fbs)_m(\\d+)",
    names_to        = c(".value", "month"),
    names_transform = list(month = as.integer)
  )

# The reverse move, long back to wide. names_glue rebuilds "fbs_m3" from the
# value name and the month; names_vary = "slowest" keeps date_m0 next to
# fbs_m0, as on the sheet. Look only, dfw is not used again.
dfw <- dfl |>
  tidyr::pivot_wider(
    names_from  = month,
    values_from = c(date, fbs),
    names_glue  = "{.value}_m{month}",
    names_vary  = "slowest"
  )

#%%@note step 11: drop the visits that never happened -------------------------

# A visit with neither a date nor an FBS is a patient who stopped coming.
# That is dropout, not dirt. A mixed model uses the visits that did happen,
# which is valid when dropout depends only on values we saw (missing at random).

dfl <- dfl |>
  dplyr::filter(!(is.na(date) & is.na(fbs)))

#%%@note step 12: FBS becomes a number on one unit -----------------------------

# The glucometer shows mmol/L, the lab reports mg/dL, and staff typed both.
# No fasting sugar in mg/dL is below 30, and none in mmol/L is above 30, so
# the size of the number tells the unit. 1 mmol/L of glucose = 18 mg/dL.
dfl <- dfl |>
  dplyr::mutate(
    fbs_typed = fbs,
    fbs       = readr::parse_number(fbs),
    fbs       = dplyr::if_else(fbs < 30, round(fbs * 18), fbs),
    # 999 is a missing code; a fasting value above 600 is not plausible for an
    # outpatient in this trial
    fbs       = dplyr::if_else(fbs > 600, NA_real_, fbs)
  )

fbs_fixed <- dfl |>
  dplyr::filter(
    readr::parse_number(fbs_typed) < 30 | readr::parse_number(fbs_typed) > 600
  ) |>
  dplyr::select(id, month, fbs_typed, fbs)

#%%@note step 13: visit dates, one habit per line -----------------------------

# Every line below fixes exactly one habit and leaves the text ready for the
# next line. Look at dfl after this step: `date` and `txt` side by side is
# the whole lesson.
dfl <- dfl |>
  dplyr::mutate(
    txt = stringr::str_squish(date),

    # a Thai month abbreviation becomes a month number: 23 มิ.ย. 2568 -> 23/6/2568
    txt = stringr::str_replace_all(txt, thai_month),
    txt = stringr::str_remove_all(txt, " "),

    # a cell Excel stored as a real date arrives as a bare serial number
    serial = dplyr::if_else(stringr::str_detect(txt, "^\\d{5}$"), txt, NA_character_),
    txt    = dplyr::if_else(
      !is.na(serial),
      format(janitor::excel_numeric_to_date(as.numeric(serial)), "%d/%m/%Y"),
      txt
    ),

    # a year above 2400 is Buddhist Era and loses 543
    year = as.integer(stringr::str_extract(txt, "\\d{4}")),
    txt = dplyr::if_else(
      year > 2400,
      stringr::str_replace(txt, "\\d{4}", as.character(year - 543)),
      txt,
      missing = txt
    ),

    # two layouts are left, dd/mm/yyyy and yyyy-mm-dd
    visit_date = dplyr::coalesce(
      lubridate::dmy(txt, quiet = TRUE),
      lubridate::ymd(txt, quiet = TRUE)
    ),

    # a visit after the study closed is a mistyped year, not a real visit
    visit_date = dplyr::if_else(
      visit_date > study_close,
      as.Date(NA),
      visit_date,
      missing = visit_date
    )
  )

# anything still missing had a date typed; keep the rows and list them for the data collector
date_bad <- dfl |>
  dplyr::filter(is.na(visit_date), !is.na(date)) |>
  dplyr::select(id, month, date)

#%%@note step 14: time since the first visit ----------------------------------

# "Month 3" is the plan. The real visit can be a few days early or late, so
# count the actual days from each patient's month 0 visit. The mixed model can
# then use either the planned month or the exact time.
dfl <- dfl |>
  dplyr::mutate(
    date_m0     = visit_date[month == 0],
    days        = as.numeric(visit_date - date_m0),
    # lubridate counts real calendar months, so 15 Jan to 15 Apr is exactly 3,
    # whatever the lengths of February and March
    time_months = round(
      lubridate::time_length(
        lubridate::interval(date_m0, visit_date),
        unit = "months"
      ),
      2
    ),
    # a visit that lost its date keeps the planned month as its time
    time_months = dplyr::coalesce(time_months, as.numeric(month)),
    # the day the visit was due: month 0 date plus the planned months.
    # %m+% keeps 31 Jan + 1 month on the last day of February (28 or 29)
    # instead of jumping into March.
    # Write period(month = month): period(month, units = "months") would ADD
    # the whole column into one period and give every visit the same due date.
    due_date    = date_m0 %m+% lubridate::period(month = month),
    .by = id
  )

# a visit more than 3 weeks from its due date is worth a look
off_window <- dfl |>
  dplyr::filter(abs(as.numeric(visit_date - due_date)) > 21) |>
  dplyr::select(id, month, due_date, visit_date)

#%%@note step 15: keep the analysis variables ---------------------------------

dfl <- dfl |>
  dplyr::select(
    id, treatment, age, sex, bmi, dm_duration,
    month, visit_date, days, time_months, fbs
  ) |>
  dplyr::arrange(id, month)

#%%@note step 16: check -------------------------------------------------------

stopifnot(
  # the numbers known for this dataset: 200 patients, 912 visits
  dplyr::n_distinct(dfl$id) == 200,
  nrow(dfl) == 912,
  # every patient has a month 0 visit, and no visit appears twice
  sum(dfl$month == 0) == 200,
  nrow(dplyr::distinct(dfl, id, month)) == nrow(dfl),
  all(dfl$month %in% c(0, 3, 6, 9, 12)),
  # every free text field landed on the levels it was meant to
  !any(is.na(dfl$treatment)),
  nlevels(dfl$sex) == 2,
  # FBS is back on one unit, and nobody is seen before they enrol
  all(dplyr::between(dfl$fbs, 40, 400), na.rm = TRUE),
  all(dfl$days >= 0, na.rm = TRUE)
)

#%%@note step 17: describe the cleaned data ------------------------------------

# one look at every variable: type, values, missing and a small graph
summarytools::dfSummary(dfl)

#%%@note step 18: save --------------------------------------------------------

saveRDS(dfl, path_clean)
