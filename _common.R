# Shared setup for every chapter. Helpers live here and nowhere else.
# The chapters never repeat script code. They load each step of a script by
# its marker, "#%%@note step N:", and knitr runs it as a chunk.

options(width = 100, dplyr.width = 100, tibble.width = 100)
knitr::opts_chunk$set(comment = "#>", fig.align = "center")

# Turn every "#%%@note step N:" block of a script into a knitr chunk named
# "<prefix>-step-N". The marker line itself is dropped.
load_steps <- function(script, prefix) {
  lines  <- readLines(script, encoding = "UTF-8")
  marker <- grep("^#%%@note step [0-9]+:", lines)
  ends   <- c(marker[-1] - 1, length(lines))
  for (i in seq_along(marker)) {
    n    <- sub("^#%%@note step ([0-9]+):.*", "\\1", lines[marker[i]])
    body <- lines[(marker[i] + 1):ends[i]]
    # trim blank lines at both ends
    while (length(body) > 0 && !nzchar(trimws(body[1])))           body <- body[-1]
    while (length(body) > 0 && !nzchar(trimws(body[length(body)]))) body <- body[-length(body)]
    knitr::read_chunk(lines = body, labels = sprintf("%s-step-%s", prefix, n))
  }
  invisible(length(marker))
}

# The step numbers in a script, so a chapter can be checked against it.
script_steps <- function(script) {
  lines <- readLines(script, encoding = "UTF-8")
  as.integer(sub("^#%%@note step ([0-9]+):.*", "\\1", grep("^#%%@note step [0-9]+:", lines, value = TRUE)))
}

# The truth settings of the simulation as a table (chapter 1, after step 1).
truth_table <- function() {
  tbl <- data.frame(
    Setting = c("fbs_baseline", "drop_a", "drop_b", "speed",
                "sd_intercept", "sd_slope", "sd_residual"),
    Value   = c(fbs_baseline, drop_a, drop_b, speed, sd_intercept, sd_slope, sd_residual),
    Meaning = c("Average FBS before treatment (mg/dL)",
                "Fall in FBS on drug A once fully worked (mg/dL)",
                "Fall in FBS on drug B once fully worked (mg/dL)",
                "Months. About 63% of the drop is done by month 4",
                "Spread of each patient's own FBS level (random intercept)",
                "Spread of each patient's drift in mg/dL a month (random slope)",
                "Visit to visit noise: last meal, sleep, the lab")
  )
  knitr::kable(tbl, align = c("l", "r", "l"))
}

# First rows of a table, for the "look at it" moments.
peek <- function(x, n = 6) {
  knitr::kable(utils::head(as.data.frame(x), n))
}
