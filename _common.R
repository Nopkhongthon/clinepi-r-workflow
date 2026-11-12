# Shared setup for every chapter. Helpers live here and nowhere else.
# The chapters never repeat script code. They load each step of a script by
# its marker, "#%%@note step N:", and knitr runs it as a chunk.

options(width = 100, dplyr.width = 100)
knitr::opts_chunk$set(comment = "#>", fig.align = "center")

# Remove the comments from lines of R code, so the page shows only code.
# The script files keep their comments. A "#" inside a string is not a comment,
# so the lines are read one character at a time. The walk follows ' " and `
# quotes, a backslash escape inside them, and raw strings such as r"(...)".
# Everything from the first "#" outside a string is cut and the spaces before it
# are trimmed. A line that is empty only because of a comment is dropped. Runs of
# blank lines shrink to one, and blank lines at both ends go.
strip_comments <- function(lines) {
  closers  <- c("(" = ")", "[" = "]", "{" = "}")
  in_quote <- ""   # the quote character while inside an ordinary string
  raw_end  <- ""   # the closing text while inside a raw string
  out      <- character(length(lines))
  emptied  <- logical(length(lines))

  for (i in seq_along(lines)) {
    line <- lines[i]
    n    <- stringr::str_length(line)
    j    <- 1
    cut  <- n + 1

    while (j <= n) {
      ch   <- stringr::str_sub(line, j, j)
      rest <- stringr::str_sub(line, j)

      if (nzchar(raw_end)) {
        # inside a raw string: only its closing text ends it
        if (stringr::str_starts(rest, stringr::fixed(raw_end))) {
          j       <- j + stringr::str_length(raw_end)
          raw_end <- ""
        } else {
          j <- j + 1
        }
      } else if (nzchar(in_quote)) {
        # inside an ordinary string: a backslash skips the next character
        if (ch == "\\") {
          j <- j + 2
        } else {
          if (ch == in_quote) in_quote <- ""
          j <- j + 1
        }
      } else if (ch == "#") {
        cut <- j
        break
      } else {
        prev_ok <- j == 1 || !stringr::str_detect(stringr::str_sub(line, j - 1, j - 1), "[A-Za-z0-9._]")
        raw     <- stringr::str_match(rest, "^[rR]([\"'])(-*)([(\\[{])")
        if (prev_ok && !is.na(raw[1, 1])) {
          raw_end <- paste0(closers[[raw[1, 4]]], raw[1, 3], raw[1, 2])
          j       <- j + stringr::str_length(raw[1, 1])
        } else {
          if (ch %in% c("\"", "'", "`")) in_quote <- ch
          j <- j + 1
        }
      }
    }

    out[i]     <- stringr::str_trim(stringr::str_sub(line, 1, cut - 1), side = "right")
    emptied[i] <- !nzchar(out[i]) && nzchar(stringr::str_trim(line))
  }

  out   <- out[!emptied]
  blank <- !nzchar(out)
  out   <- out[!(blank & c(FALSE, utils::head(blank, -1)))]
  filled <- which(nzchar(out))
  if (length(filled) == 0) return(character(0))
  out[min(filled):max(filled)]
}

# Turn every "#%%@note step N:" block of a script into a knitr chunk named
# "<prefix>-step-N". The marker line itself is dropped, and so are the comments.
load_steps <- function(script, prefix) {
  lines  <- readLines(script, encoding = "UTF-8")
  marker <- grep("^#%%@note step [0-9]+:", lines)
  ends   <- c(marker[-1] - 1, length(lines))
  for (i in seq_along(marker)) {
    n    <- sub("^#%%@note step ([0-9]+):.*", "\\1", lines[marker[i]])
    body <- strip_comments(lines[(marker[i] + 1):ends[i]])
    knitr::read_chunk(lines = body, labels = sprintf("%s-step-%s", prefix, n))
  }
  invisible(length(marker))
}

# The step numbers in a script, so a chapter can be checked against it.
script_steps <- function(script) {
  lines <- readLines(script, encoding = "UTF-8")
  as.integer(sub("^#%%@note step ([0-9]+):.*", "\\1", grep("^#%%@note step [0-9]+:", lines, value = TRUE)))
}

# First rows of a table, for the "look at it" moments.
peek <- function(x, n = 6) {
  knitr::kable(utils::head(as.data.frame(x), n))
}
