# Plots with ggplot2: from a first scatter plot to a figure for a paper -------
# Input:  output/fbs_clean.rds, written by 1_clean_data.r (one row per visit)
# Output: output/figures/fbs_by_month.tiff, one finished figure
#
# ggplot2 builds a plot in layers, joined with +
#   ggplot(data, aes())  which data, and which column goes on which axis
#   + geom_*()           how to draw it: points, lines, bars, fitted curves
#   + scale_*()          where the ticks go and the range of each axis
#   + labs()             the names: axes, legend, title
#   + theme_*() / theme() the look: background, axis lines, text size and face
# Each step below adds one idea. Run a step, look at the plot, then go on.

#%%@note step 1: packages and paths -------------------------------------------

# install.packages(c("dplyr", "ggplot2", "ragg"))

library(dplyr)    # filter() and summarise(): get the data ready for each plot
library(ggplot2)  # every plot in this script
library(ragg)     # agg_tiff(): a TIFF file that keeps its 300 dpi setting

path_clean <- "output/fbs_clean.rds"
path_fig   <- "output/figures/fbs_by_month.tiff"

#%%@note step 2: the data for the plots ---------------------------------------

dfl <- readRDS(path_clean)

# one row per patient: the month 0 visit
df_base <- dfl |>
  dplyr::filter(month == 0)

# one row per arm per month: the mean FBS and its standard deviation (SD).
# A line plot and a bar graph of means draw from this small table.
df_mean <- dfl |>
  dplyr::summarise(
    n        = sum(!is.na(fbs)),
    mean_fbs = mean(fbs, na.rm = TRUE),
    sd_fbs   = sd(fbs, na.rm = TRUE),
    .by      = c(treatment, month)
  )

df_mean

#%%@note step 3: a first scatter plot -----------------------------------------

# Every plot needs three things: the data, aes() to say which column goes on
# the x axis and which on the y axis, and a geom to draw them.
# geom_point() draws one dot per row: here one dot per patient.
p_scatter <- ggplot2::ggplot(df_base, ggplot2::aes(x = dm_duration, y = fbs)) +
  ggplot2::geom_point()

p_scatter

#%%@note step 4: colour the dots by a third variable ---------------------------

# Put one more column into aes() and ggplot2 colours the dots by it and draws
# the legend by itself. alpha = 0.6 makes the dots see-through, so dots that
# overlap show up darker. size sets how big each dot is.
p_scatter_arm <- ggplot2::ggplot(df_base, ggplot2::aes(x = dm_duration, y = fbs,
                                                       colour = treatment)) +
  ggplot2::geom_point(alpha = 0.6, size = 2)

p_scatter_arm

#%%@note step 5: a line plot ---------------------------------------------------

# A line joins the points in the order of x. Each arm needs its own line, so
# the plot uses the summary table: one row per arm per month.
# group = treatment says which points belong to the same line, and
# colour = treatment gives each line its own colour.
p_line <- ggplot2::ggplot(df_mean, ggplot2::aes(x = month, y = mean_fbs,
                                                colour = treatment, group = treatment)) +
  ggplot2::geom_line() +
  ggplot2::geom_point()

p_line

#%%@note step 6: a loess curve --------------------------------------------------

# geom_smooth() draws a fitted curve over the raw points.
# method = "loess" is a flexible local fit: it follows the data without a
# shape chosen in advance. The grey band is its 95% confidence interval.
# x is time_months, the exact time of each visit, so the dots spread out
# instead of sitting in five columns.
p_loess <- ggplot2::ggplot(dfl, ggplot2::aes(x = time_months, y = fbs)) +
  ggplot2::geom_point(alpha = 0.2) +
  ggplot2::geom_smooth(method = "loess", formula = y ~ x)

p_loess

#%%@note step 7: a straight line, the linear fit -------------------------------

# method = "lm" fits a linear regression of y on x and draws it as a straight
# line. FBS falls fast at first and then levels off, so a straight line
# misses that shape. Compare it with the loess curve of step 6.
p_linear <- ggplot2::ggplot(dfl, ggplot2::aes(x = time_months, y = fbs)) +
  ggplot2::geom_point(alpha = 0.2) +
  ggplot2::geom_smooth(method = "lm", formula = y ~ x)

p_linear

#%%@note step 8: curved fits, quadratic and cubic -----------------------------

# Still method = "lm", with a curve written into the formula.
# poly(x, 2) adds x squared: a quadratic, one bend.
# poly(x, 3) adds x cubed as well: a cubic, up to two bends.
p_quadratic <- ggplot2::ggplot(dfl, ggplot2::aes(x = time_months, y = fbs)) +
  ggplot2::geom_point(alpha = 0.2) +
  ggplot2::geom_smooth(method = "lm", formula = y ~ poly(x, 2))

p_cubic <- ggplot2::ggplot(dfl, ggplot2::aes(x = time_months, y = fbs)) +
  ggplot2::geom_point(alpha = 0.2) +
  ggplot2::geom_smooth(method = "lm", formula = y ~ poly(x, 3))

p_quadratic
p_cubic

#%%@note step 9: all four fits on one plot ------------------------------------

# Several geom_smooth() layers can share one plot. Writing colour = "Linear"
# INSIDE aes() gives that line a name, so the legend lists every fit.
# se = FALSE drops the confidence bands, which would hide each other here.
p_fits <- ggplot2::ggplot(dfl, ggplot2::aes(x = time_months, y = fbs)) +
  ggplot2::geom_point(alpha = 0.15, colour = "grey50") +
  ggplot2::geom_smooth(ggplot2::aes(colour = "Loess"),
                       method = "loess", formula = y ~ x, se = FALSE) +
  ggplot2::geom_smooth(ggplot2::aes(colour = "Linear"),
                       method = "lm", formula = y ~ x, se = FALSE) +
  ggplot2::geom_smooth(ggplot2::aes(colour = "Quadratic"),
                       method = "lm", formula = y ~ poly(x, 2), se = FALSE) +
  ggplot2::geom_smooth(ggplot2::aes(colour = "Cubic"),
                       method = "lm", formula = y ~ poly(x, 3), se = FALSE) +
  ggplot2::labs(colour = "Fit")

p_fits

#%%@note step 10: bar graphs ---------------------------------------------------

# geom_bar() counts the rows for you: one bar per sex, height = number of
# patients. fill colours the inside of the bars (colour is only the outline).
# position = "dodge" puts the two arms side by side instead of stacked.
p_bar_count <- df_base |>
  dplyr::filter(!is.na(sex)) |>
  ggplot2::ggplot(ggplot2::aes(x = sex, fill = treatment)) +
  ggplot2::geom_bar(position = "dodge")

# geom_col() draws a height you already computed, here the mean FBS.
# factor(month) makes month a set of groups, so each visit gets its own bars.
p_bar_mean <- ggplot2::ggplot(df_mean, ggplot2::aes(x = factor(month), y = mean_fbs,
                                                    fill = treatment)) +
  ggplot2::geom_col(position = "dodge")

p_bar_count
p_bar_mean

#%%@note step 11: change the theme, theme_classic ------------------------------

# A theme changes the look, never the data. The default is theme_grey(): a
# grey panel with a white grid. theme_classic() is the usual choice for a
# paper: white background, an x and a y axis line with ticks, no grid.
# Add a theme with + like any other layer.
# Other complete themes to try: theme_bw(), theme_minimal(), theme_light().
p_grey    <- p_line
p_classic <- p_line + ggplot2::theme_classic()

p_grey
p_classic

#%%@note step 12: add the axis lines and ticks yourself -------------------------

# theme_minimal() has no axis lines and no ticks, so it shows how to add them.
#   axis.line         draws the x and y axis
#   axis.ticks        draws the tick marks
#   axis.ticks.length sets how long the ticks are
# scale_x_continuous(breaks = ) chooses WHERE the ticks go: here at each visit.
# limits = sets the range of an axis. Here the y axis starts at 0, which shows
# the fall in FBS against its full size.
# guide_axis(minor.ticks = TRUE) adds small unlabelled ticks between the others.
p_axes <- p_line +
  ggplot2::theme_minimal() +
  ggplot2::theme(
    axis.line         = ggplot2::element_line(colour = "black"),
    axis.ticks        = ggplot2::element_line(colour = "black"),
    axis.ticks.length = ggplot2::unit(0.2, "cm")
  ) +
  ggplot2::scale_x_continuous(breaks = c(0, 3, 6, 9, 12)) +
  ggplot2::scale_y_continuous(limits = c(0, 200), breaks = seq(0, 200, by = 25)) +
  ggplot2::guides(y = ggplot2::guide_axis(minor.ticks = TRUE))

p_axes

#%%@note step 13: name the axes, the legend and the plot ------------------------

# labs() names everything in one place: the x and y axis, the legend (named
# after the aesthetic, here colour), a title, a subtitle and a caption.
p_named <- p_classic +
  ggplot2::scale_x_continuous(breaks = c(0, 3, 6, 9, 12)) +
  ggplot2::labs(
    x        = "Month since enrolment",
    y        = "Mean fasting blood sugar (mg/dL)",
    colour   = "Drug",
    title    = "FBS falls faster on drug B",
    subtitle = "Mean of each arm at each visit",
    caption  = "Simulated data"
  )

p_named

#%%@note step 14: bold, size and face of the axis text and the title ----------

# theme() changes one piece of text at a time with element_text().
#   axis.text  the numbers on the axes       axis.title  the axis names
#   plot.title the title                      legend.title, legend.text
# face = "plain", "bold", "italic" or "bold.italic". size is in points.
# Add .x or .y to change one axis only, for example axis.text.x.
# hjust = 0.5 centres the title (0 = left, 1 = right).
# A theme() can be kept in an object and added to any plot later.
bold_text <- ggplot2::theme(
  axis.text     = ggplot2::element_text(face = "bold", size = 12, colour = "black"),
  axis.title    = ggplot2::element_text(face = "bold", size = 13),
  plot.title    = ggplot2::element_text(face = "bold", size = 15, hjust = 0.5),
  plot.subtitle = ggplot2::element_text(hjust = 0.5),
  legend.title  = ggplot2::element_text(face = "bold"),
  # bold also means a thicker axis line and ticks, never a thicker data line
  axis.line     = ggplot2::element_line(linewidth = 1),
  axis.ticks    = ggplot2::element_line(linewidth = 1)
)

p_text <- p_named + bold_text

p_text

#%%@note step 15: dodge the points so the arms do not overlap ----------------

# At each month the two arms sit at almost the same height, so their points
# and error bars (mean +/- SD) cover each other. position_dodge() moves each
# arm a little to the side. Give the SAME dodge to every layer that has to
# move together: the error bars, the lines and the points.
p_overlap <- ggplot2::ggplot(df_mean, ggplot2::aes(x = month, y = mean_fbs,
                                                   colour = treatment, group = treatment)) +
  ggplot2::geom_errorbar(ggplot2::aes(ymin = mean_fbs - sd_fbs, ymax = mean_fbs + sd_fbs),
                         width = 0.4) +
  ggplot2::geom_line() +
  ggplot2::geom_point(size = 2.5) +
  ggplot2::theme_classic()

dodge <- ggplot2::position_dodge(width = 0.8)

p_dodged <- ggplot2::ggplot(df_mean, ggplot2::aes(x = month, y = mean_fbs,
                                                  colour = treatment, group = treatment)) +
  ggplot2::geom_errorbar(ggplot2::aes(ymin = mean_fbs - sd_fbs, ymax = mean_fbs + sd_fbs),
                         width = 0.4, position = dodge) +
  ggplot2::geom_line(position = dodge) +
  ggplot2::geom_point(size = 2.5, position = dodge) +
  ggplot2::scale_x_continuous(breaks = c(0, 3, 6, 9, 12)) +
  ggplot2::labs(x = "Month since enrolment", y = "Fasting blood sugar (mg/dL), mean (SD)",
                colour = "Drug") +
  ggplot2::theme_classic() +
  bold_text

p_overlap
p_dodged

#%%@note step 16: dodge the raw points too ------------------------------------

# With one dot per patient, the dots of the two arms also land on top of each
# other. position_jitterdodge() first puts the arms side by side (dodge) and
# then spreads the dots of each arm a little sideways (jitter), so every
# patient can be seen. factor(month) makes each visit a group to dodge within.
p_raw_dodged <- ggplot2::ggplot(dfl, ggplot2::aes(x = factor(month), y = fbs,
                                                  colour = treatment)) +
  ggplot2::geom_point(alpha = 0.5,
                      position = ggplot2::position_jitterdodge(jitter.width = 0.2,
                                                               dodge.width = 0.8,
                                                               seed = 123)) +
  ggplot2::labs(x = "Month since enrolment", y = "Fasting blood sugar (mg/dL)",
                colour = "Drug") +
  ggplot2::theme_classic() +
  bold_text

p_raw_dodged

#%%@note step 17: save the finished figure ------------------------------------

# ggsave() writes a plot to a file. Journals usually want a TIFF at 300 dpi.
# ragg::agg_tiff stores the 300 dpi inside the file, and lzw compression keeps
# the file small without losing anything. width and height are in inches.
dir.create("output/figures", showWarnings = FALSE)

ggplot2::ggsave(path_fig, p_dodged, device = ragg::agg_tiff,
                width = 7, height = 4.5, units = "in", dpi = 300, compression = "lzw")

message(sprintf("figure saved: %s", path_fig))

#%%@note step 18: check -------------------------------------------------------

stopifnot(
  # one mean per arm at each of the five visits
  nrow(df_mean) == 2 * 5,
  # the finished figure exists
  file.exists(path_fig)
)
