## -----------------------------------------------------------------------------
## Example scenario comparison plots for Ecospace outputs
##
## Makes 18 figures at 4, 8, and 12 metrics, for 4 scenarios:
##   spider-overlaid  all scenarios on one radar
##   spider-separate  one radar per scenario, 2x2
##   bar-grouped      all scenarios side by side per metric
##   bar-separate     one bar panel per scenario, 2x2
##   box-grouped      all scenarios side by side per metric, 9 draws per box
##   box-separate     one box panel per scenario, 2x2
##
## The first 4 metrics are the named metrics of interest; the 8- and 12-metric
## versions extend that same set with filler metrics. All metrics are on a
## shared -1 to 1 scale. The spider and bar plots show a single mean value per
## scenario and metric; the box plots show 9 seeded draws around those same
## means, standing in for replicate runs or years of output.

rm(list = ls())

library(fmsb)

## -----------------------------------------------------------------------------
## User flags

save_png <- TRUE                        ## FALSE = draw to the screen device instead

out_dir  <- "misc-code/example-plots"   ## relative to the repo root (.Rproj working dir)

metric_counts <- c(4, 8, 12)

## -----------------------------------------------------------------------------
## Session checks
##
## Two things silently stop the PNGs from updating, so check both up front.

## 1. out_dir is relative to the repo root. If this session is rooted somewhere
##    else, say so rather than quietly writing to a brand new folder.
if(save_png && !dir.exists("misc-code")) {
  stop("Working directory is '", getwd(), "', which is not the\n",
       "  Chesapeake-Ecospace repo root. Open Chesapeake-Ecospace.Rproj, or\n",
       "  setwd() to the repo root, then re-run.")
}

## 2. A png device left open by an earlier failed run keeps a lock on its file,
##    so later runs cannot overwrite it and the PNG on disk silently goes stale.
##    Close any that are still hanging around from a previous run.
stray_devices <- dev.list()[names(dev.list()) == "png"]

for(d in stray_devices) {
  dev.set(d)
  dev.off()
}

if(length(stray_devices) > 0) {
  message("Closed ", length(stray_devices),
          " png device(s) left open by an earlier run.")
}

if(save_png && !dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

## Used at the end to confirm every figure was actually rewritten
run_started <- Sys.time()

## -----------------------------------------------------------------------------
## Scenarios and metrics

scenario_names <- c("Status Quo", "Scenario X", "Scenario Y", "Scenario X+Y")

## -----------------------------------------------------------------------------
## Colors -- edit these

## One color per scenario. Colors are matched to scenarios BY NAME, so the order
## here does not matter and each scenario's color is unambiguous. Any R color
## works: a name ("grey40"), a hex code ("#4C72B0"), or rgb().
scenario_colors <- c(
  "Status Quo"   = "blue",
  "Scenario X"   = "orange",
  "Scenario Y"   = "mediumpurple",
  "Scenario X+Y" = "hotpink"
)

## Transparency of the shaded polygon fills (0 = no fill, 1 = solid).
## The overlaid plot uses a lighter fill because four polygons stack up.
fill_alpha_overlaid <- 0.15
fill_alpha_separate <- 0.25

## Stop early with a clear message if a scenario has no color assigned
missing_colors <- setdiff(scenario_names, names(scenario_colors))
if(length(missing_colors) > 0) {
  stop("No color assigned in scenario_colors for: ",
       paste(missing_colors, collapse = ", "))
}

## Colors in the same order as scenario_names, for the plotting code below
line_colors <- scenario_colors[scenario_names]

## -----------------------------------------------------------------------------
## Metrics

metric_names <- c(
  "BCF_Catch",
  "BCF_Biomass",
  "Blue_Crab_Biomass",
  "Jobs_Comm_BCF",
  paste0("Metric_", 5:12)
)

## -----------------------------------------------------------------------------
## Example data
##
## All metrics are scaled -1 to 1, where 0 is neutral. The four named metrics
## tell a simple trade-off story: Status Quo sits near zero, Scenario X favors
## catch and jobs at the expense of biomass, Scenario Y is the mirror image, and
## Scenario X+Y lands intermediate. These mean values are hardcoded; the box
## plots further down draw a seeded spread around them.

## Rows = scenarios, columns = the four named metrics
named_metric_values <- rbind(
  c( 0.10,  0.10, -0.10, -0.10),   ## Status Quo
  c( 0.60, -0.30, -0.10,  0.30),   ## Scenario X
  c(-0.20,  0.35,  0.70, -0.25),   ## Scenario Y
  c( 0.35,  0.05,  0.30,  0.15)    ## Scenario X+Y
)

## Rows = scenarios, columns = filler metrics 5-12
filler_metric_values <- rbind(
  c( 0.05, -0.05,  0.10,  0.00, -0.10,  0.08, -0.06,  0.12),   ## Status Quo
  c( 0.45, -0.25,  0.30,  0.55, -0.40,  0.35,  0.65, -0.15),   ## Scenario X
  c(-0.30,  0.50, -0.20,  0.10,  0.60, -0.45,  0.05,  0.45),   ## Scenario Y
  c( 0.20,  0.20,  0.15,  0.35,  0.15, -0.10,  0.30,  0.25)    ## Scenario X+Y
)

all_metric_values <- cbind(named_metric_values, filler_metric_values)
colnames(all_metric_values) <- metric_names
rownames(all_metric_values) <- scenario_names

## -----------------------------------------------------------------------------
## Build a data frame in the shape fmsb::radarchart requires:
## row 1 = max values, row 2 = min values, rows 3+ = scenarios

make_spider_data <- function(n_metrics) {

  vals <- all_metric_values[, seq_len(n_metrics), drop = FALSE]

  spider_data <- rbind(
    rep(1, n_metrics),   ## Max
    rep(-1, n_metrics),  ## Min
    vals
  )

  spider_data <- as.data.frame(spider_data)
  rownames(spider_data) <- c("Max", "Min", scenario_names)

  return(spider_data)
}

## -----------------------------------------------------------------------------
## Draws behind the box-and-whisker plots
##
## The box plots show a spread of points around the same means used by the
## spider and bar plots, standing in for replicate runs or years of output.
## The draws are random but seeded, so the figures are reproducible; RNGversion
## is pinned so they survive an R upgrade as well.

n_draws <- 9      ## points behind each box
box_sd  <- 0.12   ## spread of those points, on the -1 to 1 scale

RNGversion("4.5.0")
set.seed(42)

## Draw the full scenario x metric x n_draws array ONCE, up front. Generating it
## inside the figure loop instead would give the 4-, 8- and 12-metric figures
## different points for the same scenario and metric.
all_metric_draws <- array(
  pmin(1, pmax(-1, rnorm(
    n = length(scenario_names) * length(metric_names) * n_draws,
    mean = rep(as.vector(all_metric_values), each = n_draws),
    sd = box_sd
  ))),
  dim = c(n_draws, length(scenario_names), length(metric_names)),
  dimnames = list(NULL, scenario_names, metric_names)
)

## -----------------------------------------------------------------------------
## Long data frame of draws for the first n_metrics metrics:
## one row per scenario x metric x draw, ready for boxplot()

make_box_data <- function(n_metrics) {

  keep_metrics <- metric_names[seq_len(n_metrics)]
  draws        <- all_metric_draws[, , keep_metrics, drop = FALSE]

  box_data <- data.frame(
    scenario = factor(rep(scenario_names, each = n_draws, times = n_metrics),
                      levels = scenario_names),
    metric   = factor(rep(keep_metrics, each = n_draws * length(scenario_names)),
                      levels = keep_metrics),
    value    = as.vector(draws)
  )

  return(box_data)
}

## -----------------------------------------------------------------------------
## Axis label size shrinks as the number of metrics grows, otherwise the labels
## collide. Panels are a quarter the size of a full figure, so shrink further.

label_cex_for <- function(n_metrics, panel = FALSE) {

  base_cex <- if(n_metrics <= 4)      1.00
              else if(n_metrics <= 8) 0.85
              else                    0.75

  if(panel) base_cex - 0.10 else base_cex
}

## -----------------------------------------------------------------------------
## Figure helper -- honors the save_png flag
##
## The device is closed by on.exit(), so it closes even if the plotting code
## errors. That matters: a leaked open device holds a lock on its PNG file on
## Windows, which blocks every later run from rewriting that file.

save_figure <- function(file_name, width, height, draw_plot) {

  if(!save_png) {
    draw_plot()
    return(invisible(NULL))
  }

  png(file.path(out_dir, file_name), width = width, height = height, res = 150)
  on.exit(dev.off(), add = TRUE)

  draw_plot()
}

## -----------------------------------------------------------------------------
## Plot type 1: all scenarios overlaid on one spider plot

plot_spider_overlaid <- function(spider_data, plot_title) {

  label_cex <- label_cex_for(ncol(spider_data))

  ## radarchart() hardcodes plot(xlim = c(-1.2, 1.2), asp = 1) and draws the
  ## metric labels at 1.2 -- right on the x limit. Room for those labels comes
  ## from a plot region that is WIDER than it is tall, not from bigger margins,
  ## so keep the left/right mar small and the device wide.

  par(mfrow = c(1, 1), mar = c(2, 1, 4, 1))

  radarchart(
    spider_data,
    axistype = 1,
    seg = 4,          ## 4 rings so all 5 caxislabels are used (-1.0 - 1.0)

    ## Polygon line settings
    pcol = line_colors,
    plwd = 2,
    plty = 1,

    ## Fill colors with transparency
    pfcol = adjustcolor(line_colors, alpha.f = fill_alpha_overlaid),

    ## Grid and axis styling
    cglcol = "grey70",
    cglty = 1,
    cglwd = 0.8,
    axislabcol = "grey30",
    vlcex = label_cex,

    ## Axis labels
    caxislabels = c("-1.0", "-0.5", "0.0", "0.5", "1.0"),

    title = plot_title
  )

  legend(
    "topright",
    legend = scenario_names,
    col    = line_colors,
    lty    = 1,
    lwd    = 2,
    bty    = "o",
    cex    = 0.9
  )
}

## -----------------------------------------------------------------------------
## Plot type 2: one spider plot per scenario, in a 2x2 panel

plot_spider_separate <- function(spider_data, plot_title) {

  label_cex <- label_cex_for(ncol(spider_data), panel = TRUE)

  ## radarchart() hardcodes plot(xlim = c(-1.2, 1.2), asp = 1) and draws the
  ## metric labels at 1.2 -- right on the x limit. Room for those labels comes
  ## from a plot region that is WIDER than it is tall, not from bigger margins,
  ## so keep the left/right mar small and the device wide.

  par(
    mfrow = c(2, 2),
    mar   = c(1, 0.5, 3, 0.5),
    oma   = c(0, 0, 3, 0)
  )

  for(i in seq_along(scenario_names)) {

    ## Keep only Max, Min, and the current scenario
    plot_data <- spider_data[c("Max", "Min", scenario_names[i]), ]

    radarchart(
      plot_data,
      axistype = 1,
      seg = 4,          ## 4 rings so all 5 caxislabels are used (-1.0 - 1.0)
      pcol = line_colors[i],
      pfcol = adjustcolor(line_colors[i], alpha.f = fill_alpha_separate),
      plwd = 2,
      plty = 1,
      cglcol = "grey70",
      cglty = 1,
      cglwd = 0.8,
      axislabcol = "grey30",
      vlcex = label_cex,
      caxislabels = c("-1.0", "-0.5", "0.0", "0.5", "1.0"),
      title = scenario_names[i]
    )
  }

  mtext(plot_title, outer = TRUE, cex = 1.2, font = 2)
}

## -----------------------------------------------------------------------------
## Bar plot helpers
##
## The spider data frame carries Max/Min rows that radarchart needs; barplot
## wants the scenario rows only. Metric names are long, so they are rotated
## (las = 2) and the bottom margin is sized from the longest one.

bar_values <- function(spider_data) {
  as.matrix(spider_data[scenario_names, , drop = FALSE])
}

bottom_margin_for <- function(metric_labels, label_cex) {
  2 + 0.42 * max(nchar(metric_labels)) * label_cex
}

## -----------------------------------------------------------------------------
## Plot type 3: grouped bar plot -- all scenarios side by side for each metric
##              (the bar analogue of the overlaid spider plot)

plot_bars_grouped <- function(spider_data, plot_title) {

  vals      <- bar_values(spider_data)
  label_cex <- label_cex_for(ncol(vals))

  par(
    mfrow = c(1, 1),
    mar   = c(bottom_margin_for(colnames(vals), label_cex), 4, 6, 2)
  )

  barplot(
    vals,
    beside    = TRUE,
    col       = line_colors,
    border    = "grey30",
    ylim      = c(-1, 1),
    ylab      = "Metric value",
    main      = plot_title,
    las       = 2,
    cex.names = label_cex,
    cex.axis  = 0.9
  )

  ## Zero line -- the reference the signed metrics are read against
  abline(h = 0, col = "grey30", lwd = 1)

  ## Legend sits in the top margin so it never collides with a tall bar
  legend(
    "top",
    inset  = c(0, -0.09),
    xpd    = TRUE,
    horiz  = TRUE,
    legend = scenario_names,
    fill   = line_colors,
    border = "grey30",
    bty    = "n",
    cex    = 0.9
  )
}

## -----------------------------------------------------------------------------
## Plot type 4: one bar plot per scenario, in a 2x2 panel
##              (the bar analogue of the separated spider plots)

plot_bars_separate <- function(spider_data, plot_title) {

  vals      <- bar_values(spider_data)
  label_cex <- label_cex_for(ncol(vals), panel = TRUE)

  par(
    mfrow = c(2, 2),
    mar   = c(bottom_margin_for(colnames(vals), label_cex), 4, 3, 1),
    oma   = c(0, 0, 3, 0)
  )

  for(i in seq_along(scenario_names)) {

    barplot(
      vals[i, ],
      col       = line_colors[i],
      border    = "grey30",
      ylim      = c(-1, 1),
      ylab      = "Metric value",
      main      = scenario_names[i],
      las       = 2,
      cex.names = label_cex,
      cex.axis  = 0.9
    )

    abline(h = 0, col = "grey30", lwd = 1)
  }

  mtext(plot_title, outer = TRUE, cex = 1.2, font = 2)
}

## -----------------------------------------------------------------------------
## Box plot helpers

## Boxes are built from only 9 points, so the box alone overstates precision.
## Overlay the draws themselves -- jittered so identical values do not hide.
add_draw_points <- function(values, at_x) {
  points(
    jitter(rep(at_x, length(values)), amount = 0.12),
    values,
    pch = 16,
    cex = 0.5,
    col = adjustcolor("grey20", alpha.f = 0.6)
  )
}

## -----------------------------------------------------------------------------
## Plot type 5: grouped box plot -- all scenarios side by side for each metric
##              (the box analogue of the grouped bar plot)

plot_box_grouped <- function(box_data, plot_title) {

  metric_labels <- levels(box_data$metric)
  n_scen        <- length(scenario_names)
  label_cex     <- label_cex_for(length(metric_labels))

  ## One cluster of n_scen boxes per metric, with a gap between clusters
  box_at    <- as.vector(outer(seq_len(n_scen), (seq_along(metric_labels) - 1) * (n_scen + 1), "+"))
  cluster_mid <- (seq_along(metric_labels) - 1) * (n_scen + 1) + (n_scen + 1) / 2

  values_by_box <- split(box_data$value, list(box_data$scenario, box_data$metric))

  par(
    mfrow = c(1, 1),
    mar   = c(bottom_margin_for(metric_labels, label_cex), 4, 6, 2)
  )

  boxplot(
    values_by_box,
    at       = box_at,
    col      = rep(line_colors, times = length(metric_labels)),
    border   = "grey30",
    ylim     = c(-1, 1),
    ylab     = "Metric value",
    main     = plot_title,
    xaxt     = "n",
    frame.plot = FALSE,
    outline  = FALSE,   ## the draws are added below, do not draw them twice
    cex.axis = 0.9
  )

  for(i in seq_along(values_by_box)) {
    add_draw_points(values_by_box[[i]], box_at[i])
  }

  abline(h = 0, col = "grey30", lwd = 1)

  ## Metric names sit under the middle of each cluster
  axis(1, at = cluster_mid, labels = metric_labels, las = 2, cex.axis = label_cex, tick = FALSE)

  legend(
    "top",
    inset  = c(0, -0.09),
    xpd    = TRUE,
    horiz  = TRUE,
    legend = scenario_names,
    fill   = line_colors,
    border = "grey30",
    bty    = "n",
    cex    = 0.9
  )
}

## -----------------------------------------------------------------------------
## Plot type 6: one box plot per scenario, in a 2x2 panel
##              (the box analogue of the separated bar plots)

plot_box_separate <- function(box_data, plot_title) {

  metric_labels <- levels(box_data$metric)
  label_cex     <- label_cex_for(length(metric_labels), panel = TRUE)

  par(
    mfrow = c(2, 2),
    mar   = c(bottom_margin_for(metric_labels, label_cex), 4, 3, 1),
    oma   = c(0, 0, 3, 0)
  )

  for(i in seq_along(scenario_names)) {

    scenario_rows <- box_data[box_data$scenario == scenario_names[i], ]
    values_by_box <- split(scenario_rows$value, scenario_rows$metric)

    boxplot(
      values_by_box,
      col       = line_colors[i],
      border    = "grey30",
      ylim      = c(-1, 1),
      ylab      = "Metric value",
      main      = scenario_names[i],
      las       = 2,
      frame.plot = FALSE,
      outline   = FALSE,
      cex.axis  = label_cex
    )

    for(j in seq_along(values_by_box)) {
      add_draw_points(values_by_box[[j]], j)
    }

    abline(h = 0, col = "grey30", lwd = 1)
  }

  mtext(plot_title, outer = TRUE, cex = 1.2, font = 2)
}

## -----------------------------------------------------------------------------
## Make all 18 figures

for(n_metrics in metric_counts) {

  spider_data <- make_spider_data(n_metrics)
  box_data    <- make_box_data(n_metrics)

  plot_title <- paste0("Example scenario comparison -- ", n_metrics, " metrics")

  ## Overlaid
  save_figure(
    sprintf("spider-overlaid-%02d-metrics.png", n_metrics),
    width = 1300, height = 1050,
    draw_plot = function() plot_spider_overlaid(spider_data, plot_title)
  )

  ## Separated
  save_figure(
    sprintf("spider-separate-%02d-metrics.png", n_metrics),
    width = 1600, height = 1400,
    draw_plot = function() plot_spider_separate(spider_data, plot_title)
  )

  ## Bars, grouped
  save_figure(
    sprintf("bar-grouped-%02d-metrics.png", n_metrics),
    width = 800 + 60 * n_metrics, height = 1000,
    draw_plot = function() plot_bars_grouped(spider_data, plot_title)
  )

  ## Bars, separated
  save_figure(
    sprintf("bar-separate-%02d-metrics.png", n_metrics),
    width = 1600, height = 1400,
    draw_plot = function() plot_bars_separate(spider_data, plot_title)
  )

  ## Boxes, grouped
  save_figure(
    sprintf("box-grouped-%02d-metrics.png", n_metrics),
    width = 800 + 60 * n_metrics, height = 1000,
    draw_plot = function() plot_box_grouped(box_data, plot_title)
  )

  ## Boxes, separated
  save_figure(
    sprintf("box-separate-%02d-metrics.png", n_metrics),
    width = 1600, height = 1400,
    draw_plot = function() plot_box_separate(box_data, plot_title)
  )
}

## -----------------------------------------------------------------------------
## Confirm the figures really were rewritten by THIS run, rather than reporting
## success while stale files sit on disk

if(save_png) {

  expected_files <- c(
    sprintf("spider-overlaid-%02d-metrics.png", metric_counts),
    sprintf("spider-separate-%02d-metrics.png", metric_counts),
    sprintf("bar-grouped-%02d-metrics.png",    metric_counts),
    sprintf("bar-separate-%02d-metrics.png",   metric_counts),
    sprintf("box-grouped-%02d-metrics.png",    metric_counts),
    sprintf("box-separate-%02d-metrics.png",   metric_counts)
  )

  file_stamps <- file.info(file.path(out_dir, expected_files))$mtime
  not_rewritten <- expected_files[is.na(file_stamps) | file_stamps < run_started]

  if(length(not_rewritten) > 0) {
    stop("These figures were NOT rewritten:\n  ",
         paste(not_rewritten, collapse = "\n  "),
         "\n  The file is most likely locked -- close it in any image viewer or",
         "\n  Explorer preview pane, then re-run.")
  }

  cat("Wrote", length(expected_files), "figures to", normalizePath(out_dir), "\n")
}

## -----------------------------------------------------------------------------
## Reset plotting window

par(mfrow = c(1, 1), mar = c(5, 4, 4, 2), oma = c(0, 0, 0, 0))
