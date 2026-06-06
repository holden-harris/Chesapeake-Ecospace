## -----------------------------------------------------------------------------
## Example spider plot with 5 metrics and 4 scenarios

library(fmsb)

## Create example data
## fmsb::radarchart requires:
## row 1 = max values
## row 2 = min values
## rows 3+ = scenarios

spider_data <- data.frame(
  Economic_metric            = c(1, 0, 0.70, 0.60, 0.80, 0.20),
  Striped_bass_metric        = c(1, 0, 0.50, 0.85, 0.70, 0.65),
  Bluecatfish_metric         = c(1, 0, 0.80, 0.55, 0.65, 0.30),
  Another_bluecatfish_metric = c(1, 0, 0.60, 0.75, 0.20, 0.85),
  Bluecrab_metric            = c(1, 0, 0.90, 0.25, 0.60, 0.70)
)

rownames(spider_data) <- c(
  "Max",
  "Min",
  "Scenario 1",
  "Scenario 2",
  "Scenario 3",
  "Scenario 4"
)

## Scenario names
scenario_names <- c("Scenario 1", "Scenario 2", "Scenario 3", "Scenario 4")

## Set line colors
line_colors <- c("red", "blue", "darkgreen", "purple")

## -----------------------------------------------------------------------------
## Plot 1: Overlaid spider plot with all 4 scenarios

par(mfrow = c(1, 1), mar = c(2, 2, 3, 5))

radarchart(
  spider_data,
  axistype = 1,
  
  ## Polygon line settings
  pcol = line_colors,
  plwd = 2,
  plty = 1,
  
  ## Fill colors with transparency
  pfcol = adjustcolor(line_colors, alpha.f = 0.15),
  
  ## Grid and axis styling
  cglcol = "grey70",
  cglty = 1,
  cglwd = 0.8,
  axislabcol = "grey30",
  vlcex = 1.0,
  
  ## Axis labels
  caxislabels = c("0.0", "0.2", "0.4", "0.6", "0.8", "1.0"),

)

legend(
  "topright",
  legend = scenario_names,
  col    = line_colors,
  lty    = 1,
  lwd    = 2,
  bty    = "y",
  cex    = 0.9
)

## -----------------------------------------------------------------------------
## Plot 2: Four separate spider plots, one per scenario

par(mfrow = c(2, 2), mar = c(2, 2, 3, 2))

for(i in seq_along(scenario_names)) {
  
  ## Keep only Max, Min, and the current scenario
  plot_data <- spider_data[c("Max", "Min", scenario_names[i]), ]
  
  radarchart(
    plot_data,
    axistype = 1,
    pcol = line_colors[i],
    pfcol = adjustcolor(line_colors[i], alpha.f = 0.25),
    plwd = 2,
    plty = 1,
    cglcol = "grey70",
    cglty = 1,
    cglwd = 0.8,
    axislabcol = "grey30",
    vlcex = 0.9,
    caxislabels = c("0.0", "0.2", "0.4", "0.6", "0.8", "1.0"),
    title = scenario_names[i]
  )
}

## Reset plotting window
par(mfrow = c(1, 1), mar = c(5, 4, 4, 2))

