## -----------------------------------------------------------------------------
## Example spider plot with 8 metrics and 6 scenarios

library(fmsb)

## Create example data
## fmsb::radarchart requires the first two rows to be max and min values
spider_data <- data.frame(
  Metric_1 = c(1, 0, 0.80, 0.55, 0.65, 0.40, 0.90, 0.70),
  Metric_2 = c(1, 0, 0.60, 0.75, 0.50, 0.85, 0.45, 0.70),
  Metric_3 = c(1, 0, 0.70, 0.60, 0.80, 0.50, 0.65, 0.90),
  Metric_4 = c(1, 0, 0.90, 0.45, 0.60, 0.70, 0.55, 0.80),
  Metric_5 = c(1, 0, 0.50, 0.85, 0.70, 0.65, 0.40, 0.75),
  Metric_6 = c(1, 0, 0.65, 0.70, 0.55, 0.90, 0.60, 0.45),
  Metric_7 = c(1, 0, 0.75, 0.50, 0.85, 0.60, 0.70, 0.55),
  Metric_8 = c(1, 0, 0.55, 0.90, 0.45, 0.75, 0.80, 0.65)
)

rownames(spider_data) <- c(
  "Max", "Min",
  "Scenario 1", "Scenario 2", "Scenario 3",
  "Scenario 4", "Scenario 5", "Scenario 6"
)

## Set line colors
line_colors <- c("red", "blue", "darkgreen", "purple", "orange", "brown")

## Plot
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
  
  ## Optional axis labels
  caxislabels = c("0.0", "0.2", "0.4", "0.6", "0.8", "1.0"),
  
  ## Title
  #title = "Example Spider Plot"
)

## Add legend
legend(
  "bottomright",
  legend = paste("Scenario", 1:6),
  col = line_colors,
  lty = 1,
  lwd = 2,
  bty = "y",
  cex = .9
)

## -----------------------------------------------------------------------------
## Example spider plot with 8 metrics and 6 separate scenario plots

## Set up multi-panel plotting window: 2 rows x 3 columns
par(mfrow = c(2, 3), mar = c(2, 2, 3, 2))

## Loop through scenarios
for(i in seq_along(scenario_names)) {
  
  ## Keep only Max, Min, and the current scenario
  plot_data <- spider_data[c("Max", "Min", scenario_names[i]), ]
  
  radarchart(
    plot_data,
    axistype = 1,
    pcol = scenario_colors[i],
    pfcol = adjustcolor(scenario_colors[i], alpha.f = 0.25),
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
par(mfrow = c(1, 1), mar = c(2, 2, 3, 2))
