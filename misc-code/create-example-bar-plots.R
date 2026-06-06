## -----------------------------------------------------------------------------
## Example multi-panel bar plots for Ecospace outputs

rm(list = ls())

## -----------------------------------------------------------------------------
## Example scenario names

scenario_names <- c(
  "Scenario 1",
  "Scenario 2",
  "Scenario 3",
  "Scenario 4",
  "Scenario 5",
  "Scenario 6"
)

## Set one color per scenario
scenario_colors <- c("red", "blue", "darkgreen", "purple", "orange", "brown")

## -----------------------------------------------------------------------------
## Example output data
## Rows = scenarios
## Columns = output metrics

plot_data <- data.frame(
  scenario = factor(scenario_names, levels = scenario_names),
  biomass_preharvest_bcf = c(120, 135, 110, 145, 130, 125),
  catches_harvest_bcf    = c( 18,  24,  15,  27,  22,  20),
  catches_trophy_bcf     = c(  6,   8,   5,   9,   7,   6),
  effort_bcf_rec_fleets  = c( 90, 105,  85, 115, 100,  95),
  catches_blue_crab      = c( 40,  38,  42,  35,  37,  39),
  catches_striped_bass   = c( 25,  22,  27,  20,  23,  24),
  reduced_bcf_predation  = c(0.10, 0.18, 0.12, 0.25, 0.16, 0.14),
  ecological_composite   = c(0.62, 0.68, 0.59, 0.71, 0.66, 0.64)
)

## -----------------------------------------------------------------------------
## Panel labels and matching column names

panel_info <- data.frame(
  col_name = c(
    "biomass_preharvest_bcf",
    "catches_harvest_bcf",
    "catches_trophy_bcf",
    "effort_bcf_rec_fleets",
    "catches_blue_crab",
    "catches_striped_bass",
    "reduced_bcf_predation",
    "ecological_composite"
  ),
  panel_title = c(
    "Biomass: pre-harvest BCF",
    "Catches: harvest BCF",
    "Catches: trophy BCF",
    "Effort: BCF rec fleets",
    "Catches: Blue crab",
    "Catches: Striped bass",
    "Reduced BCF predation",
    "Ecological composite metric"
  ),
  stringsAsFactors = FALSE
)

## -----------------------------------------------------------------------------
## Plot settings

par(
  mfrow = c(2, 4),
  mar   = c(7, 4, 3, 1),
  oma   = c(0, 0, 2, 0)
)

## -----------------------------------------------------------------------------
## Loop through panels

for(i in seq_len(nrow(panel_info))) {
  
  y_vals <- plot_data[[panel_info$col_name[i]]]
  
  bp <- barplot(
    height    = y_vals,
    names.arg = scenario_names,
    col       = scenario_colors,
    border    = "black",
    las       = 2,
    main      = panel_info$panel_title[i],
    ylab      = "",
    cex.names = 0.9,
    cex.main  = 1.0
  )
}

## -----------------------------------------------------------------------------
## Optional overall title

mtext(
  "Example Ecospace Outputs Across Scenarios",
  outer = TRUE,
  cex   = 1.3,
  font  = 2
)

## -----------------------------------------------------------------------------
## Reset plotting layout

par(mfrow = c(1, 1))