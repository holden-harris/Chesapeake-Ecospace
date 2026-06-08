# Clear environment and load packages
rm(list = ls()); gc()
library(dplyr)
library(rfishbase)

#------------------------------------------------------------------------------
# Set folder directories
dir_in  <- "./data-inputs/species-info"
dir_out <- "./data-inputs/species-info/aquamaps-species-info"

dir.create(dir_out, showWarnings = FALSE, recursive = TRUE)

#------------------------------------------------------------------------------
# Load species list
sp_list <- read.csv(paste0(dir_in, "/species-list.csv"))
# A species list will need a column with 'scientific_name' to match in FishBase

#------------------------------------------------------------------------------
# Query FishBase tables
# These API calls may take time depending on list size
spp_raw     <- rfishbase::species(sp_list$scientific_name);      print("Species")
growth_raw  <- rfishbase::popgrowth(sp_list$scientific_name);    print("Growth")
ecology_raw <- rfishbase::ecology(sp_list$scientific_name);      print("Ecology")
repro_raw   <- rfishbase::reproduction(sp_list$scientific_name); print("Reproduction")
stocks_raw  <- rfishbase::stocks(sp_list$scientific_name);       print("Stocks (env/climate)")
oxygen_raw  <- rfishbase::oxygen(sp_list$scientific_name);       print("Oxygen")
speed_raw   <- rfishbase::speed(sp_list$scientific_name);        print("Speed")

write.csv(spp_raw,     file.path(dir_out, "aquamaps-info_species.csv"), row.names = FALSE)
write.csv(growth_raw,  file.path(dir_out, "aquamaps-info_growth.csv"), row.names = FALSE)
write.csv(ecology_raw, file.path(dir_out, "aquamaps-info_ecology.csv"), row.names = FALSE)
write.csv(repro_raw,   file.path(dir_out, "aquamaps-info_repro.csv"), row.names = FALSE)
write.csv(stocks_raw,  file.path(dir_out, "aquamaps-info_stocks.csv"), row.names = FALSE)
write.csv(oxygen_raw,  file.path(dir_out, "aquamaps-info_oxygen.csv"), row.names = FALSE)
write.csv(speed_raw,   file.path(dir_out, "aquamaps-info_speed.csv"), row.names = FALSE)

#------------------------------------------------------------------------------
# Select and join species-level attributes
selected_cols_species <- c(
  "Species", "FBname", "BodyShapeI",
  "DepthRangeShallow", "DepthRangeDeep", 
  "DepthRangeComShallow", "DepthRangeComDeep", 
  "LongevityWild", "LongevityCaptive", 
  "Vulnerability", "VulnerabilityClimate", 
  "Length", "CommonLength", "Weight"
)

sp_attributes <- sp_list %>% # join selected columns into sp_list
  left_join(select(spp_raw, all_of(selected_cols_species)),
            by = c("scientific_name" = "Species"))

#------------------------------------------------------------------------------
# Summarize growth data (K, Loo) by species
growth_summary <- growth_raw %>%
  group_by(Species) %>%
  summarise(
    n_growth_studies = n(),
    K_avg   = mean(K, na.rm = TRUE),
    Loo_avg = mean(Loo, na.rm = TRUE),
    .groups = "drop"
  )

sp_attributes <- sp_attributes %>% # left-join into species attributes dataframe
  left_join(growth_summary, by = c("scientific_name" = "Species"))

#------------------------------------------------------------------------------
# Select and join reproduction fields
selected_cols_repro <- c(
  "Species", "ReproMode", "Spawning", 
  "RepGuild1", "RepGuild2", "AddInfos"
)

sp_attributes <- sp_attributes %>%
  left_join(select(repro_raw, all_of(selected_cols_repro)),
            by = c("scientific_name" = "Species"))

#------------------------------------------------------------------------------
# Select and join ecological traits (trophic level, remarks)
selected_cols_ecology <- c(
  "Species", "DietTroph", "DietSeTroph", 
  "DietRemark", "AddRems"
)

sp_attributes <- sp_attributes %>%
  left_join(select(ecology_raw, all_of(selected_cols_ecology)),
            by = c("scientific_name" = "Species"))

#------------------------------------------------------------------------------
# Write out table
write.csv(sp_attributes, 
          file.path(dir_out, "species_attributes.csv"), row.names = FALSE)
