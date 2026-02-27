library(gamstransfer)
library(tidyverse)

# 1. Load Data
m <- Container$new("./input/Results2025_final.gdx")
df_area <- m["Harvest_area_compare"]$records
colnames(df_area) <- c("region","item","ssp","scen1","For_Scen","year","value")

# 2. Reference Mappings
scen_df <- tibble::tribble(
  ~For_Scen,   ~scens,
  "RCP4p5",    "BASE", "RCP4p5_1",  "FP", "RCP4p5_2",  "EA",
  "RCP4p5",    "Base_FPSOC", "RCP4p5", "Base_EASOC", "RCP4p5", "Base_FPWET",
  "RCP4p5",    "Base_EAWET", "RCP4p5", "Base_FPCAP", "RCP4p5", "Base_EACAP",
  "RCP4p5_1",  "Base_FPWDM", "RCP4p5_2", "Base_EAWDM"
)

# Use your mapping logic from the previous prompt here
# (Defining mapping_data and full_mapping)
source("./codes/mapping_logic.R") # Recommended: put the tribble in a separate file to keep this clean

# 3. Process AREA
df_area_filtered <- filter_area_safely(df_area)

df_area_clean <- df_area_filtered %>% 
  filter(item == "total_forest_area") %>%
  mutate(
    value = value * 1000,
    variable = "AREA",
    item = "FOR"
  ) %>%
  select(region, For_Scen, variable, item, year, value) %>%
  inner_join(scen_df, by = "For_Scen", relationship = "many-to-many") %>%
  inner_join(full_mapping, by = c("region" = "source_label")) %>%
  group_by(agmip_target, variable, item, scens, year) %>%
  summarise(value = sum(value, na.rm = TRUE), .groups = "drop")

saveRDS(df_area_clean, "processed_forest_area.rds")