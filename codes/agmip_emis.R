library(gamstransfer)
library(tidyverse)
source("./codes/mapping_logic.R")

# 1. Load Data
m <- Container$new("./input/Results2025_final.gdx")
df_emis <- m["CARBON_SINK"]$records
colnames(df_emis) <- c("region","item", "unit" ,"ssp","scen1","For_Scen","year","value")

# 2. Reference Mappings
scen_df <- tibble::tribble(
  ~For_Scen,   ~scens,
  "RCP4p5",    "BASE", "RCP4p5_1",  "FP", "RCP4p5_2",  "EA",
  "RCP4p5",    "Base_FPSOC", "RCP4p5", "Base_EASOC", "RCP4p5", "Base_FPWET",
  "RCP4p5",    "Base_EAWET", "RCP4p5", "Base_FPCAP", "RCP4p5", "Base_EACAP",
  "RCP4p5_1",  "Base_FPWDM", "RCP4p5_2", "Base_EAWDM"
)

# --- NEW FILTER STEP TO PREVENT DOUBLE COUNTING ---
# We define a list of 'preferred' region labels to avoid overlapping data.
# Since your list has both "Austria" and "AUTReg", we choose only the "Reg" versions.
preferred_regions <- unique(df_emis$region)[grepl("Reg$", unique(df_emis$region)) | 
                                              unique(df_emis$region) %in% c("RCEU", "ROWE", "CongoBasin", "RCAM", "RSAM", "RSAS", "RSEA_OPA", "RSEA_PAC")]

# --- REFINED FILTER TO ELIMINATE DOUBLE COUNTING ---

# 1. Define the "Official" Model Regions (the aggregates)
primary_agmip_regions <- c(
  # Regional Aggregates
  "ANZ", "ArgentinaReg", "BrazilReg", "CanadaReg", "ChinaReg", "CongoBasin", 
  "Former_USSR", "IndiaReg", "IndonesiaReg", "JapanReg", "MalaysiaReg", 
  "MexicoReg", "MidEastNorthAfr", "Pacific_Islands", "RCAM", "RCEU", "ROWE", 
  "RSAM", "RSAS", "RSEA_OPA", "RSEA_PAC", "SouthAfrReg", "SouthKorea", 
  "EasternAf", "SouthernAf", "WesternAf", "TurkeyReg", "UkraineReg", "USAReg",
  
  # Individual European "Reg" units
  "AUTReg", "BELReg", "BGRReg", "CYPReg", "CZEReg", "DEUReg", "DNKReg", 
  "ESPReg", "ESTReg", "FINReg", "FRAReg", "GBRReg", "GRCReg", "HRVReg", 
  "HUNReg", "IRLReg", "ITAReg", "LTUReg", "LUXReg", "LVAReg", "MLTReg", 
  "NLDReg", "POLReg", "PRTReg", "ROUReg", "SVKReg", "SVNReg", "SWEReg", 
  "NorwayReg"
)


# 2. Filter df_emis to ONLY keep these primary regions
# This automatically drops "Argentina", "Brazil", "Albania", etc.
df_emis_filtered <- df_emis %>%
  filter(region %in% primary_agmip_regions)

# --------------------------------------------------
# If the "Reg" labels are missing for some countries, you might need to be more specific.
# Here, we filter the dataframe to only include the "Reg" labels for Europe.
df_emis_filtered <- df_emis_filtered %>%
  filter(
    # Keep specific regional aggregates
    region %in% preferred_regions | 
      # Keep others that don't have a "Reg" equivalent in your list
      (!region %in% c("Austria", "Belgium", "Germany", "France", "Italy", "Spain", "Netherlands")) 
  )
# --------------------------------------------------

# 2. Process EMISSIONS (using the filtered dataframe)
df_emis_clean <- df_emis_filtered %>% 
  filter(item %in% c("Forest", "HWP")) %>% 
  group_by(region, For_Scen, year) %>% 
  summarise(value = sum(value), .groups = "drop") %>%
  mutate(item_list = list(c("FOR", "TOT"))) %>% unnest(item_list) %>%
  mutate(variable_list = list(c("EMIS", "ECO2"))) %>% unnest(variable_list) %>%
  rename(item = item_list, variable = variable_list) %>%
  inner_join(scen_df, by = "For_Scen", relationship = "many-to-many") %>%
  inner_join(full_mapping, by = c("region" = "source_label")) %>%
  group_by(agmip_target, variable, item, scens, year) %>%
  summarise(value = sum(value, na.rm = TRUE), .groups = "drop")

# Step A: Format Forest Emissions to match Main Result structure
forest_to_add <- df_emis_clean %>%
  # Match column names exactly
  rename(
    Region = agmip_target,
    Variable = variable,
    Item = item,
    Scenario = scens, # We fix case in next step
    Year = year,
    Value = value
  ) %>%
  mutate(
    Year = as.integer(as.character(Year)), # Ensure Year is integer
    Model = "GLOBIOM",
    Unit = "MtCO2e",
  ) %>%
  select(Model, Scenario, Region, Item, Variable, Year, Unit, Value)

saveRDS(forest_to_add, "processed_forest_emis.rds")
