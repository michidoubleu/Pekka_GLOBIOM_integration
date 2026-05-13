df_area <- m["Harvest_area_compare"]$records
colnames(df_area) <- c("region","item","ssp","scen1","For_Scen","year","value")

# 2. Reference Mappings
scen_df <- tibble::tribble(
  ~For_Scen,   ~scens,
  "RCP4p5",    "BASE", 
  "RCP4p5_1",  "FP", 
  "RCP4p5_2",  "EA",
  "RCP4p5",    "Base_FPSOC", 
  "RCP4p5",    "Base_EASOC",
  "RCP4p5",    "Base_FPSWF", 
  "RCP4p5",    "Base_EASWF",
  "RCP4p5",    "Base_FPFRT", 
  "RCP4p5",    "Base_EAFRT",
  "RCP4p5",    "Base_FPPST", 
  "RCP4p5",    "Base_EAPST",
  "RCP4p5",    "Base_FPORG", 
  "RCP4p5",    "Base_EAORG",
  "RCP4p5",    "Base_FPWET",
  "RCP4p5",    "Base_EAWET", 
  "RCP4p5",    "Base_FPCAP", 
  "RCP4p5",    "Base_EACAP",
  "RCP4p5",    "Base_FPWDM", 
  "RCP4p5",    "Base_EAWDM",
  "RCP4p5_1",  "Base_FPFRS", 
  "RCP4p5_2",  "Base_EAFRS"
)

# Use your mapping logic from the previous prompt here
# (Defining mapping_data and full_mapping)
source("./codes/mapping_logic.R") # Recommended: put the tribble in a separate file to keep this clean

# 3. Process AREA
#df_area_filtered <- filter_area_safely(df_area)

df_area_clean <- df_area %>% 
  filter(item == "Affor") %>%
  mutate(
    value = value * 1000,
    variable = "AREA",
    item = "FOR"
  ) %>%
  select(region, For_Scen, variable, item, year, value) %>%
  inner_join(scen_df, by = "For_Scen", relationship = "many-to-many") %>%
  inner_join(full_mapping, by = c("region" = "source_label")) %>%
  group_by(agmip_target, variable, item, scens, year) %>%
  summarise(value = sum(value, na.rm = TRUE), .groups = "drop") %>% filter(scens=="BASE")



# 1. Load Data
df_emis <- m["CARBON_SINK"]$records
colnames(df_emis) <- c("region","item", "unit" ,"ssp","scen1","For_Scen","year","value")


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
  filter(item %in% c("Forest_affor")) %>% 
  group_by(region, For_Scen, year) %>% 
  summarise(value = sum(value), .groups = "drop") %>%
  mutate(variable = "EMIS",
         item = "FOR") %>%
  inner_join(scen_df, by = "For_Scen", relationship = "many-to-many") %>%
  inner_join(full_mapping, by = c("region" = "source_label")) %>%
  group_by(agmip_target, variable, item, scens, year) %>%
  summarise(value = sum(value, na.rm = TRUE), .groups = "drop") %>% filter(scens=="BASE")


conv.factor <- bind_rows(df_area_clean, df_emis_clean) %>% pivot_wider(names_from = "variable", values_from = "value") %>% mutate(sink.fac=EMIS/AREA) %>% dplyr::select(agmip_target, item, year, sink.fac)

saveRDS(conv.factor, file = "output/copy2openinput/affor_emis_factors.rds")
