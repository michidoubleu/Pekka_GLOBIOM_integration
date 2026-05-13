library(gamstransfer)
library(tidyverse)

# 1. Load Data
df_prod <- m["Production_compare"]$records
colnames(df_prod) <- c("region","item","ssp","scen1","For_Scen","year","value")

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
  "RCP4p5_1",  "Base_FPWDM", 
  "RCP4p5_2",  "Base_EAWDM",
  "RCP4p5",    "Base_FPFRS", 
  "RCP4p5",    "Base_EAFRS"
)


# 2. Map, Filter, and Aggregate
df_prod_clean <- df_prod %>%
  mutate(
    variable = "FPROD",
    # Map the raw items to your target reporting codes
    item_mapped = case_when(
      item == "WoodChips_C"   ~ "FOR|WCC",
      item == "WoodChips_NC"  ~ "FOR|WCN",
      item == "Sawdust_C"     ~ "FOR|SDC",
      item == "Sawdust_NC"    ~ "FOR|SDN",
      item == "PW_Biomass_C"  ~ "FOR|PLC",
      item == "PW_Biomass_NC" ~ "FOR|PLN",
      item == "SW_Biomass_C"  ~ "FOR|SLC",
      item == "SW_Biomass_NC" ~ "FOR|SLN",
      item == "Bark"          ~ "FOR|BRK",
      item == "BlackLiquor"   ~ "FOR|BLI",
      TRUE                    ~ NA_character_
    )
  ) %>%
  # Drop any items that aren't in our target list
  filter(!is.na(item_mapped)) %>%
  # Join with your existing mappings to get standard Regions and Scenarios
  inner_join(scen_df, by = "For_Scen", relationship = "many-to-many") %>%
  inner_join(full_mapping, by = c("region" = "source_label")) %>%
  # Aggregate the values
  group_by(agmip_target, variable, item_mapped, scens, year) %>%
  summarise(value = sum(value*1000000, na.rm = TRUE), .groups = "drop")

# 3. Format into the Final Tidy Structure
forest_prod_tidy <- df_prod_clean %>%
  mutate(
    Model = "GLOBIOM",
    # NOTE: Adjust the Unit below to match your raw GDX data 
    # (e.g., "Mm3", "1000 m3", or "1000 t")
    Unit  = "m3/yr", 
    Year  = as.integer(as.character(year)),
    scens = as.factor(scens)
  ) %>%
  select(
    Model,
    Scenario = scens,
    Region   = agmip_target,
    Item     = item_mapped,
    Variable = variable,
    Year,
    Unit,
    Value    = value
  ) %>%
  arrange(Scenario, Region, Year, Item)
