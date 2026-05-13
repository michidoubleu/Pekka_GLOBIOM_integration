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
  "RCP4p5_1",  "Base_FPWDM", 
  "RCP4p5_2",  "Base_EAWDM",
  "RCP4p5",    "Base_FPFRS", 
  "RCP4p5",    "Base_EAFRS"
)


library(dplyr)

# 1. Base Cleaning and Mapping (Updated for FOR|PRIM)
df_area_clean <- df_area %>% 
  filter(item != "total_forest_area") %>%
  mutate(
    value = value * 1000,
    variable = "AREA",
    item = case_when(
      item == "PriFor"  ~ "FOR|PRIM",                    # Primary Forest mapped here
      item %in% c("CurS", "Cur0") ~ "FOR|PRO",           # Remaining Unmanaged/Strict Protection
      item == "CurC_L"  ~ "FOR|LOC",                     
      item == "CurNC_L" ~ "FOR|LON",                     
      item == "CurC_M"  ~ "FOR|MIC",                     
      item == "CurNC_M" ~ "FOR|MIN",                     
      item == "CurC"    ~ "FOR|HIC",                     
      item == "CurNC"   ~ "FOR|HIN",                     
      TRUE              ~ as.character(item)             
    )
  ) %>%
  # Make sure FOR|PRIM is included in the filter!
  filter(item %in% c("FOR|PRIM", "FOR|PRO", "FOR|LOC", "FOR|LON", 
                     "FOR|MIC", "FOR|MIN", "FOR|HIC", "FOR|HIN")) %>%
  select(region, For_Scen, variable, item, year, value) %>%
  inner_join(scen_df, by = "For_Scen", relationship = "many-to-many") %>%
  inner_join(full_mapping, by = c("region" = "source_label")) %>%
  group_by(agmip_target, variable, item, scens, year) %>%
  summarise(value = sum(value, na.rm = TRUE), .groups = "drop")

# 2. Format into the Tidy Structure
forest_area_tidy_base <- df_area_clean %>%
  mutate(
    Model = "GLOBIOM",
    Unit  = "1000 ha",
    year  = as.integer(as.character(year)),
    scens = as.factor(scens)
  ) %>%
  select(
    Model,
    Scenario = scens,
    Region   = agmip_target,
    Item     = item,
    Variable = variable,
    Year     = year,
    Unit,
    Value    = value
  )

# 3. Create the Managed Forest (FOR|MAN) Aggregate
managed_aggregate <- forest_area_tidy_base %>%
  # Filter only for the 6 managed intensity categories
  filter(Item %in% c("FOR|LOC", "FOR|LON", "FOR|MIC", "FOR|MIN", "FOR|HIC", "FOR|HIN")) %>%
  # Group by everything EXCEPT Item and Value
  group_by(Model, Scenario, Region, Variable, Year, Unit) %>%
  # Sum the values to get the total managed forest area
  summarise(Value = sum(Value, na.rm = TRUE), .groups = "drop") %>%
  # Assign the new item code
  mutate(Item = "FOR|MAN")

# 4. Bind the aggregate back to the main dataframe
forest_area_tidy <- bind_rows(forest_area_tidy_base, managed_aggregate) %>%
  # Optional: arrange by Scenario, Region, and Year to keep it neat
  arrange(Scenario, Region, Year, Item)

# Verify the output
str(forest_area_tidy)

