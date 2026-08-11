library(gamstransfer)
library(tidyverse)

# 1. Load Data
df_prod <- m["Production_compare"]$records
colnames(df_prod) <- c("region","item","ssp","scen1","For_Scen","year","value")


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
  filter(!is.na(item_mapped)) %>%
  inner_join(scen_df, by = "For_Scen", relationship = "many-to-many") %>%
  inner_join(full_mapping, by = c("region" = "source_label")) %>%
  group_by(agmip_target, variable, item_mapped, scens, year) %>%
  summarise(value = sum(value * 1000000, na.rm = TRUE), .groups = "drop")

# Add total forest production ("FOR")
df_prod_total <- df_prod_clean %>%
  group_by(agmip_target, variable, scens, year) %>%
  summarise(
    value = sum(value, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(item_mapped = "FOR")

# Combine detailed items and total
df_prod_clean <- bind_rows(df_prod_clean, df_prod_total)

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
