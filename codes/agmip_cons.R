library(gamstransfer)
library(tidyverse)

# 1. Load Data
df_cons <- m["Consumption_compare"]$records
colnames(df_cons) <- c("region","item","ssp","scen1","For_Scen","year","value")


# 2. Map, Filter, and Aggregate
df_cons_clean <- df_cons %>%
  mutate(
    variable = "FCONS",
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
  summarise(value = sum(value, na.rm = TRUE)*1000000, .groups = "drop")


# Add total forest consumption ("FOR")
df_cons_total <- df_cons_clean %>%
  group_by(agmip_target, variable, scens, year) %>%
  summarise(
    value = sum(value, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(item_mapped = "FOR")

# Combine detailed items and total
df_cons_clean <- bind_rows(df_cons_clean, df_cons_total)




# 3. Format into the Final Tidy Structure
forest_cons_tidy <- df_cons_clean %>%
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
