library(dplyr)
library(tidyr)

# 1. Hardcode the exogenous yield values
yield_data_raw <- tibble::tribble(
  ~Country, ~Value,
  "Austria", 7.5, "Belgium", 7.7, "Bulgaria", 6,
  "Croatia", 4.7, "Cyprus", 1.2, "CzechRep", 9.4,
  "Denmark", 11.3, "Estonia", 5.7, "Finland", 4.8,
  "France", 5.5, "Germany", 11.2, "Greece", 1.3,
  "Hungary", 6.9, "Ireland", 11.5, "Italy", 4.1,
  "Latvia", 6.6, "Lithuania", 6.4, "Luxembourg", 7.6,
  "Netherlands", 9.2, "Poland", 8, "Portugal", 8.6,
  "Romania", 8.7, "Slovakia", 7.6, "Slovenia", 9,
  "Spain", 2.4, "Sweden", 4.4, "UK", 7.6,
  "EU27", 5.8
)

# 2. Define the mapping to your 3-letter target codes
# Note: "UK" wasn't in your target list array, so I mapped it to standard "GBR". 
# "EU27" is mapped to "EUE". Adjust these two if your model expects something else (like "EUR").
region_map <- c(
  "Austria" = "AUT", "Belgium" = "BEL", "Bulgaria" = "BGR",
  "Croatia" = "HRV", "Cyprus" = "CYP", "CzechRep" = "CZE",
  "Denmark" = "DNK", "Estonia" = "EST", "Finland" = "FIN",
  "France" = "FRA", "Germany" = "DEU", "Greece" = "GRC",
  "Hungary" = "HUN", "Ireland" = "IRL", "Italy" = "ITA",
  "Latvia" = "LVA", "Lithuania" = "LTU", "Luxembourg" = "LUX",
  "Netherlands" = "NLD", "Poland" = "POL", "Portugal" = "PRT",
  "Romania" = "ROU", "Slovakia" = "SVK", "Slovenia" = "SVN",
  "Spain" = "ESP", "Sweden" = "SWE", "UK" = "GBR", "EU27" = "EUE"
)

# 3. Extract the unique scenarios and years from your existing tidy dataframe
# (This ensures perfect alignment with your forest area/production tables)
unique_scens <- unique(forest_area_tidy$Scenario)
unique_years <- unique(forest_area_tidy$Year)

# 4. Map, Expand, and Format into the Tidy Structure
forest_yield_tidy <- yield_data_raw %>%
  mutate(Region = region_map[Country]) %>%
  # Remove the original Country column
  select(-Country) %>%
  # crossing() creates a full combination of the regions with ALL scenarios and years
  crossing(Scenario = unique_scens, Year = unique_years) %>%
  # Add the static target columns
  mutate(
    Model    = "GLOBIOM",
    Item     = "FOR",
    Variable = "FYLD",
    Unit     = "m3/ha",
    # Ensure Scenario remains a factor if required
    Scenario = as.factor(Scenario)
  ) %>%
  # Select and order exactly as requested
  select(
    Model,
    Scenario,
    Region,
    Item,
    Variable,
    Year,
    Unit,
    Value
  ) %>%
  arrange(Scenario, Region, Year)


