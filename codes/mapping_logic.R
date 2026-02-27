library(tidyverse)

# 1. Define the base mapping
mapping_data <- tribble(
  ~source_label,    ~agmip_target,
  # --- European Countries (Mapping both full names and Reg codes) ---
  "Austria", "AUT", "AUTReg", "AUT",
  "Belgium", "BEL", "BELReg", "BEL",
  "Bulgaria", "BGR", "BGRReg", "BGR",
  "Croatia", "HRV", "HRVReg", "HRV",
  "Cyprus", "CYP", "CYPReg", "CYP",
  "CzechRep", "CZE", "CZEReg", "CZE",
  "Denmark", "DNK", "DNKReg", "DNK",
  "Estonia", "EST", "ESTReg", "EST",
  "Finland", "FIN", "FINReg", "FIN",
  "France", "FRA", "FRAReg", "FRA",
  "Germany", "DEU", "DEUReg", "DEU",
  "Greece", "GRC", "GRCReg", "GRC",
  "Hungary", "HUN", "HUNReg", "HUN",
  "Ireland", "IRL", "IRLReg", "IRL",
  "Italy", "ITA", "ITAReg", "ITA",
  "Latvia", "LVA", "LVAReg", "LVA",
  "Lithuania", "LTU", "LTUReg", "LTU",
  "Luxembourg", "LUX", "LUXReg", "LUX",
  "Malta", "MLT", "MLTReg", "MLT",
  "Netherlands", "NLD", "NLDReg", "NLD",
  "Poland", "POL", "POLReg", "POL",
  "Portugal", "PRT", "PRTReg", "PRT",
  "Romania", "ROU", "ROUReg", "ROU",
  "Slovakia", "SVK", "SVKReg", "SVK",
  "Slovenia", "SVN", "SVNReg", "SVN",
  "Spain", "ESP", "ESPReg", "ESP",
  "Sweden", "SWE", "SWEReg", "SWE",
  
  # --- Regional/Macro Groups (Full Country Names) ---
  "Canada", "CAN", "CanadaReg", "CAN",
  "USA", "USA", "USAReg", "USA",
  "Brazil", "BRA", "BrazilReg", "BRA",
  "Mexico", "OSA", "MexicoReg", "OSA",
  "Argentina", "OSA", "ArgentinaReg", "OSA",
  "China", "CHN", "ChinaReg", "CHN",
  "India", "IND", "IndiaReg", "IND",
  "Indonesia", "SEA", "IndonesiaReg", "SEA",
  "Malaysia", "SEA", "MalaysiaReg", "SEA",
  "Japan", "SEA", "JapanReg", "SEA",
  "RussianFed", "FSU", "Ukraine", "FSU", "UkraineReg", "FSU",
  "Australia", "ANZ", "AustraliaReg", "ANZ",
  "NewZealand", "ANZ", "NewZealandReg", "ANZ",
  "Turkey", "MEN", "TurkeyReg", "MEN",
  "SouthAfrica", "SSA", "SouthAfrReg", "SSA",
  
  # --- Aggregates to Macro Regions ---
  "Canada", "NAM", "USA", "NAM", "CanadaReg", "NAM", "USAReg", "NAM",
  "Brazil", "OAM", "Mexico", "OAM", "Argentina", "OAM",
  "BrazilReg", "OAM", "MexicoReg", "OAM", "ArgentinaReg", "OAM",
  "China", "SAS", "India", "SAS", "Indonesia", "SAS", "Malaysia", "SAS",
  "ChinaReg", "SAS", "IndiaReg", "SAS", "IndonesiaReg", "SAS", "MalaysiaReg", "SAS"
)

# 2. Logic to generate EUE and EUR automatically from the country list
# This saves us from writing "Austria" -> "EUE" manually for every row.
eu_countries <- c("Austria", "Belgium", "Bulgaria", "Croatia", "Cyprus", "CzechRep", 
                  "Denmark", "Estonia", "Finland", "France", "Germany", "Greece", 
                  "Hungary", "Ireland", "Italy", "Latvia", "Lithuania", "Luxembourg", 
                  "Malta", "Netherlands", "Poland", "Portugal", "Romania", "Slovakia", 
                  "Slovenia", "Spain", "Sweden", "AUTReg", "BELReg", "BGRReg", "HRVReg", 
                  "CYPReg", "CZEReg", "DNKReg", "ESTReg", "FINReg", "FRAReg", "DEUReg", 
                  "GRCReg", "HUNReg", "IRLReg", "ITAReg", "LVAReg", "LTUReg", "LUXReg", 
                  "MLTReg", "NLDReg", "POLReg", "PRTReg", "ROUReg", "SVKReg", "SVNReg", 
                  "ESPReg", "SWEReg")

eue_mapping <- tibble(source_label = eu_countries, agmip_target = "EUE")
eur_mapping <- tibble(source_label = eu_countries, agmip_target = "EUR")

# Add special cases for EUR that aren't in the 27 EUE list
extra_eur <- tribble(
  ~source_label, ~agmip_target,
  "RCEU", "EUR",
  "ROWE", "EUR",
  "EU27", "EUR"
)

# 3. Combine everything
full_mapping <- bind_rows(mapping_data, eue_mapping, eur_mapping, extra_eur) %>%
  distinct()