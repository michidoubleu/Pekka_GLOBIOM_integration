library(gamstransfer)
library(tidyverse)
source("./codes/mapping_logic.R")

cat("=== Testing Regional Aggregation for Double Counting ===\n\n")

# 1. Load Data
m <- Container$new("./input/Results2025_final.gdx")

df_area <- m["Harvest_area_compare"]$records
colnames(df_area) <- c("region","item","ssp","scen1","For_Scen","year","value")

# Filter to a specific scenario year as an example
df_area_test <- df_area %>% 
  filter(item == "total_forest_area", For_Scen == "RCP4p5", ssp == "SSP2", scen1 == "BASE", year == "2030")

df_emis <- m["CARBON_SINK"]$records
colnames(df_emis) <- c("region","item", "unit" ,"ssp","scen1","For_Scen","year","value")
df_emis_test <- df_emis %>% 
  filter(item == "Forest", For_Scen == "RCP4p5", year == "2030")

# 2. Aggregation Check Function
# This function calculates the sum of the MAPPED base regions and compares it 
# to a known valid set of mutually exclusive regions from the original data
check_aggregation <- function(df, data_name, filter_func = NULL) {
  cat(sprintf("\n--- %s ---\n", data_name))
  
  # 1. Apply the safe filtering to get the true total without double counting
  if (!is.null(filter_func)) {
    df_filtered <- filter_func(df)
  } else {
    # If no filter func provided, we try to use the old hardcoded logic for benchmarking
    primary_agmip_regions <- c(
      "ANZ", "ArgentinaReg", "BrazilReg", "CanadaReg", "ChinaReg", "CongoBasin", 
      "Former_USSR", "IndiaReg", "IndonesiaReg", "JapanReg", "MalaysiaReg", 
      "MexicoReg", "MidEastNorthAfr", "Pacific_Islands", "RCAM", "RCEU", "ROWE", 
      "RSAM", "RSAS", "RSEA_OPA", "RSEA_PAC", "SouthAfrReg", "SouthKorea", 
      "EasternAf", "SouthernAf", "WesternAf", "TurkeyReg", "UkraineReg", "USAReg",
      "AUTReg", "BELReg", "BGRReg", "CYPReg", "CZEReg", "DEUReg", "DNKReg", 
      "ESPReg", "ESTReg", "FINReg", "FRAReg", "GBRReg", "GRCReg", "HRVReg", 
      "HUNReg", "IRLReg", "ITAReg", "LTUReg", "LUXReg", "LVAReg", "MLTReg", 
      "NLDReg", "POLReg", "PRTReg", "ROUReg", "SVKReg", "SVNReg", "SWEReg", "NorwayReg"
    )
    
    preferred_regions <- unique(df$region)[grepl("Reg\\$", unique(df$region)) | 
                                             unique(df$region) %in% c("RCEU", "ROWE", "CongoBasin", "RCAM", "RSAM", "RSAS", "RSEA_OPA", "RSEA_PAC")]
    
    df_filtered <- df %>% filter(region %in% primary_agmip_regions)
    df_filtered <- df_filtered %>% filter(
      region %in% preferred_regions | 
        (!region %in% c("Austria", "Belgium", "Germany", "France", "Italy", "Spain", "Netherlands")) 
    )
  }
  
  true_total <- sum(df_filtered$value, na.rm=TRUE)
  cat(sprintf("True Global Total (Filtered Original Data): %12.2f\n", true_total))
  
  # 2. Map the UNFILTERED data using mapping_logic (how agmip_area.R currently does it)
  mapped_data <- df %>%
    inner_join(full_mapping, by = c("region" = "source_label"))
  
  # 3. Define mutually exclusive base AgMIP targets to check the sum
  # These are the lowest level mappings from mapping_logic.R
  base_targets <- c("AUT", "BEL", "BGR", "HRV", "CYP", "CZE", "DNK", "EST", "FIN", 
                    "FRA", "DEU", "GRC", "HUN", "IRL", "ITA", "LVA", "LTU", "LUX", 
                    "MLT", "NLD", "POL", "PRT", "ROU", "SVK", "SVN", "ESP", "SWE", 
                    "CAN", "USA", "BRA", "OSA", "CHN", "IND", "SEA", "FSU", "ANZ", 
                    "MEN", "SSA")
  
  mapped_base <- mapped_data %>% filter(agmip_target %in% base_targets)
  sum_mapped <- sum(mapped_base$value, na.rm=TRUE)
  
  cat(sprintf("Mapped Global Total (Tested Data):          %12.2f\n", sum_mapped))
  
  if (abs(true_total - sum_mapped) > 1e-6) {
    cat(">>> WARNING: Mapped total differs from True total! Double-counting detected.\n")
    diff_val <- sum_mapped - true_total
    cat(sprintf(">>> Difference (Mapped - True): %12.2f\n", diff_val))
    
    # Identify which target regions received duplicates
    dups <- mapped_base %>%
      group_by(agmip_target) %>%
      summarise(n_sources = n_distinct(region),
                sources = paste(unique(region), collapse=", "),
                total_val = sum(value)) %>%
      filter(n_sources > 1)
    
    if(nrow(dups) > 0) {
      cat("\nTarget regions with multiple overlapping sources mapped to them:\n")
      print(as.data.frame(dups))
    }
  } else {
    cat(">>> SUCCESS: Mapped total matches True total!\n")
  }
}

cat("Testing UN-FILTERED data (How agmip_area.R is currently set up):\n")
check_aggregation(df_area_test, "AREA (Unfiltered Before Mapping)")

# We run emis unfiltered just to see if the GDX data for emissions inherently has duplicates
check_aggregation(df_emis_test, "EMISSIONS (Unfiltered Before Mapping)")

# EMISSIONS
check_aggregation(df_emis_test, "EMISSIONS (Filtered Before Mapping)", filter_emis_safely)

# AREA
check_aggregation(df_area_test, "AREA (Filtered Before Mapping)", filter_area_safely)
