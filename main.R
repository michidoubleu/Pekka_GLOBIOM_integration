#orchistration of chain to run extraction of for
library(gamstransfer)
library(tidyverse)
source("./codes/mapping_logic.R")

m <- Container$new("./input/Results2025_final.gdx")


##### calc of affor to CO2 sink conversion factors
source("codes/calc_affor_conffactors.R")
##### calc of HWP emissions
source("codes/agmip_emis.R")

source("codes/agmip_area.R")
source("codes/agmip_prod.R")
source("codes/agmip_cons.R")
source("codes/agmip_nett.R")
source("codes/agmip_yld.R")

full_forest_data <- bind_rows(
  forest_area_tidy,
  forest_prod_tidy,
  forest_cons_tidy,   # Output from agmip_cons.R
  forest_nett_tidy,   # Output from agmip_nett.R
  forest_yield_tidy
) %>%
  # Sort the final combined dataset so it is organized logically
  arrange(Scenario, Region, Year, Variable, Item)

# 3. Verify the final combined structure
str(full_forest_data)

saveRDS(full_forest_data, "./output/copy2openinput/processed_forest.rds")




