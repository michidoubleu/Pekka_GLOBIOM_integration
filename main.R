#orchistration of chain to run extraction of for
library(gamstransfer)
library(tidyverse)
source("./codes/mapping_logic.R")

m <- Container$new("./input/Pekka_LAMASUS_Forest.gdx")

scen_df <- tibble::tribble(
  ~For_Scen,   ~scens,
  "RCP4p5"  ,  "BASE", 
  "RCP4p5_1",  "FP", 
  "RCP4p5_8",  "EA",
  "RCP4p5_10",  "CP",
  "RCP4p5_1", "Base_FPWDM", 
  "RCP4p5_2", "Base_EAWDM",
  "RCP4p5_1", "Base_CPWDM", 
  "RCP4p5",    "Base_FPBSR", 
  "RCP4p5_6",  "Base_EABSR",
  "RCP4p5_9",  "Base_CPBSR", 
  "RCP4p5",  "Base_FPSOC", 
  "RCP4p5",  "Base_EASOC",
  "RCP4p5",  "Base_CPSOC",
  "RCP4p5",  "Base_FPSWF", 
  "RCP4p5",  "Base_EASWF",
  "RCP4p5",  "Base_CPSWF",
  "RCP4p5",  "Base_FPFRT", 
  "RCP4p5",  "Base_EAFRT",
  "RCP4p5",  "Base_CPFRT",
  "RCP4p5",  "Base_FPPST", 
  "RCP4p5",  "Base_EAPST",
  "RCP4p5",  "Base_CPPST",
  "RCP4p5",  "Base_FPORG", 
  "RCP4p5",  "Base_EAORG",
  "RCP4p5",  "Base_CPORG",
  "RCP4p5",  "Base_FPPTL",
  "RCP4p5",  "Base_EAPTL",
  "RCP4p5",  "Base_CPPTL", 
  "RCP4p5",  "Base_FPCAP", 
  "RCP4p5",  "Base_EACAP",
  "RCP4p5",  "Base_FPFRS", 
  "RCP4p5",  "Base_EAFRS", 
  "RCP4p5",  "Base_CPFRS"
)


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




