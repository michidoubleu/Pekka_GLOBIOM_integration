rm(list=ls())

library(gamstransfer)
library(dplyr)
library(tidyr)

eu27 <- c(
  "Austria", "Belgium", "Bulgaria", "Croatia", "CzechRep", "Denmark",
  "Estonia", "Finland", "France", "Germany", "Greece", "Hungary",
  "Ireland", "Italy", "Latvia", "Lithuania", "Luxembourg",
  "Netherlands", "Poland", "Portugal", "Romania", "Slovakia",
  "Slovenia", "Spain", "Sweden"
)

# 1. Create a Container linked to your GDX file
# Make sure the .gdx file is in your working directory, or provide the full path.
m <- Container$new("./input/Results2025_final.gdx")

# 2. Extract the data as R data frames
df_affor <- m["Harvest_area_compare"]$records

colnames(df_affor) <- c("region","item","ssp","scen1","For_Scen","year","value")

scen_map <- c(
  "RCP4p5"   = "WDM_BS", 
  "RCP4p5_1" = "WDM_HP", 
  "RCP4p5_2" = "WDM_EA"
)

# 2. Process harvest_volume
df_affor <- df_affor %>%
  dplyr::select(-ssp, -scen1) %>%
  filter(For_Scen %in% names(scen_map), item%in%c("Affor","Defor"), region!="World") %>%       # Keep only the 3 scenarios
  mutate(For_Scen = recode(For_Scen, !!!scen_map)) # Rename them

net.affor <- df_affor %>% pivot_wider(names_from = "item", values_from = "value")
net.affor[is.na(net.affor)] <- 0
net.affor <- net.affor %>% mutate(net.aff=pmax(Affor-Defor,0)*1000)

net.affor <- net.affor%>% filter(region%in%eu27) %>%
  group_by(region, For_Scen) %>%
  mutate(
    # Subtract the value of net.aff where year is 2010 from all rows in the group
    net.aff = net.aff - net.aff[year == "2010"]
  ) %>%
  ungroup()

net.affor <- net.affor %>%
  mutate(year_num = as.numeric(as.character(year))) %>%
  group_by(region, For_Scen) %>%
  arrange(year_num, .by_group = TRUE) %>%
  mutate(
    add.aff = net.aff - lag(net.aff, default = 0)
  ) %>%
  ungroup() %>% mutate(add.aff=ifelse(add.aff<0,0,add.aff))


final.data <- net.affor %>% 
  dplyr::select(-Defor,-Affor, -year_num, -net.aff) %>% 
  pivot_wider(names_from = "year", values_from = "add.aff") %>% 
  mutate(region=paste0(region,"Reg"))

write.csv(final.data, "output/affor_scen.csv", row.names = F)
