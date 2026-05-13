rm(list=ls())

library(gamstransfer)
library(dplyr)
library(tidyr)
library(gdxrrw)
library(openxlsx)

source("./codes/region_mappings.R")

m <- Container$new("./input/Results2025_final.gdx")
# 2. Extract the data as R data frames
df_affor <- m["Harvest_area_compare"]$records

colnames(df_affor) <- c("region","item","ssp","scen1","For_Scen","year","value")
scen_map <- c("RCP4p5"   = "WDM_BS", "RCP4p5_1" = "WDM_HP", "RCP4p5_2" = "WDM_EA")

df_affor.base <- df_affor %>%
  select(-ssp, -scen1) %>%
  filter(For_Scen %in% names(scen_map),
         item %in% c("Affor","Defor"),
         region != "World") %>%
  mutate(For_Scen = recode(For_Scen, !!!scen_map))

net.affor <- df_affor.base %>%
  pivot_wider(names_from = "item", values_from = "value")
net.affor[is.na(net.affor)] <- 0
  
net.affor <- net.affor %>%
  mutate(Affor = Affor  * 1000, Defor = Defor  * 1000)

final.data <- net.affor %>% 
    filter(For_Scen == "WDM_BS") %>%
    select(-For_Scen) %>%
    left_join(mapping_key) %>%
    group_by(target_region, year) %>%
    summarise(Affor = sum(Affor), Defor = sum(Defor), .groups = "drop") %>%
    rename(region = target_region) %>%
    filter(region %in% eu27_regions) %>%
    mutate(year = as.numeric(as.character(year)))
  
source("./codes/getNDC.R")

net.affor_combined <- final.data %>% filter(year<=2050) %>%
    left_join(cumulative_decades, by = c("region" = "Country", "year" = "Year"))
  
  
net.affor_combined <- net.affor_combined %>%
    mutate(RCP = pmax(Affor - Defor, 0),
           NDC = pmax(NDC - Defor, 0)) %>% dplyr::select(-Affor, -Defor)

#enforce growth being positive
net.affor_combined <- net.affor_combined %>%
  group_by(region) %>%
  arrange(year) %>% # Essential to ensure we process chronologically
  mutate(
    NDC = cummax(NDC),
    RCP = cummax(RCP)
  ) %>%
  ungroup()

# 1. Create a baseline dataframe for the year 2000
baseline_2000 <- net.affor_combined %>%
  distinct(region) %>%
  mutate(year = 2000, NDC = 0, RCP = 0)

# 2. Combine, calculate growth, and clean up
growth_rates <- net.affor_combined %>%
  bind_rows(baseline_2000) %>%
  arrange(region, year) %>%
  group_by(region) %>%
  mutate(
    NDC_growth = NDC - lag(NDC),
    RCP_growth = RCP - lag(RCP)
  ) %>%
  # Remove the year 2000 row as it has no 'previous' year to compare to
  filter(year > 2000) %>%
  select(region, year, NDC_growth, RCP_growth)



Base_BSFRS <- net.affor_combined %>%
  group_by(region) %>%
  # 1. Identify the 'anchor' values for each region at the year 2020
  mutate(
    rcp_anchor_2020 = RCP[year == 2020],
    ndc_anchor_2020 = NDC[year == 2020]
  ) %>%
  # 2. Apply logic: RCP until 2020, then RCP_2020 + NDC_growth thereafter
  mutate(
    Base_BSFRS = case_when(
      year <= 2020 ~ RCP,
      year > 2020  ~ rcp_anchor_2020 + (NDC - ndc_anchor_2020)
    )
  ) %>%
  # 3. Clean up helper columns
  select(region, year, Base_BSFRS)

Base_EAFRS <- net.affor_combined %>%
  group_by(region) %>%
  # 1. Capture the RCP value at the 2020 pivot point
  mutate(rcp_2020 = RCP[year == 2020]) %>%
  # 2. Apply the 'Zero Growth' logic
  mutate(
    Base_EAFRS = case_when(
      year <= 2020 ~ RCP,
      year > 2020  ~ rcp_2020
    )
  ) %>%
  # 3. Drop the helper column
  select(region, year, Base_EAFRS)

# 1. Calculate the Scaling Factor for the 2020-2030 growth
rcp_increments_2030 <- net.affor_combined %>%
  group_by(region) %>%
  summarise(
    inc_20_30 = RCP[year == 2030] - RCP[year == 2020]
  )

total_rcp_inc_2030 <- sum(rcp_increments_2030$inc_20_30)
scaling_factor <- 4200 / total_rcp_inc_2030

# 2. Build the FP Scenario
Base_FPFRS <- net.affor_combined %>%
  group_by(region) %>%
  mutate(
    # Anchors for calculation
    rcp_2020 = RCP[year == 2020],
    rcp_2030 = RCP[year == 2030],
    
    # Define FP logic
    Base_FPFRS = case_when(
      # Period 1: Standard RCP
      year <= 2020 ~ RCP,
      
      # Period 2: Scaled RCP growth (totaling 4200)
      year == 2030 ~ rcp_2020 + (rcp_2030 - rcp_2020) * scaling_factor,
      
      # Period 3: Grow from the new 2030 point using original RCP increments
      year > 2030  ~ (rcp_2020 + (rcp_2030 - rcp_2020) * scaling_factor) + (RCP - rcp_2030)
    )
  ) %>%
  # 3. Drop the helper column
  select(region, year, Base_FPFRS)



# scaling_factor <- 1
# 
# # 2. Build the FP Scenario
# notree_FPFRS <- net.affor_combined %>%
#   group_by(region) %>%
#   mutate(
#     # Anchors for calculation
#     rcp_2020 = RCP[year == 2020],
#     rcp_2030 = RCP[year == 2030],
#     
#     # Define FP logic
#     notree_FPFRS = case_when(
#       # Period 1: Standard RCP
#       year <= 2020 ~ RCP,
#       
#       # Period 2: Scaled RCP growth (totaling 4200)
#       year == 2030 ~ rcp_2020 + (rcp_2030 - rcp_2020) * scaling_factor,
#       
#       # Period 3: Grow from the new 2030 point using original RCP increments
#       year > 2030  ~ (rcp_2020 + (rcp_2030 - rcp_2020) * scaling_factor) + (RCP - rcp_2030)
#     )
#   ) %>%
#   # 3. Drop the helper column
#   select(region, year, notree_FPFRS)


Affor_scen <- Base_BSFRS %>% left_join(Base_EAFRS) %>% left_join(Base_FPFRS)


Affor_scen <- Affor_scen %>%
    pivot_longer(
      cols = c(Base_BSFRS, Base_EAFRS, Base_FPFRS), 
      names_to = "FRSScen", 
      values_to = "value")




baseline_2000 <- Affor_scen %>%
  ungroup() %>%
  distinct(region, FRSScen) %>%
  mutate(year = 2000, value = 0)

Affor_forplot <- Affor_scen %>%
  bind_rows(baseline_2000) %>%
  arrange(region, FRSScen, year)

  
  # 2. Create the plot
  ggplot(Affor_forplot, aes(x = year, y = value, color = FRSScen, group = FRSScen)) +
    geom_line(size = 1) +
    geom_point() +
    # Facet by region, allowing y-axes to scale independently if needed
    facet_wrap(~region, scales = "free_y") +
    labs(
      title = "Afforestation Trends by Region",
      subtitle = "Comparison of Scenarios (2010-2050)",
      x = "Year",
      y = "Area (kha)",
      color = "Type"
    ) +
    theme_minimal() +
    theme(
      legend.position = "bottom",
      strip.text = element_text(face = "bold")
    )
  
  
  # 2. Create the plot
  ggplot(Affor_forplot %>% group_by(FRSScen, year) %>% summarise(value=sum(value)), aes(x = year, y = value, color = FRSScen, group = FRSScen)) +
    geom_line(size = 1) +
    geom_point() +
    labs(
      title = "Afforestation Trends EU27",
      subtitle = "Comparison of Scenarios (2010-2050)",
      x = "Year",
      y = "Area (kha)",
      color = "Type"
    ) +
    theme_minimal() +
    theme(
      legend.position = "bottom",
      strip.text = element_text(face = "bold")
    )
  
  
write.csv(Affor_scen %>% pivot_wider(names_from = year, values_from = value), file = "cum_netaff_LAMASUSscen.csv", row.names = F)


# ---- GDX export (unchanged) ----
gams_path <- "C:/GAMS/49" 
igdx(gams_path)
for.gams <- Affor_scen %>% pivot_wider(names_from = year, values_from = value)
jj <- unique(for.gams$FRSScen)[1]
for(jj in unique(for.gams$FRSScen)) {
  df.wide <- for.gams %>% filter(FRSScen==jj) %>% dplyr::select(-FRSScen)
  
  ii <- substr(jj,6,7)
  
  oList <- wgdx.reshape(df.wide, 2,
                        symName = paste0("PekkaAforNet_Reg_",ii),
                        tName = "ScenYear")
  
  
  gdx_file_write <- paste0("./Pekka_affor_", ii, ".gdx")
  wgdx.lst(gdx_file_write, oList)
  
  
} 
  
  


  