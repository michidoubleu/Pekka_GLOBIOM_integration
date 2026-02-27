library(gamstransfer)
library(dplyr)
m <- Container$new('./input/Results2025_final.gdx')
df_area <- m['Harvest_area_compare']$records
colnames(df_area) <- c('region', 'item', 'ssp', 'scen1', 'For_Scen', 'year', 'value')
df_area_test <- df_area %>% filter(item == 'total_forest_area', For_Scen == 'RCP4p5', ssp == 'SSP2', scen1 == 'BASE', year == '2030')
cat('Sum of unfiltered area: ', sum(df_area_test$value, na.rm=TRUE), '\n')
cat('Number of rows: ', nrow(df_area_test), '\n')
