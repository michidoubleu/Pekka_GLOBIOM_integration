library(gamstransfer)
library(dplyr)
m <- Container$new('./input/Results2025_final.gdx')

df_area <- m['Harvest_area_compare']$records
cat('Harvest_area_compare regions:\n')
print(unique(df_area[,1]))

df_emis <- m['CARBON_SINK']$records
cat('\nCARBON_SINK regions:\n')
print(unique(df_emis[,1]))
