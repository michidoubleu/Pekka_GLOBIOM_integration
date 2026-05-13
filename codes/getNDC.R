
# Load necessary libraries
# install.packages("tidyverse")
# install.packages("zoo")
library(tidyverse)
library(zoo)

# Install the package if you haven't already
# install.packages("readxl")
library(readxl)

# Define the path to your Excel file
# If the file is in your working directory, just use the filename
excel_path <- "./input/Affor_Defor_REF_SCEN.xlsx" 

# Read the specific sheets by name
affor_raw <- read_excel(excel_path, sheet = "Afforestation_ha")
defor_raw <- read_excel(excel_path, sheet = "Deforestation_ha")

# 2. Reshape and Calculate Annual Net Afforestation
net_affor_long <- affor_raw %>%
  pivot_longer(cols = matches("^[0-9]{4}$"), 
               names_to = "Year", 
               values_to = "Afforestation") %>%
  mutate(Year = as.numeric(Year)) %>%
  inner_join(
    defor_raw %>%
      pivot_longer(cols = matches("^[0-9]{4}$"), 
                   names_to = "Year", 
                   values_to = "Deforestation") %>%
      mutate(Year = as.numeric(Year)),
    by = c("Country", "Scenario", "Year")
  ) %>%
  mutate(Net_Afforestation = Afforestation)

# 3. Calculate Cumulative Sum and Filter for Decades
cumulative_decades <- net_affor_long %>%
  group_by(Country, Scenario) %>%
  arrange(Year) %>%
  mutate(NDC = round(cumsum(Net_Afforestation)/1000,1)) %>%
  ungroup() %>%
  # Filter for the milestone years
  filter(Year %in% seq(2000, 2070, by = 10)) %>%
  select(Country, Scenario, Year, NDC) 

# 2. Rename and Filter the dataframe
cumulative_decades <- cumulative_decades %>%
  mutate(Country = case_when(
    Country == "Czech Republic" ~ "CzechRepReg",  # Handle the special case
    TRUE ~ paste0(Country, "Reg")                 # Append "Reg" to all others
  )) %>%
  # Filter out countries (like Malta/Cyprus/UK) that are not in your region list
  filter(Country %in% eu27_regions)

cumulative_decades <- cumulative_decades %>% dplyr::select(-Scenario)
