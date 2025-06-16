#next: add file name to id file then load and zipper
library(tidyverse)
library(readxl)

main.folder = "~/Documents/GitHub/fastcharge-het"
setwd(main.folder)
#import functions ####
#source("loadfunctions.R")
#source("PackageCheck.R")

#debugging ####
#print_checks = TRUE

#import identity dataframe #####
id <- read_excel("fastcharge-het-attributes.xlsx") #created from notion database, has chem, soc bat for each test id
id = mutate(id, filename = sub("\\.csv$", "", filename))

###import csv data#####
setwd("Charge Profile Data")
# Get the list of files
files <- list.files(pattern = '*.csv')

# Initialize an empty list to store data frames
df_list <- list()

for (file in files) {
  df <- read_csv(file, col_names = c("soc", "power")) %>%
    mutate(filename = sub("\\.csv$", "", file))

  df_list[[file]] <- df  # Store in the list
}

# Combine all data frames into one
profiles <- bind_rows(df_list)

#Check by plotitng power vs soc import data  
ggplot(data, aes(x = soc, y = power, color = filename)) +
  geom_point() + 
  theme(legend.position = "none") 

#create main dataframe, combining attributes and csvs 
main = left_join(
  profiles, 
  id,
  by = "filename")

#Check merge by adding oem facetwrap 
ggplot(main, aes(x = soc, y = power, color = filename)) +
  geom_point() + 
  theme(legend.position = "none") +
  facet_wrap(~oem)
#introduce additional units
data <- main %>%
  group_by(filename) %>%
  mutate(
    model_name = paste(model,pack.kWh, sep = "_"),
    psoc_packR.kWh = pack.kWh*soc/100,
    ) %>%
  ungroup()

#introduce additional units2
data <- data %>%
  group_by(filename) %>%
  mutate(
    dsoc = c(NA, diff(soc)),
    dsoc = ifelse(dsoc < 0, NA, dsoc),
    dtime.hr = ifelse(abs(power) > 1e-6 & !is.na(dsoc), dsoc / power, 0),
    dtime.hr = replace_na(dtime.hr, 0),
    denergy_calc.kWh = power * dtime.hr,
    cum_time.min = round(cumsum((dtime.hr*60)),2),
    cum_time.hr = round(cumsum((dtime.hr)),2),
    energy_calc.kWh = cumsum(denergy_calc.kWh),
    pack_size_calc.kWh = last(energy_calc.kWh) / ((last(soc) - first(soc)) / 100),
    pack_partial.kWh = pack_size_calc.kWh * soc / 100,
    check_diff = energy_calc.kWh - pack_partial.kWh + first(soc)*pack_partial.kWh,  # should be ~0 if linear assumption holds
    mutate(across(c(cum_time.min,energy_calc.kWh,pack_size_calc.kWh,pack_partial.kWh, check_diff), ~ round(.x, 3)))
  ) %>%
  ungroup()

#toy model to examine 
dt = subset(data, filename == "Toyota _ ProAce City Electric _ 50 kWh (2024+)_150kW+_SOC vs Charge Speed")

#library(patchwork)
p = ggplot(dt, 
           aes(x = soc, 
               y = power, 
               color = filename)) +
  geom_point() + 
  theme(legend.position = "none")


#Check by plotting power vs tpack.kWh#Check by plotting power vs time import data 
p + ggplot(dt, 
       aes(x = psoc_packR.kWh, 
           y = power, 
           color = filename)) +
  geom_point() + 
  theme(legend.position = "none") #+
#facet_wrap(~oem)

#check pack size calculations
summarise(max_partial_calc = max(pack_partial.kWh),
          max_size_calc = max(pack_size_calc.kWh),
          pack_reported = first(pack.kWh),
          psoc_packR.kWh = max(psoc_packR.kWh),
          derate =  first(pack.kWh) - max(pack_partial.kWh),
          min_soc = min(soc),
          max_soc = max(soc)
) %>% 
  ungroup()

# Max charging power by model, year
  summary = data %>%
    group_by(filename) %>%
    summarise(max_power = max(power),
             oem = oem,
             model = model,
             model_name = model_name) %>%
    ungroup()
  
  ggplot(max_power, aes(x = qs, y = filename, color = filename)) +
    geom_bar()
  
  
# Transform to time, time 20-80%? 
  data <- data %>%
    group_by(filename) %>%
    mutate(
      dsoc = c(NA, diff(soc)),
      dsoc = ifelse(dsoc < 0, NA, dsoc),
      dtime.hr = ifelse(abs(power) > 1e-6 & !is.na(dsoc), dsoc / power, 0),
      dtime.hr = replace_na(dtime.hr, 0),
      denergy_calc.kWh = power * dtime.hr,
      cum_time.min = round(cumsum((dtime.hr*60)),2),
      cum_time.hr = round(cumsum((dtime.hr)),2)) %>% ungroup()
  
  d2080 = filter(data, (soc < 80 & soc > 20))
  
  ggplot(d2080, aes(x = cum_time.hr, y = soc, color = filename)) +
    geom_line() + theme(legend.position = "none") 
  ## need to count time from 20 to 80 so need function to subtract time at 20% 
  
  
# For 20-80% average power
  avgp_2080 <- data %>%
    group_by(filename) %>%
    filter(data, (soc < 80 & soc > 20)) %>%
    summarise( avg_power = mean(power)) %>% 
    ungroup()
  
# For 40-60% average power 
  avgp_4060 <- data %>%
    group_by(filename) %>%
    filter(data, (soc < 60 & soc > 40)) %>%
    summarise( avg_power = mean(power)) %>% 
    ungroup()
  
# Delta power per unit SOC at what SOC is this highest 
  data <- data %>%
    group_by(filename) %>%
    mutate(
      #running derivative, need to skip and do different values 
      #want per unit soc not per data specificity 
      unit_soc = dsec(power,soc)
    ) %>% 
    ungroup()
  
# Delta min power max power per charge
  data <- data %>%
    group_by(filename) %>%
    summary(
      #running derivative, need to skip and do different values 
      #want per unit soc not per data specificity 
      min_power = min(power),
      max_power = max(power),
      oem = oem,
      start.year = start.year,
      end.year = end.year
    ) %>% 
    ungroup()

ggplot(main, aes(x = soc, y = power, color = oem)) +
  geom_point() + 
  #theme(legend.position = "none") +
  facet_grid(end.year~end.year)

ggplot(main, aes(x = soc, y = power, color = filename)) +
  geom_point() + 
  theme(legend.position = "none") +
  facet_wrap(~main$pack.kWh)

