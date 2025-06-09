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

#create main dataframe, combining attributes and csvs 
main = left_join(
  profiles, 
  id,
  by = "filename")

#compare id and profiles for matches to merge on 
ggplot(main, aes(x = soc, y = power, color = filename)) +
  geom_point() + 
  theme(legend.position = "none") +
  facet_wrap(~oem)

ggplot(main, aes(x = soc, y = power, color = oem)) +
  geom_point() + 
  #theme(legend.position = "none") +
  facet_grid(end.year~end.year)


