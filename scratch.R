#scratch 



#ggplot(profiles, aes(x = soc, y = power, color = filename)) +
#  geom_point() + 
#  theme(legend.position = "none") +
#  facet_wrap(~pack_energy.trans_kWh)

ggplot(profiles, aes(x = soc, y = power, color = manufacturer)) +
  geom_point() + 
  #theme(legend.position = "none") +
  facet_wrap(~year)


#check imports for mismatch  
if(print_checks == TRUE){ 
  identified = id$channel
  as_files = names(df.capacity)
  missing_files <- setdiff(identified, as_files) #Files that are missing
  missing_ids <- setdiff(as_files, identified)
  paste("files missing", missing_files)
  paste("ids missing", missing_ids)
  
} #Checking that the id sheet has identifies all files


tesla = profiles %>% filter(manufacturer == "Tesla")
view(tesla)

fpro = profiles %>% filter(filename == "Tesla Model S CCS adapter_150kW_SOC vs charge speed" )
#fpro = select(fpro, power, soc)
fpro <- main %>%
  mutate(
    dsoc = c(NA, diff(soc)),
    dsoc = ifelse(dsoc < 0, NA, dsoc),
    dtime_hr = ifelse(abs(power) > 1e-6 & !is.na(dsoc), dsoc / power, 0),
    dtime_hr = replace_na(dtime_hr, 0),
    denergy.trans_kWh = power * dtime_hr,
    total.time_min = round(cumsum((dtime_hr*60)),2),
    energy.trans_kWh = cumsum(denergy.trans_kWh),
    pack_size = last(energy.trans_kWh) / ((last(soc) - first(soc)) / 100),
    pack_kWh = pack_size * soc / 100,
    check_diff = energy.trans_kWh - pack_kWh + first(soc)*pack_kWh,  # should be ~0 if linear assumption holds
    mutate(across(c(total.time_min,energy.trans_kWh,pack_size,pack_kWh, check_diff), ~ round(.x, 3)))
  ) 

ggplot(fpro, aes(x = soc, y = power)) +
  geom_point() + 
  labs(title = first(fpro$filename)) +
  theme(legend.position = "none"

# After the mutate, check for the condition and select columns if needed
if (checks == FALSE) {
  fpro <- select(fpro, soc, power, total.time_min, pack_kWh)
  
  p = ggplot(fpro, aes(x = soc, y = power)) +
    geom_point() + 
    labs(title = first(fpro$filename)) +
    theme(legend.position = "none")
  t = ggplot(fpro, aes(y = power, x = total.time_min)) +
    geom_point() + 
    theme(legend.position = "none")
  e = ggplot(fpro, aes(y = power, x = pack_kWh)) +
    geom_point() + 
    theme(legend.position = "none")
  
  library(patchwork)
  p / t + e
  
}

view(fpro)
#check total duration

gen_sim <- function(file) {
  file <- file %>%
    mutate(
      dsoc = c(NA, diff(soc)),
      dsoc = ifelse(dsoc < 0, NA, dsoc),
      dtime_hr = ifelse(abs(power) > 1e-6 & !is.na(dsoc), dsoc / power, 0),
      dtime_hr = replace_na(dtime_hr, 0),
      denergy.trans_kWh = power * dtime_hr,
      total.time_min = round(cumsum((dtime_hr*60)),2),
      energy.trans_kWh = cumsum(denergy.trans_kWh),
      pack_size = last(energy.trans_kWh) / ((last(soc) - first(soc)) / 100),
      pack_kWh = pack_size * soc / 100,
      check_diff = energy.trans_kWh - pack_kWh + first(soc)*pack_kWh,  # should be ~0 if linear assumption holds
      mutate(across(c(total.time_min,energy.trans_kWh,pack_size,pack_kWh, check_diff), ~ round(.x, 3)))
    ) 
}

test = gen_sim(subset(tesla, filename == ("Tesla Model X CCS adapter_150+kW_SOC vs charge speed")))
test2 = gen_sim(subset(tesla, filename == ("Tesla Model S CCS adapter_150kW_SOC vs charge speed")))

ggplot() +
  geom_point(data = test, aes(x = total.time_min, 
                              y = power,
                              color = filename)) +
  geom_point(data = test2, aes(x = total.time_min, 
                               y = power,
                               color = filename)) + 
  theme(legend.position = "bottom") 

theme(legend.position = "none")
b= ggplot(test2, aes(y = power, x = total.time_min)) +
  geom_point() + 
  theme(legend.position = "none")

a + b 

#one file all mutated for time 
mutate_profile <- function(df) {
  df %>%
    mutate(
      dsoc = c(NA, diff(soc)),
      dsoc = ifelse(dsoc < 0, NA, dsoc),
      dtime_hr = ifelse(abs(power) > 1e-6 & !is.na(dsoc), dsoc / power, 0),
      dtime_hr = replace_na(dtime_hr, 0),
      denergy.trans_kWh = power * dtime_hr,
      total.time_min = round(cumsum(dtime_hr * 60), 2),
      energy.trans_kWh = cumsum(denergy.trans_kWh),
      pack_size = last(energy.trans_kWh) / ((last(soc) - first(soc)) / 100),
      pack_kWh = pack_size * soc / 100,
      check_diff = energy.trans_kWh - pack_kWh + first(soc) * pack_kWh
    ) %>%
    mutate(across(c(total.time_min, energy.trans_kWh, pack_size, pack_kWh, check_diff), round, 3))
}

profiles_m <- profiles %>%
  group_by(filename) %>%
  group_modify(~ mutate_profile(.x)) %>%
  ungroup()

ggplot(profiles_m, aes(x = total.time_min, y = power, color = filename)) +
  geom_point() + 
  theme(legend.position = "none") +
  facet_wrap(~manufacturer)

b = ggplot(subset(profiles_m, manufacturer == "Tesla"), aes(x = total.time_min, y = power, color = filename)) +
  geom_point() + 
  theme(legend.position = "none") +
  facet_wrap(~manufacturer)

c = ggplot(subset(profiles_m, manufacturer == "Tesla"), aes(x = total.time_min, y = soc, color = filename)) +
  geom_point() + 
  theme(legend.position = "none") +
  facet_wrap(~manufacturer)

d = ggplot(subset(profiles, manufacturer == "Tesla"), aes(x = soc, y = power, color = filename)) +
  geom_point() + 
  theme(legend.position = "none") +
  facet_wrap(~manufacturer)

e = ggplot(subset(profiles_m, manufacturer == "Tesla"), aes(x = soc, y = total.time_min, color = filename)) +
  geom_point() + 
  theme(legend.position = "none") +
  facet_wrap(~manufacturer) 
b/c
d/e

tesla_150 = profiles_m %>% filter(filename %in% c("Tesla Model S CCS adapter_150kW_SOC vs charge speed", 
                                                  "Tesla Model X CCS adapter_150+kW_SOC vs charge speed"))

ggplot(tesla_150, aes(x = total.time_min, y = power, color = filename)) +
  geom_point() + 
  theme(legend.position = "bottom") 

#next: add file name to id file then load and zipper
df = read_xlsx("~/Documents/Open Projects/Standard Fast Charge/NCA_slow.xlsx")
view(df)
data = read_xlsx("~/Documents/Open Projects/Standard Fast Charge/NCA_detail.xlsx")
#view(data)

library(lubridate)

data <- data %>%
  rename(
    record = `Record Index`,
    status = Status,
    jump = JumpTo,
    cycle = Cycle,
    step = Step,
    current_mA = `Cur(mA)`,
    voltage_V = `Voltage(V)`,
    capacity_mAh = `CapaCity(mAh)`,
    energy_mWh = `Energy(mWh)`,
    rel_time = `Relative Time(h:min:s.ms)`,
    abs_time = `Absolute Time`
  ) %>%
  mutate(
    rel_time = ymd_hms(rel_time, tz = "UTC"),                     # convert relative time to hms format
    abs_time = ymd_hms(abs_time, tz = "UTC")    # convert absolute time to datetime
  )


dch = data[7:150,]
dch$power_W = dch$current_mA*1000/dch$voltage_V

ggplot(dch, aes(y = dch$power_W, x = dch$rel_time)) +
  geom_point() + 
  theme(legend.position = "none")

view(dch)    


ggplot(dch, aes(y = power_kW, x = abs_time)) +
  geom_point() + 
  theme(legend.position = "none") +
  facet_wrap(~manufacturer)