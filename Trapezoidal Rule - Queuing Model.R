vehicle_queue <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
vehicle_position <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
vehicle_SOC <- c(10, 20, 30, 40, 50, 60, 70, 80, 90, 10)
vehicle_waittime <- rep(0, 10)
charger_availability <- TRUE

# Store original SOCs for later comparison
initial_SOC <- vehicle_SOC
delta_charge_time <- rep(0, 10)  # to track how long each vehicle charged

# Discrete SoC vs time lookup table (mock data)
soc_points <- seq(0, 90, by = 10)
time_points <- c(0, 5, 9, 14, 20, 30, 45, 65, 85, 110) # convert to hours 
time_vs_soc <- approxfun(soc_points, time_points)
print(time_vs_soc)

# Trapezoidal rule charging time estimation
estimate_charging_time <- function(start_soc, end_soc) {
  if (start_soc >= end_soc) return(0)
  soc_range <- seq(start_soc, end_soc, by = 1)
  time_vals <- time_vs_soc(soc_range)
  trap_time <- sum((head(time_vals, -1) + tail(time_vals, -1)) / 2)
  return(trap_time)
  df -> data.frame(time_vals = time_vals, trap_time = trap_time, soc = soc_range)
}


# Simulation loop
for (v in vehicle_queue) {
  if (charger_availability) {
    charger_availability <- FALSE
    
    # Estimate charging time
    t_charge <- estimate_charging_time(vehicle_SOC[v], 90)
    delta_charge_time[v] <- t_charge  # save it
    
    vehicle_SOC[v] <- 90
    vehicle_position[v] <- NA
    
    for (i in seq_along(vehicle_waittime)) {
      if (i != v && !is.na(vehicle_position[i])) {
        vehicle_waittime[i] <- vehicle_waittime[i] + t_charge
      }
    }
    charger_availability <- TRUE
  }
}

ggplot(df, aes(x = time_vals, y = soc)) + geom_point()

# Output final dataframe
vehicle_dataframe <- data.frame(
  Vehicle = vehicle_queue,
  Initial_SOC = initial_SOC,
  Final_SOC = vehicle_SOC,
  Delta_Charge_Time = round(delta_charge_time, 2),
  Wait_Time = round(vehicle_waittime, 2),
  Position = vehicle_position
)


# another column to relate wait time to cumalative time, delta t values 

