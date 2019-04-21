#---
# This script is not run unless the "weather_variables.Rdata" is missing or damage, which should happen. 
# ---

library(dplyr)

# --- 
# First load all pre-process data 
# ---

to.load <- list.files(path = "data/BOM/", 
                      pattern = "*.RData", full.names = TRUE)

lapply(to.load,load, .GlobalEnv)

max.temp.1 <- max.temp %>% 
  dplyr::select(year.max.temp, annual.max.temp, lat, lon, pop, distance)

min.temp.1 <- min.temp %>% 
  dplyr::select(year.min.temp, annual.min.temp, pop)

rain.1 <- rain %>% 
  dplyr::select(year.rain, annual.rain, pop)

solar.1 <- solar %>% 
  dplyr::select(year.solar, annual.solar, pop)

weather <-bind_cols( max.temp.1,min.temp.1, rain.1, solar.1) %>% 
  dplyr::select(year.max.temp, pop, lat, lon, annual.max.temp, 
                annual.min.temp, annual.rain, annual.solar)

# ---
# Combine variables
# ---

weather.summary <- weather %>% dplyr::group_by(pop) %>% 
  dplyr::summarize(mean.max = mean(annual.max.temp, na.rm = TRUE),
                   mean.min = mean(annual.min.temp, na.rm = TRUE),
                   mean.rain = mean(annual.rain, na.rm = TRUE),
                   mean.solar = mean(annual.solar, na.rm = TRUE),
                   annual.temp = mean(abs((annual.max.temp + annual.min.temp)/2), na.rm = TRUE)) %>% 
  droplevels() %>% 
  distinct()

# ---
# Calculate weather variables 
# ---

# maximum temperature of the warmest month
high.max.temp.1 <- high.max.temp %>% 
  dplyr::group_by(pop) %>% 
  dplyr::summarise(max.high.temp = max(High.temp)) %>% 
  dplyr::arrange(max.high.temp)

# minimum temperature of the warmest month
low.max.temp.1 <- low.max.temp %>%  
  dplyr::group_by(pop) %>% 
  dplyr::summarise(low.high.temp = max(High.temp)) %>% 
  dplyr::arrange(low.high.temp)

# minimum temperature of the coldest month
low.min.temp.1 <- low.min.temp %>% 
  dplyr::group_by(pop) %>% 
  dplyr::summarise(low.min.temp = max(High.temp)) %>% 
  dplyr::arrange(low.min.temp)

# maximum temperature of the coldest month
high.min.temp.1 <- high.min.temp %>% 
  dplyr::group_by(pop) %>% 
  dplyr::summarise(high.min.temp = max(High.temp)) %>% 
  dplyr::arrange(high.min.temp)

# Precipitation of the driest and wettest month
rain.annual.1 <- rain.annual %>% 
  dplyr::group_by(pop) %>% 
  dplyr::mutate(min.dry.month= min(monthly.rain)) %>% 
  dplyr::mutate(max.wet.month= max(monthly.rain)) %>%
  dplyr::group_by(pop, min.dry.month, max.wet.month) %>%
  dplyr::summarise() %>%
  dplyr::arrange(min.dry.month, max.wet.month)

#---
# Combine all weather variables into a single data frame and export for later use
#---

weather_variables <- weather.summary %>% 
  dplyr::left_join(high.max.temp.1, by = "pop") %>% 
  dplyr::left_join(low.max.temp.1, by = "pop") %>%
  dplyr::left_join(low.min.temp.1, by = "pop") %>%
  dplyr::left_join(high.min.temp.1, by = "pop") %>%
  dplyr::left_join(rain.annual.1, by = "pop")


save(weather_variables, file = "data/weather_variables.Rdata")


rm("max.temp.1","min.temp.1", "rain.1", "solar.1", "weather", "weather.summary", 
  "high.max.temp.1", "low.max.temp.1", "low.min.temp.1", "high.min.temp.1", 
  "rain.annual.1","max.temp","min.temp", "rain", "solar", 
  "high.max.temp", "low.max.temp", "low.min.temp", "high.min.temp", "rain.annual", "to.load")
