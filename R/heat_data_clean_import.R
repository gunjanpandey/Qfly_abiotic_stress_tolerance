
#---
# Check dependencies from setup packages
#---

source("R/setup-environment.R")

#---
# Heat Tolerance (Heat Knockdown Time) data import ------
#---

heat <-read_csv("data/Master_sheet_heat.csv", col_types = cols())

# Select columns to keep from the data and create dummy variable for 
# population origin: 

heat <- heat %>% 
  dplyr::select(num, exp, pop, sex, time, gen, round, lat, lon) %>% 
  na.omit() %>% 
  dplyr::mutate(origin = if_else(pop %in% c("Batemans Bay", "Bega Valley", "Brisbane", 
                                     "Cape Tribulation", "Sydney", "Utchee Creek", 
                                     "Darwin"), "Costal", "Inland"))

# Normalize experiment runs with S06: 

h.s06 <- heat %>% 
  filter(pop == "S06") %>% 
  mutate(mean = round(mean(time),2)) %>% 
  group_by(exp, sex) %>% 
  mutate(emedian = round(median(time),2)) %>% 
  ungroup() %>% 
  group_by(sex, exp, emedian) %>% 
  summarise() %>%
  group_by(sex) %>%
  mutate(median = mean(emedian))


heat <- heat %>% 
  dplyr::left_join(h.s06, by = c("exp","sex")) %>%
  dplyr::ungroup() %>% 
  dplyr::group_by(exp, sex) %>% 
  dplyr::mutate(rtime = round(time/(emedian/median),2)) %>%  # normalize for batch effect
  dplyr::ungroup() %>%
  dplyr::group_by(.dots=c("pop", "sex", "gen")) %>% 
  dplyr::mutate(nmedian = round(median(time),2)) %>% # median knockdown time withouth batch normalization
  dplyr::mutate(pmedian = round(median(rtime),2)) %>% # batch effect normalized median knockdown time
  droplevels() %>%
  dplyr::ungroup() %>%
  dplyr::mutate(exp = as.factor(exp)) %>% # Make the variable exp a factor
  dplyr::mutate(pop = as.factor(pop)) %>% # Make the variable pop a factor
  dplyr::mutate(sex = as.factor(sex)) %>% # Make the variable sex a factor
  dplyr::mutate(gen = as.factor(gen)) %>% # Make the variable gen a factor
  dplyr::mutate(status= ifelse(gen == "G2", "Wild", "Domesticated")) # create dummy varible for domestication status

# subset the data for figure 1: 
heat.domes.2 <- heat %>% 
  ungroup() %>%
  dplyr::filter(sex == "Male") %>%
  dplyr::select("pop", "pmedian", "nmedian","status", "lat") %>%
  dplyr::filter(!pop %in% c("S06")) %>%
  distinct() %>% 
  droplevels()

# Subset the data for GAMMA-GLM model: 

heat.domes.3 <- heat %>% 
  ungroup() %>%
  dplyr::select("pop", "pmedian", "nmedian", "status", "lat", "rtime") %>%
  dplyr::filter(!pop %in% c("S06")) %>%
  distinct() %>% 
  droplevels()

# Reorganized the levels of the domestication status
heat.domes.3$status <- factor(heat.domes.3$status, levels = c("Wild", "Domesticated"))


# Subsetting by generation

## Wild
heat.wild <- heat %>% 
  filter(!exp == 4) %>% 
  dplyr::select("pop", "gen", "sex", "lat", "lon", "status", "origin",
                "emedian", "median", "nmedian", "pmedian") %>%
  droplevels() %>%
  distinct()

heat.wild.1 <- heat.wild %>% 
  filter(!pop == "S06") %>%
  droplevels() %>%
  ungroup() %>%
  dplyr::select("pop", "gen", "sex", "lat", "lon", "status", "origin",
                "emedian", "median", "nmedian", "pmedian") %>% 
  ungroup() %>% 
  dplyr::group_by(.dots=c("pop", "sex", "gen")) %>% 
  mutate(proportion = round(nmedian/emedian,2)) %>% 
  droplevels()

heat.wild.1.s <- heat.wild.1 %>%
  ungroup() %>%
  group_by(sex) %>%
  group_split() %>%
  purrr::map(function(x) droplevels(x))  # 1. Females; 2. Males


heat.wild.s <- heat %>% 
  filter(!exp == 4) %>% 
  filter(!pop == "S06") %>%
  droplevels() %>%
  ungroup() %>%
  dplyr::select("pop", "gen", "sex", "lat", "lon", "status", "origin",
                "emedian", "median", "nmedian", "pmedian", "rtime") %>% 
  ungroup() %>% 
  dplyr::group_by(.dots=c("pop", "sex", "gen")) %>% 
  mutate(proportion = round(nmedian/emedian,2)) %>% 
  droplevels() %>%
  ungroup() %>%
  group_by(sex) %>%
  group_split() %>%
  purrr::map(function(x) droplevels(x)) # 1. Females; 2. Males


## Domesticated

heat.old <- heat %>% 
  filter(exp == 4) %>% 
  dplyr::select("pop", "gen", "sex", "lat", "lon", "status", "origin",
                "emedian", "median", "nmedian", "pmedian") %>% 
  distinct() %>% 
  droplevels()

heat.old.1 <-heat.old %>% 
  filter(!pop == "S06") %>% 
  droplevels() %>%
  ungroup() %>%
  dplyr::select("pop", "gen", "sex", "lat", "lon", "status", "origin",
                "emedian", "median", "nmedian", "pmedian") %>% 
  ungroup() %>% 
  dplyr::group_by(.dots=c("pop", "sex", "gen")) %>% 
  mutate(proportion = round(nmedian/emedian,2)) %>% 
  droplevels()

heat.old.1.s <- heat.old.1 %>%
  ungroup() %>%
  group_by(sex) %>%
  group_split() # 1. Females; 2. Males

heat.old.s <- heat %>%
  filter(exp == 4) %>% 
  filter(!pop == "S06") %>%
  droplevels() %>%
  ungroup() %>%
  dplyr::select("pop", "gen", "sex", "lat", "lon", "status", "origin",
                "emedian", "median", "nmedian", "pmedian", "rtime") %>% 
  ungroup %>% 
  dplyr::group_by(.dots=c("pop", "sex", "gen")) %>% 
  mutate(proportion = round(nmedian/emedian,2)) %>% 
  droplevels() %>%
  ungroup() %>%
  group_by(sex) %>%
  group_split() %>%
  purrr::map(function(x) droplevels(x))# 1. Females; 2. Males

#---
# Weather correlation new dataset cold tolerance: -----
#---

heat.weather <- heat.wild.1 %>% 
  dplyr::left_join(weather_variables, by = "pop") %>% 
  droplevels() %>% 
  dplyr::select("pmedian",
                "mean.max", "mean.min", 
                "mean.rain", "mean.solar", 
                "annual.temp", "max.high.temp", 
                "low.high.temp", "low.min.temp", 
                "high.min.temp", "min.dry.month") %>% 
  distinct()

#---
# Correlation wild vs domesticated flies
#---

corr.heat.wild.males.prop <- heat.wild.1.s[[2]] %>% 
  dplyr::ungroup() %>%
  dplyr::select("pop", "lat", "sex","pmedian", "proportion") %>%
  dplyr::left_join(heat.old.1.s[[2]], by =c("pop", "lat", "sex")) %>%
  dplyr::mutate(change = round(pmedian.y/pmedian.x,2)) %>%
  dplyr::select("pop", "lat", "sex", "status", "proportion.x",
                "proportion.y", "pmedian.x", "pmedian.y", "change") %>%
  dplyr::rename(Wild = "pmedian.x") %>%
  dplyr::rename(Domesticated = "pmedian.y") %>%
  dplyr::rename(proportion.wild = "proportion.x") %>%
  dplyr::rename(proportion.domes = "proportion.y")

