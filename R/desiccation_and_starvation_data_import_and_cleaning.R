
#---
# Check dependencies from setup packages
#---

source("R/setup-environment.R")
source("R/heat_data_clean_import.R")
source("R/cold_data_import_and_cleaning.R")


#---
# Desication and starvaton tolerance data import -----
#---

desiccation.all <- read_csv("data/Master_Sheet_Desiccation_all_repeat.csv", na = "NA", 
                            col_types = cols(), guess_max = 10000)

latlong <-  read_csv("data/Lat-and-long-pops.csv", col_types = cols()) %>% 
  mutate(pop = as.factor(pop))

# setup columns to keep from the data and create dummy variables
desiccation <- desiccation.all %>% 
  filter(time > 0) %>% 
  na.omit() %>%
  mutate(pops = as.factor(pops)) %>% 
  mutate(trt = as.factor(trt)) %>% 
  mutate(gen = as.factor(gen)) %>%
  mutate(pop = as.factor(pop))

## Normalize experiment runs with S06: 

des.s06 <- desiccation %>% 
  filter(pops == "S06") %>%
  mutate(tmedian = round(median(time), 2)) %>% 
  group_by(.dots=c("exp", "sex", "trt")) %>% 
  mutate(emedian = round(median(time),2)) %>%
  mutate(nemedian = ifelse(exp == 8, tmedian, round(emedian, 2))) %>% 
  ungroup() %>% 
  group_by(exp, sex, trt, nemedian) %>%
  summarise() %>%
  group_by(sex, trt) %>%
  mutate(median = mean(nemedian))


desiccation.1 <- desiccation %>% 
  left_join(des.s06, by = c("exp", "trt", "sex")) %>% 
  dplyr::select(-c("pop", "com", "num"))  %>% 
  droplevels() %>% 
  dplyr::rename(pop = "pops") %>%
  mutate(pop = as.factor(pop)) %>% 
  ungroup() %>% 
  dplyr::group_by(gen, exp, rep, trt) %>% 
  mutate(rtime = round(time/(nemedian/median), 2)) %>% 
  droplevels() %>% 
  ungroup() %>% 
  dplyr::left_join(latlong, by = "pop") %>%
  group_by(.dots = c("exp", "pop", "sex", "trt", "gen", "rep")) %>% 
  mutate(nmedian = round(median(time), 2)) %>%
  mutate(pmedian = round(median(rtime), 2)) %>%
  ungroup() %>%
  group_by(.dots = c("exp", "pop", "sex", "trt", "gen")) %>% 
  mutate(mean.pmedian = round(median(rtime), 2))

des.domes.repeat <- desiccation.1 %>%
  ungroup() %>%
  dplyr::select("pop", "sex", "rep","gen", "lat", "trt", "nmedian", "nemedian", "pmedian", "mean.pmedian") %>%
  dplyr::filter(!pop %in% c("S06")) %>%
  dplyr::mutate(gen = case_when(gen == "G10" ~ "G10+", gen == "G11" ~ "G10+", TRUE ~ as.character(gen))) %>% 
  dplyr::mutate(trt = case_when(trt == "Control" ~ "Starvation", trt == "Desiccant" ~ "Desiccation")) %>%
  distinct() %>% 
  droplevels()

des.domes <- desiccation.1 %>%
  filter(!gen %in% c("G2_9", "G3_9")) %>%
  filter(sex == "Male") %>%
  ungroup() %>%
  dplyr::select("pop", "sex", "rep","gen", "lat", "trt", "pmedian", "mean.pmedian") %>%
  dplyr::filter(!pop %in% c("S06", "Canberra", "Cape Tribulation", "Darwin", "Bega Valley")) %>%
  dplyr::mutate(gen = case_when(gen == "G10" ~ "G10+", gen == "G11" ~ "G10+", TRUE ~ as.character(gen))) %>% 
  dplyr::mutate(trt = case_when(trt == "Control" ~ "Starvation", trt == "Desiccant" ~ "Desiccation")) %>%
  distinct() %>% 
  droplevels()

des.domes$gen <- factor(des.domes$gen, levels = c("G2", "G4", "G6", "G8", "G10+"))

des.domes.sd <- des.domes %>% 
  group_by(pop, gen, trt) %>%
  summarise(
    SD = round(sd(pmedian),2),
    pmedian = round(mean(pmedian),2)) 


desiccant <- desiccation.1 %>% 
  filter(!gen %in% c("G2_9", "G3_9")) %>%
  dplyr::filter(trt == "Desiccant") %>%
  droplevels() 

starvation <- desiccation.1 %>% 
  filter(!gen %in% c("G2_9", "G3_9")) %>%
  dplyr::filter(trt == "Control") %>%
  droplevels()


desiccant.glm <- desiccation.1 %>% 
  ungroup() %>%
  dplyr::filter(!gen %in% c("G2_9", "G3_9")) %>%
  dplyr::filter(trt == "Desiccant") %>%
  dplyr::filter(sex == "Male") %>%
  dplyr::mutate(gens = as.numeric(gsub("[^0-9]", "", gen))) %>% 
  dplyr::mutate(pop = as.factor(pop)) %>% 
  dplyr::mutate(sex = as.factor(sex)) %>% 
  dplyr::mutate(trt = as.factor(trt)) %>% 
  dplyr::filter(!pop %in% c("Cape Tribulation", "Darwin", "Canberra", "Bega Valley", "S06")) %>%
  dplyr::select("pmedian", "pop", "gens", "rtime") %>% 
  dplyr::distinct() %>%
  droplevels()

starvation.glm <- desiccation.1 %>% 
  ungroup() %>%
  dplyr::filter(!gen %in% c("G2_9", "G3_9")) %>%
  dplyr::filter(trt == "Control") %>%
  dplyr::filter(sex == "Male") %>%
  dplyr::mutate(gens = as.numeric(gsub("[^0-9]", "", gen))) %>% 
  dplyr::mutate(pop = as.factor(pop)) %>% 
  dplyr::mutate(sex = as.factor(sex)) %>% 
  dplyr::mutate(trt = as.factor(trt)) %>% 
  dplyr::filter(!pop %in% c("Cape Tribulation", "Darwin", "Canberra", "Bega Valley", "S06")) %>%
  dplyr::select("pmedian", "pop", "gens", "rtime") %>% 
  dplyr::distinct() %>%
  droplevels()


# Subsetting by generation
## Wild


### Desiccation wild
desiccant.wild <- desiccant %>%
  filter(gen == "G2") %>%
  ungroup() %>%
  dplyr::select("pop", "gen", "sex", "lat", "trt", 
                "nemedian", "median", "nmedian", "pmedian") %>%
  ungroup() %>%
  mutate(status = "Wild") %>%
  dplyr::group_by(.dots=c("pop", "sex", "gen")) %>%
  mutate(proportion = round(nmedian/nemedian, 2)) %>%
  mutate(origin = if_else(pop %in% c("Batemans Bay", "Bega Valley", "Brisbane",
                                     "Cape Tribulation", "Sydney", "Utchee Creek",
                                     "Darwin"), 
                          "Costal", "Inland")) %>%
  droplevels()

desiccant.wild.1.s <- desiccant.wild %>%
  ungroup() %>%
  distinct() %>%
  group_by(sex) %>%
  group_split() %>%   
  purrr::map(function(x) droplevels(x)) # 1 Females; 2. Males

desiccant.wild.s <- desiccant %>%
  filter(gen == "G2") %>%
  ungroup() %>%
  dplyr::select("pop", "gen", "sex", "lat", "nemedian", "trt",
                "median", "nmedian", "pmedian", "rtime") %>%
  ungroup() %>%
  dplyr::group_by(.dots=c("pop", "sex", "gen")) %>%
  mutate(proportion = round(nmedian/nemedian, 2)) %>%
  mutate(status = "Wild") %>%
  droplevels() %>%
  ungroup() %>%
  group_by(sex) %>%
  group_split() %>%   
  purrr::map(function(x) droplevels(x)) # 1 Females; 2. Males

### Starvation wild
starvation.wild <- starvation %>%
  filter(gen == "G2") %>%
  ungroup() %>%
  dplyr::select("pop", "gen", "sex", "lat", "trt",
                "nemedian", "median", "nmedian", "pmedian") %>%
  ungroup() %>%
  mutate(status = "Wild") %>%
  dplyr::group_by(.dots=c("pop", "sex", "gen")) %>%
  mutate(proportion = round(nmedian/nemedian, 2)) %>%
  mutate(origin = if_else(pop %in% c("Batemans Bay", "Bega Valley", "Brisbane",
                                     "Cape Tribulation", "Sydney", "Utchee Creek",
                                     "Darwin"), 
                          "Costal", "Inland")) %>%
  droplevels()

starvation.wild.1.s <- starvation.wild %>%
  ungroup() %>%
  distinct() %>%
  group_by(sex) %>%
  group_split() %>%   
  purrr::map(function(x) droplevels(x)) # 1 Females; 2. Males

starvation.wild.s <- starvation %>%
  filter(gen == "G2") %>%
  ungroup() %>%
  dplyr::select("pop", "gen", "sex", "lat", "nemedian", "trt",
                "median", "nmedian", "pmedian", "rtime") %>%
  ungroup() %>%
  dplyr::group_by(.dots=c("pop", "sex", "gen")) %>%
  mutate(proportion = round(nmedian/nemedian, 2)) %>%
  mutate(status = "Wild") %>%
  droplevels() %>%
  ungroup() %>%
  group_by(sex) %>%
  group_split() %>%
  purrr::map(function(x) droplevels(x)) # 1 Females; 2. Males


## Wild vs Domesticated data: 

des.wild.repeat <- des.domes.repeat %>%
  filter(gen %in% c("G2","G2_9", "G3_9", "G10+")) %>%
  dplyr::mutate(status = case_when(gen == "G2" ~ "Wild", gen == "G10+" ~ "Domesticated",
                                   gen == "G2_9" ~ "Wild", gen == "G3_9" ~ "Wild",
                                   TRUE ~ as.character(gen))) %>%
  dplyr::mutate(status.1 = case_when(gen == "G2" ~ "Wild", gen == "G10+" ~ "Domesticated",
                                     gen == "G2_9" ~ "Wild_repeated", gen == "G3_9" ~ "Wild_repeated",
                                     TRUE ~ as.character(gen))) %>% 
  ungroup() %>%
  dplyr::select("pop", "gen", "sex", "trt","lat", "status", "status.1","pmedian", "nmedian", "nemedian") %>%
  ungroup() %>%
  dplyr::group_by(.dots=c("pop", "sex", "gen")) %>%
  mutate(proportion = round(nmedian/nemedian, 2)) %>%
  droplevels()

des.wild.repeat$status <- factor(des.wild.repeat$status, levels = c("Wild", "Domesticated"))

des.wild.repeat.2 <- des.wild.repeat %>%
  dplyr::filter(status.1 %in% c("Wild", "Domesticated")) %>% 
  dplyr::filter(sex == "Male") %>%
  droplevels() %>%
  dplyr::ungroup() %>%
  group_by(.dots=c("pop", "gen", "sex", "lat", "trt", "status", "status.1")) %>% 
  transmute(pmedian = mean(pmedian),
            nmedian = mean(nmedian),
            nemedian = mean(nemedian),
            proportion = mean(proportion)) %>% 
  distinct() %>%
  ungroup() %>%
  dplyr::group_by(status) %>%
  dplyr::group_split() %>% 
  purrr::map(function(x) droplevels(x))



## ----- Domesticated ------

### Desiccation domesticated
desiccant.domes <- desiccant %>%
  filter(gen %in% c("G10", "G11")) %>%
  ungroup() %>%
  dplyr::select("pop", "gen", "sex", "lat", "trt", 
                "nemedian", "median", "nmedian", "pmedian") %>%
  ungroup() %>%
  mutate(status = "Domesticated") %>%
  dplyr::group_by(.dots=c("pop", "sex", "gen")) %>%
  mutate(proportion = round(nmedian/nemedian, 2)) %>%
  droplevels()

desiccant.domes.1.s <- desiccant.domes %>%
  ungroup() %>%
  distinct() %>%
  group_by(.dots=c("pop", "gen", "sex", "lat", "trt", "status")) %>% 
  transmute(pmedian = mean(pmedian),
            median = mean(median),
            nmedian = mean(nmedian),
            nemedian = mean(nemedian),
            proportion = mean(proportion)) %>% 
  distinct() %>%
  ungroup() %>%
  group_by(sex) %>%
  group_split() %>%   
  purrr::map(function(x) droplevels(x)) # 1 Females; 2. Males

desiccant.domes.s <- desiccant %>%
  filter(gen %in% c("G10", "G11")) %>%
  ungroup() %>%
  dplyr::select("pop", "gen", "sex", "lat", "nemedian", "trt",
                "median", "nmedian", "pmedian", "rtime") %>%
  ungroup() %>%
  mutate(status = "Domesticated") %>%
  dplyr::group_by(.dots=c("pop", "sex", "gen")) %>%
  mutate(proportion = round(nmedian/nemedian, 2)) %>%
  droplevels() %>%
  ungroup() %>%
  group_by(sex) %>%
  group_split() %>%   
  purrr::map(function(x) droplevels(x)) # 1 Females; 2. Males


### Starvation domesticated
starvation.domes <- starvation %>%
  filter(gen %in% c("G10", "G11")) %>%
  ungroup() %>%
  dplyr::select("pop", "gen", "sex", "lat", "trt",
                "nemedian", "median", "nmedian", "pmedian") %>%
  ungroup() %>%
  mutate(status = "Domesticated") %>%
  dplyr::group_by(.dots=c("pop", "sex", "gen")) %>%
  mutate(proportion = round(nmedian/nemedian, 2)) %>%
  droplevels()

starvation.domes.1.s <- starvation.domes %>%
  ungroup() %>%
  distinct() %>%
  group_by(.dots=c("pop", "gen", "sex", "lat", "trt", "status")) %>% 
  transmute(pmedian = mean(pmedian),
            median = mean(median),
            nmedian = mean(nmedian),
            nemedian = mean(nemedian),
            proportion = mean(proportion)) %>% 
  distinct() %>%
  ungroup() %>%
  group_by(sex) %>%
  group_split() %>%   
  purrr::map(function(x) droplevels(x)) # 1 Females; 2. Males

starvation.domes.s <- starvation %>%
  filter(gen %in% c("G10", "G11")) %>%
  ungroup() %>%
  dplyr::select("pop", "gen", "sex", "lat", "nemedian", "trt",
                "median", "nmedian", "pmedian", "rtime") %>%
  ungroup() %>%
  mutate(status = "Domesticated") %>%
  dplyr::group_by(.dots=c("pop", "sex", "gen")) %>%
  mutate(proportion = round(nmedian/nemedian, 2)) %>%
  droplevels() %>%
  ungroup() %>%
  group_by(sex) %>%
  group_split() %>%   purrr::map(function(x) droplevels(x)) # 1 Females; 2. Males


####---- Correlations wild -----

### Correlation treatments: 

corr.desiccation.and.starvation.wild.males <- desiccant.wild.1.s[[2]] %>%
  dplyr::select("pop", "lat", "gen", "pmedian", "trt") %>%
  left_join(starvation.wild.1.s[[2]], by = c("pop", "lat", "gen")) %>%
  dplyr::select("pmedian.x", "pmedian.y") %>% 
  dplyr::rename(Male_desiccation = "pmedian.x") %>%
  dplyr::rename(Male_starvation = "pmedian.y")

### Correlation treatments: 

corr.desiccation.and.starvation.domes.males <- desiccant.domes.1.s[[2]] %>%
  dplyr::select("pop", "lat", "gen", "pmedian", "trt") %>%
  left_join(starvation.domes.1.s[[2]], by = c("pop", "lat", "gen")) %>%
  dplyr::select("pmedian.x", "pmedian.y") %>% 
  dplyr::rename(Male_desiccation = "pmedian.x") %>%
  dplyr::rename(Male_starvation = "pmedian.y")


### Correlation old vs domesticated females desiccation:

corr.desiccation.domes.wild.females <- desiccant.wild.1.s[[1]] %>% 
  dplyr::select("pop", "pmedian") %>%
  left_join(desiccant.domes.1.s[[1]], by =c("pop")) %>%
  dplyr::select("pmedian.x", "pmedian.y") %>%
  dplyr::rename(Wild_Females = "pmedian.x") %>%
  dplyr::rename(Domesticated_Females = "pmedian.y")

corr.desiccation.domes.wild.females.prop <- desiccant.wild.1.s[[1]] %>% 
  dplyr::select("pop", "lat", "sex","pmedian", "trt", "proportion") %>%
  left_join(desiccant.domes.1.s[[1]], by =c("pop", "lat", "sex")) %>%
  mutate(change = round(pmedian.y/pmedian.x,2)) %>%
  dplyr::select("pop", "lat", "sex", "status", "trt.x", "proportion.x", 
                "pmedian.x", "trt.y", "proportion.y", "pmedian.y", "change") %>%
  dplyr::rename(Wild = "pmedian.x") %>%
  dplyr::rename(Domesticated = "pmedian.y") %>%
  dplyr::rename(proportion.wild = "proportion.x") %>%
  dplyr::rename(proportion.domes = "proportion.y")

### Correlation old vs domesticated males desiccation:
corr.desiccation.domes.wild.males <- desiccant.wild.1.s[[2]] %>% 
  dplyr::select("pop", "pmedian") %>%
  left_join(desiccant.domes.1.s[[2]], by =c("pop")) %>%
  dplyr::select("pmedian.x", "pmedian.y") %>%
  dplyr::rename(Wild_Males = "pmedian.x") %>%
  dplyr::rename(Domesticated_Males = "pmedian.y")

corr.desiccation.domes.wild.males.prop <- desiccant.wild.1.s[[2]] %>% 
  dplyr::select("pop", "lat", "sex","pmedian", "trt", "proportion") %>%
  left_join(desiccant.domes.1.s[[2]], by =c("pop", "lat", "sex")) %>%
  mutate(change = round(pmedian.y/pmedian.x,2)) %>%
  dplyr::select("pop", "lat", "sex", "status", "trt.x", "proportion.x", 
                "pmedian.x", "trt.y", "proportion.y", "pmedian.y", "change") %>%
  dplyr::rename(Wild = "pmedian.x") %>%
  dplyr::rename(Domesticated = "pmedian.y") %>%
  dplyr::rename(proportion.wild = "proportion.x") %>%
  dplyr::rename(proportion.domes = "proportion.y")

### Correlation old vs domesticated females starvation:

corr.starvation.domes.wild.females <- starvation.wild.1.s[[1]] %>% 
  dplyr::select("pop", "pmedian") %>%
  left_join(starvation.domes.1.s[[1]], by =c("pop")) %>%
  dplyr::select("pmedian.x", "pmedian.y") %>%
  dplyr::rename(Wild_Females = "pmedian.x") %>%
  dplyr::rename(Domesticated_Females = "pmedian.y")

corr.starvation.domes.wild.females.prop <- starvation.wild.1.s[[1]] %>% 
  dplyr::select("pop", "lat", "sex","pmedian", "trt", "proportion") %>%
  left_join(starvation.domes.1.s[[1]], by =c("pop", "lat", "sex")) %>%
  mutate(change = round(pmedian.y/pmedian.x,2)) %>%
  dplyr::select("pop", "lat", "sex", "status", "trt.x", "proportion.x", 
                "pmedian.x", "trt.y", "proportion.y", "pmedian.y", "change") %>%
  dplyr::rename(Wild = "pmedian.x") %>%
  dplyr::rename(Domesticated = "pmedian.y") %>%
  dplyr::rename(proportion.wild = "proportion.x") %>%
  dplyr::rename(proportion.domes = "proportion.y")

### Correlation old vs domesticated males starvation:

corr.starvation.domes.wild.males <- desiccant.wild.1.s[[2]] %>% 
  dplyr::select("pop", "pmedian") %>%
  left_join(starvation.domes.1.s[[2]], by =c("pop")) %>%
  dplyr::select("pmedian.x", "pmedian.y") %>%
  dplyr::rename(Wild_Males = "pmedian.x") %>%
  dplyr::rename(Domesticated_Males = "pmedian.y")

corr.starvation.domes.wild.males.prop <- starvation.wild.1.s[[2]] %>% 
  dplyr::select("pop", "lat", "sex","pmedian", "trt", "proportion") %>%
  left_join(starvation.domes.1.s[[2]], by =c("pop", "lat", "sex")) %>%
  mutate(change = round(pmedian.y/pmedian.x,2)) %>%
  dplyr::select("pop", "lat", "sex", "status", "trt.x", "proportion.x", 
                "pmedian.x", "trt.y", "proportion.y", "pmedian.y", "change") %>%
  dplyr::rename(Wild = "pmedian.x") %>%
  dplyr::rename(Domesticated = "pmedian.y") %>%
  dplyr::rename(proportion.wild = "proportion.x") %>%
  dplyr::rename(proportion.domes = "proportion.y")

#### Proportions of change in domestication

desiccation.change.domestication <- corr.desiccation.domes.wild.males.prop %>% 
  bind_rows(corr.starvation.domes.wild.males.prop)

#corr.starvation.domes.wild.females.prop, corr.starvation.domes.wild.males.prop)

desiccation.change.domestication <- desiccation.change.domestication %>% 
  dplyr::ungroup() %>%
  dplyr::group_by(.dots =c("pop", "sex", "status", "trt.x")) %>% 
  dplyr::mutate(change = round(mean(change),3)) %>%
  dplyr::mutate(proportion.wild = round(mean(proportion.wild),3)) %>%
  dplyr::mutate(proportion.domes = round(mean(proportion.domes),3)) %>%
  dplyr::ungroup() %>%
  dplyr::select(-c("Domesticated", "Wild")) %>%
  dplyr::mutate(trt.x = case_when(trt.x == "Control" ~ "Starvation", trt.x == "Desiccant" ~ "Desiccation")) %>%
  dplyr::distinct() %>%
  droplevels()

#### Proportion of change in domestication by generation: 

propotional.change.generations <-  desiccation.1 %>%
  filter(!gen %in% c("G2_9", "G3_9")) %>%
  ungroup() %>%
  dplyr::select("pop", "sex", "rep","gen", "lat", "trt", "nemedian", "nmedian", "pmedian") %>%
  dplyr::filter(!pop %in% c("S06")) %>%
  dplyr::mutate(gen = case_when(gen == "G10" ~ "G10+", gen == "G11" ~ "G10+", TRUE ~ as.character(gen))) %>% 
  dplyr::mutate(trt = case_when(trt == "Control" ~ "Starvation", trt == "Desiccant" ~ "Desiccation")) %>%
  distinct() %>% 
  droplevels() %>%
  ungroup() %>%
  group_by(.dots=c("pop", "sex", "rep", "trt")) %>%
  dplyr::mutate(proportion = round(nmedian/nemedian,2)) %>%
  ungroup() %>% 
  group_by() %>% 
  group_split(gen)

##### Correlation between treatments : ----

corr.trt.domes <- heat.old.1.s[[2]] %>% dplyr::select("pop", "pmedian") %>% 
  dplyr::left_join(cold.old.1.s[[2]], by =c("pop")) %>%
  dplyr::rename(heat = "pmedian.x") %>%
  dplyr::rename(cold = "pmedian.y") %>%
  dplyr::left_join(desiccant.domes.1.s[[2]], by = c("pop")) %>% 
  dplyr::rename(des = "pmedian") %>% 
  dplyr::left_join(starvation.domes.1.s[[2]], by = c("pop")) %>%
  dplyr::rename(star = "pmedian") %>% 
  dplyr::select("heat", "cold", "des", "star")


corr.trt.wild <- heat.wild.1.s[[2]] %>% 
  dplyr::select("pop", "pmedian") %>%
  left_join(cold.wild.1.s[[2]], by =c("pop")) %>% 
  dplyr::rename(heat = "pmedian.x") %>%
  dplyr::rename(cold = "pmedian.y") %>%
  left_join(desiccant.wild.1.s[[2]], by = c("pop")) %>% 
  dplyr::rename(des = "pmedian") %>% 
  left_join(starvation.wild.1.s[[2]], by = c("pop")) %>%
  dplyr::rename(star = "pmedian") %>%
  dplyr::select("heat", "cold", "des", "star")

corr.trt <- dplyr::bind_rows(corr.trt.wild, corr.trt.domes) %>% round(.,2)

#---
# Repeatibility of results -----
#---

desiccation.repeat.AS_SY <- desiccation.1 %>%
  filter(gen %in% c("G2", "G2_9", "G3_9")) %>%
  filter(pop %in% c("Alice Springs", "Sydney")) %>%
  mutate(pop_gen = paste(pop, gen, sep = "_"))

repeated.desiccation <- read_csv("data/Master_desiccation_resampling.csv") 

repeated.desiccation <- repeated.desiccation %>% mutate_at(vars(-c(time)), list(factor)) %>% 
  filter(gens %in% c("G2", "G100")) %>% droplevels()

repeated.desiccation.s06 <- repeated.desiccation %>%
  filter(pop == "S06") %>% 
  group_by(exp) %>% 
  mutate(emedian = round(median(time),2)) %>% 
  ungroup() %>% 
  mutate(median = round(mean(emedian),2)) %>%
  group_by(exp, emedian, median) %>%
  summarise()

repeated.desiccation <- repeated.desiccation %>% left_join(repeated.desiccation.s06, by = "exp")

repeated.desiccation <- repeated.desiccation %>% 
  group_by(exp) %>%
  mutate(rtime = round(time/(emedian/median),2)) %>%
  ungroup() %>%
  group_by(.dots=c("pop", "exp", "gens")) %>%
  mutate(pmedian = round(median(rtime),2)) %>%
  #mutate(proportion = round(nmedian/emedian,2)) %>%
  ungroup() %>%
  dplyr::select(c("pop", "exp", "gens", "gen", "pmedian")) %>% #, "nmedian", "emedian","proportion")) %>%
  filter(!pop == "S06") %>%
  distinct() %>% 
  droplevels() %>% 
  dplyr::mutate(sample = case_when(exp == "1" ~ "first", exp == "3" ~ "first", exp == "9" ~ "second",exp == "11" ~ "second",exp == "12" ~ "second", TRUE ~ as.character(exp)))

# Desiccation

desiccant.repeat <- desiccation.repeat.AS_SY %>%
  dplyr::filter(trt == "Desiccant") %>%
  droplevels()

desiccant.repeat.s <- desiccant.repeat %>%
  ungroup() %>%
  dplyr::select("pop", "gen", "pop_gen", "sex", "lat", "nemedian", 
                "median", "nmedian", "pmedian", "rtime") %>%
  ungroup() %>%
  group_by(sex, pop) %>%
  group_split() %>%
  purrr::map(function(x) droplevels(x))  # 1. Females; 2. Males

#Starvation
starvation.repeat <- desiccation.repeat.AS_SY %>% 
  dplyr::filter(trt == "Control") %>%
  droplevels()

starvation.repeat.s <- starvation.repeat %>%
  ungroup() %>%
  dplyr::select("pop", "gen", "pop_gen", "sex", "lat", "nemedian", 
                "median", "nmedian", "pmedian", "rtime") %>%
  ungroup() %>%
  group_by(sex, pop) %>%
  group_split() %>%
  purrr::map(function(x) droplevels(x))  # 1. Females; 2. Males

