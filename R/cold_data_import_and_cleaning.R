
#---
# Load dependecies from setup 
#---

source("R/setup-environment.R")

#---
# Cold Tolerance (Chill Coma Recovery) ------
#---

cold <-read_csv("data/Cold_master_sheet_complete_review.csv", 
                col_types = cols(), guess_max = 10000)

# Make variables into factors and create dummy variable for population origin 

cold <- na.omit(cold) %>%
  dplyr::mutate(pop = as.factor(pop)) %>%
  dplyr::mutate(gen = as.factor(gen)) %>%
  dplyr::mutate(sex = as.factor(sex)) %>%
  dplyr::mutate(exp = as.factor(exp)) %>%
  dplyr::mutate(trt = as.factor(trt))

cold <- cold %>% dplyr::mutate(origin = if_else(pop %in% 
                                                  c("Batemans Bay", "Bega Valley", 
                                                    "Brisbane", "Cape Tribulation", 
                                                    "Sydney", "Utchee Creek", "Darwin"), 
                                                "Costal", "Inland"))

# Normalize variation between experiments: 

c.s06 <- cold %>% 
  filter(pop == "S06") %>% 
  dplyr::group_by(exp, sex) %>% 
  dplyr::mutate(emedian = round(median(time),2)) %>% 
  dplyr::ungroup() %>%
  dplyr::group_by(sex, exp, emedian) %>% 
  dplyr::summarise() %>%
  dplyr::group_by(sex) %>%
  dplyr::mutate(median = mean(emedian))

cold <- cold %>% 
  dplyr::left_join(c.s06, by = c("exp", "sex"))%>%
  ungroup %>%
  dplyr::group_by(exp, sex) %>% 
  dplyr::mutate(rtime = round(time/(emedian/median),2)) %>%
  dplyr::ungroup() %>%
  dplyr::group_by(.dots=c("pop", "sex", "gen")) %>% 
  dplyr::mutate(nmedian = round(median(time),2)) %>% 
  dplyr::mutate(pmedian = round(median(rtime),2))%>% 
  droplevels()

# reorganize the factor levels for the domestication status variable: 

cold$status <- factor(cold$status, levels = c("Wild", "Domesticated"))

#j.transformation <- jtrans(cold$rtime, test = "ad.test") # Normality transformation

#cold <- cold %>% ungroup() %>% dplyr::mutate(j.trans.rtime = jtrans(rtime, test = "ad.test", exclude_original = FALSE)$transformed)  # Johnson normality transformation
#jtrans(heat$rtime, test = "lillie.test", exclude_original = FALSE)

cold.trt.p.2 <- cold %>% 
  dplyr::ungroup() %>%
  dplyr::filter(sex == "Male") %>%
  dplyr::select("pop", "pmedian", "sex", "status", "lat") %>%
  dplyr::filter(!pop %in% c("S06")) %>%
  dplyr::distinct() %>% 
  droplevels()

cold.trt.s <- cold.trt.p.2 %>% # For the figures 
  dplyr::ungroup() %>%
  dplyr::group_by(status) %>%
  dplyr::group_split() %>%
  purrr::map(function(x) droplevels(x)) # 1. Females; 2. Males

cold.trt.p.3 <- cold %>% 
  dplyr::ungroup() %>%
  dplyr::select("pop", "pmedian", "sex", "status", "lat", "rtime") %>%
  dplyr::filter(!pop %in% c("S06")) %>%
  dplyr::distinct() %>% 
  droplevels()


# Subsetting by generation

## Wild
cold.wild <- cold %>% # To subset
  dplyr::filter(!exp == 5) %>% 
  dplyr::select("pop", "gen", "sex", "lat", "lon", "status", "origin",
                "emedian", "median", "nmedian", "pmedian") %>%
  droplevels() %>%
  distinct()


cold.wild.1 <- cold.wild %>% # For the weather variables
  dplyr::filter(!pop == "S06") %>%
  droplevels() %>%
  dplyr::ungroup() %>%
  dplyr::select("pop", "gen", "sex", "lat", "lon", "status", "origin",
                "emedian", "median", "nmedian", "pmedian") %>% 
  dplyr::ungroup() %>% 
  dplyr::group_by(.dots=c("pop", "sex", "gen")) %>% 
  dplyr::mutate(proportion = round(nmedian/emedian,2)) %>% 
  droplevels()

cold.wild.1.s <- cold.wild.1 %>% # For the Correlation with the wild 
  dplyr::ungroup() %>%
  dplyr::group_by(sex) %>%
  dplyr::group_split() %>%
  purrr::map(function(x) droplevels(x)) # 1. Females; 2. Males


cold.wild.s <- cold %>%  # For the ANOVA
  dplyr::filter(!exp == 5) %>% 
  dplyr::filter(!pop == "S06") %>%
  droplevels() %>%
  dplyr::ungroup() %>%
  dplyr::select("pop", "gen", "sex", "lat", "lon", "status", "origin",
                "emedian", "median", "nmedian", "pmedian", "rtime") %>% 
  dplyr::ungroup() %>% 
  dplyr::group_by(.dots=c("pop", "sex", "gen")) %>% 
  dplyr::mutate(proportion = round(nmedian/emedian,2)) %>% 
  droplevels() %>%
  dplyr::ungroup() %>%
  dplyr::group_by(sex) %>%
  dplyr::group_split() %>%
  purrr::map(function(x) droplevels(x)) # 1. Females; 2. Males


### Domesticated

cold.old <- cold %>% 
  dplyr::filter(exp == 5) %>% 
  dplyr::select("pop", "gen", "sex", "lat", "lon", "status", "origin", 
                "emedian", "median", "nmedian", "pmedian") %>% 
  dplyr::distinct() %>% 
  droplevels()

cold.old.1 <-cold.old %>% 
  dplyr::filter(!pop == "S06") %>% 
  droplevels() %>%
  dplyr::ungroup() %>%
  dplyr::select("pop", "gen", "sex", "lat", "lon", "status", "origin",
                "emedian", "median", "nmedian", "pmedian") %>% 
  dplyr::ungroup() %>% 
  dplyr::group_by(.dots=c("pop", "sex", "gen")) %>% 
  dplyr::mutate(proportion = round(nmedian/emedian,2)) %>% 
  droplevels()

cold.old.1.s <- cold.old.1 %>%
  dplyr::ungroup() %>%
  dplyr::group_by(sex) %>%
  dplyr::group_split() %>%
  purrr::map(function(x) droplevels(x)) # 1. Females; 2. Males

cold.old.s <- cold %>%
  dplyr::filter(exp == 5) %>% 
  dplyr::filter(!pop == "S06") %>%
  droplevels() %>%
  dplyr::ungroup() %>%
  dplyr::select("pop", "gen", "sex", "lat", "lon", "status", "origin",
                "emedian", "median", "nmedian", "pmedian", "rtime") %>% 
  dplyr::ungroup() %>% 
  dplyr::group_by(.dots=c("pop", "sex", "gen")) %>% 
  dplyr::mutate(proportion = round(nmedian/emedian,2)) %>% 
  droplevels() %>%
  dplyr::ungroup() %>%
  dplyr::group_by(sex) %>%
  dplyr::group_split() %>%
  purrr::map(function(x) droplevels(x)) # 1. Females; 2. Males


#### Data for ANCOVA: 
cold.ancova <- cold %>% 
  dplyr::filter(!pop == "S06") %>%
  dplyr::select("pop", "gen", "sex", "lat", "status", "pmedian") %>%
  droplevels() %>%
  dplyr::distinct() %>%
  dplyr::group_by(sex) %>%
  dplyr::group_split() %>%
  purrr::map(function(x) droplevels(x)) # 1. Females; 2. Males


### ---- Statistical analysis cold wild----

## Correlation wild vs domesticated males

corr.cold.domes.wild.males <- cold.wild.1.s[[2]] %>% 
  dplyr::select("pop", "pmedian") %>%
  dplyr::left_join(cold.old.1.s[[2]], by =c("pop")) %>%
  dplyr::select("pmedian.x", "pmedian.y") %>%
  dplyr::rename(Wild_Males = "pmedian.x") %>%
  dplyr::rename(Domesticated_Males = "pmedian.y")

corr.cold.domes.wild.males.prop <- cold.wild.1.s[[2]] %>% 
  dplyr::select("pop", "lat", "sex","pmedian", "proportion") %>%
  dplyr::left_join(cold.old.1.s[[2]], by =c("pop", "lat", "sex")) %>%
  dplyr::mutate(change = round(pmedian.y/pmedian.x,2)) %>%
  dplyr::select("pop", "lat", "sex", "status", "proportion.x", 
                "proportion.y", "pmedian.x", "pmedian.y", "change") %>%
  dplyr::rename(Wild = "pmedian.x") %>%
  dplyr::rename(Domesticated = "pmedian.y") %>%
  dplyr::rename(proportion.wild = "proportion.x") %>%
  dplyr::rename(proportion.domes = "proportion.y")
