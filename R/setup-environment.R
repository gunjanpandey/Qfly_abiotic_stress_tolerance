
# ---
# List of libraries need it for the analysis: 
## As we are using packrat to manage the dependecies of this analysis, all libraries
## should be installed. But to avoid confusion we check that this is the case: 
# ---

# the following function was copied from: 
# https://stackoverflow.com/questions/4090169/elegant-way-to-check-for-missing-packages-and-install-them

load_packages <- function(x){
  for( i in x ){
    #  require returns TRUE invisibly if it was able to load package
    if( ! require( i , character.only = TRUE ) ){
      #  If package was not able to be loaded then re-install
      install.packages( i , dependencies = TRUE )
      #  Load package after installing
      require( i , character.only = TRUE )
    }
  }
}

load_packages(c("readr", "dplyr", "ggplot2", "ggridges", 
                "ggpubr", "cowplot", "purrr", "sp", "egg", 
                "psych", "xtable", "grid", "Hmisc", "jtrans", 
                "nortest", "boot", "emmeans"))


#---
# Import homebrew functions and colour pallete: 
#---

source("R/corstar.R")
source("R/Qfly_palette .R")

#---
# Import Climate data 
#---

source("R/weather_variables.R")

#if(!exists("weather_variables")){
#  if(!file.exists("data/weather_variables.Rdata")){
#    source("R/weather_variables.R")
#  }
#}
