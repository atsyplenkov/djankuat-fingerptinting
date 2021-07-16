################################################################################
#
# Supplementary material for:
# Tsyplenkov, A., Vanmaercke, M., Collins, A.L., Kharchenko, S., Golosov, V.,
# 2021. Elucidating suspended sediment dynamics in a glacierized catchment
# after an exceptional erosion event: The Djankuat catchment, Caucasus Mountains,
# Russia. CATENA 203, 105285. https://doi.org/10.1016/j.catena.2021.105285
#
# Contact: Anatoly Tsyplenkov (atsyplenkov@gmail.com)
# 
# Install required packages   
#
################################################################################

# install pacman to streamline further package installation
# solution by Ilya Kashnitsky from: https://github.com/ikashnitsky/the-lancet-2018/blob/master/R/0-prepare-r-session.R
if (!require("pacman", character.only = TRUE)){
  install.packages("pacman", dep = TRUE)
  if (!require("pacman", character.only = TRUE))
    stop("Package not found")
}

# these are the required packages
pkgs <- c(
  "tidyverse", 
  "magrittr",
  "readxl",
  "writexl",
  "sf",
  "MASS",
  "hrbrthemes",
  "mapview",
  "extrafont",
  "fingerPro",
  "ggsci",
  "here",
  "scales",
  "lubridate",
  "pangaear",
  "repmis",
  "zoo",
  "fasstr",
  "ggpubr"
)


# install the missing packages
# only run if at least one package is missing
if(!sum(!p_isinstalled(pkgs))==0){
  p_install(
    package = pkgs[!p_isinstalled(pkgs)], 
    character.only = TRUE
  )
}

# install development versions
# loadflux (for hydrological event demarcation)
devtools::install_github("atsyplenkov/loadflux")
# atslib 
devtools::install_github("atsyplenkov/atslib")
