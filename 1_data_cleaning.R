######0 Setup######

#read data
data_raw <-  read.csv("C:\\Users\\SK03\\ownCloud\\dmh_UNICEF_proj\\Raw Data\\DH\\DH_12Countries_110222.csv")

#load packages
packages <- c("tidyverse")
invisible(lapply(packages, library, character.only=TRUE))
rm(packages)

#####1 select data#####
data_sel <- data_raw %>%
  dplyr::select(
  contains("Respondent_ID"), # Participant ID
  ends_with("COUNTRY"), #Country
  ends_with("ECS_SELECTED_CH_AGE"), #age
  ends_with("ECS_SELECTED_CH_GENDER"),  #sex
  starts_with("B1"), #frequency of internet use
  starts_with("C2"), #forms of internet use
  starts_with("D1"), #Digital Skills
  starts_with("H1"), #well-being 1-10 scale
  starts_with("wgt_gross")) #gross sampling weight for inter-country and cross-country analysis
  
######2 create additional variables######
data_sel$cbu_id <- 1:nrow(data_sel)

