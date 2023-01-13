######0 Options & Setup######
###options:
impute_missing_data #gets defined in master script

#load packages
packages <- c("tidyverse", "readxl")
invisible(lapply(packages, library, character.only=TRUE))
rm(packages)

#read data
data_raw <- read.csv("C:\\Users\\SK03\\ownCloud\\dmh_UNICEF_proj\\Raw Data\\DH\\DH_12Countries_110222.csv")
names(data_raw) <- tolower(names(data_raw))

#read data dictionary
dictionary <- openxlsx::read.xlsx("20210505_UNICEF_data_surveyoverview.xlsx", sheet = 2)
rownames(dictionary) <- as.character(dictionary$Variable.name)

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

######2 filter variables out######

#get missings per column
colMeans(is.na(data_sel))

#delete variables that were only collected in a subset of countries
data_fil <- data_sel %>%
  select(-c("c2e", "c2i", "c2u", "c2v", "d1h"))

######3 recode variables#####

#recode country
data_rec <- data_fil %>% mutate(
  country = recode(country, 
                   `1` = "Ethiopia", 
                   `2` = "Kenya", 
                   `3` = "Mozambique", 
                   `4` = "Nambia",
                   `6` = "Tanzania", 
                   `7` = "Uganda", 
                   `8` = "Cambodia", 
                   `9` = "Indonesia",
                   `10` = "Malaysia", 
                   `11` = "Philippines",
                   `12` = "Thailand", 
                   `13` = "Vietnam"
  )) %>% 
  #recode sex at 2 levels
  mutate(
    sex_fem = recode(ecs_selected_ch_gender,
                 `1` = 0, 
                 `2` = 1
    )) %>%
  #recode age at 6 levels 
  mutate(
    age = recode(ecs_selected_ch_age, 
                 `1` = 12, 
                 `2` = 13, 
                 `3` = 14, 
                 `4` = 15,
                 `5` = 16, 
                 `6` = 17
    ))
  #make all 888s and 999s to NA
data_rec[data_rec == 888] <- NA
data_rec[data_rec == 999] <- NA

#create alternative id variable because original id has duplicates
data_rec$cbu_id <- 1:nrow(data_rec) #needs to be done after 888 999 recoding, otherwise will create missings there

######4 impute missing values######

#run imputation
if(impute_missing_data == 1){
  input_data_mi <- mice::mice (data_rec, m=5, maxit=10, meth="rf", seed = 13012023, pred = mice::quickpred(data_rec))
  saveRDS(input_data_mi,file  ="input_data_mi.rds") 
} else {
  input_data_mi  <- readRDS(file  ="input_data_mi.rds")
}

#save densityplot of the results
png("densityplot_multiple_imputation.png", width = 1000, height = 1000)
mice::densityplot(input_data_mi)
dev.off()

#complete dataset
data_comp <- complete(input_data_mi)

#####5 export data######
data_cleaned <- data_comp 

#save data into parent directory of current wd
saveRDS(data_cleaned, paste(dirname(getwd()), "/data_cleaned.rds", sep = ""))
