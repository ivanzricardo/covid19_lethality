###################### COVID-19 LETHALITY IN BRAZIL ######################################################
# Script for analyzing COVID-19-related AIH data in Brazil (DOI: 10.1371/journal.pone.0254633            #
# Last update: 07/16/2021 (ivan.zimmermann@unb.br)                                                       #
############################ START #######################################################################

#Install packages
install.packages("tidyverse")
install.packages("sqldf")
install.packages("eeptools")

#Call packages
library(read.dbc)
library(tidyverse)
library(data.table)
library(sqldf)
library(RCurl)
library(eeptools)
library(ggpubr)


################## LOAD DATA ######################
spec_cols <- cols(UF_ZI = col_character(),
N_AIH = col_character(),
MUNIC_RES = col_character(),
NASC = col_date(format = ""),
DT_INTER = col_date(format = ""),
DT_SAIDA = col_date(format = ""),
DIAS_PERM = col_double(),
CNES = col_character(),
DIAGSEC2 = col_character(),
DIAGSEC3 = col_character(),
DIAGSEC4 = col_character(),
DIAGSEC5 = col_character(),
DIAGSEC6 = col_character(),
DIAGSEC7 = col_character(),
DIAGSEC8 = col_character(),
DIAGSEC9 = col_character(),
munResNome = col_character(),
munResUf = col_character()
)

aih_covid_2020 <- read_csv2("aih_covid_2020_db.csv", col_types = spec_cols)

################## ORGANIZE DATA BY EPIDEMIOLOGICAL WEEK ######################
#load epi week reference
epi_week <-  readxl::read_xlsx("Brazil_Epi_Week_2020-2021.xlsx")

#create dates fields
aih_covid_2020$dt_hosp <- as.Date(aih_covid_2020$DT_INTER, "%Y-%m-%d")
aih_covid_2020$dt_birth <- as.Date(aih_covid_2020$NASC, "%Y-%m-%d")
epi_week$dt_start <- as.Date(epi_week$dt_start, "%Y-%m-%d %H:%M:%S")
epi_week$dt_end <- as.Date(epi_week$dt_end, "%Y-%m-%d")

#join the weeks
aih_covid_2020 <- sqldf("SELECT * FROM aih_covid_2020
            LEFT JOIN epi_week
            ON  aih_covid_2020.dt_hosp BETWEEN epi_week.dt_start AND epi_week.dt_end")

################## ORGANIZE DATA BY GEOGRAPHIC REGION ########################
#load regions reference
uf_rg <-  readxl::read_xlsx("AIH_UF_NAME_REGION.xlsx")

#create uf_cod
aih_covid_2020 <- aih_covid_2020 %>% 
  mutate(uf_cod = substr(UF_ZI,1,2))

#join the regions
aih_covid_2020 <- sqldf("SELECT * FROM aih_covid_2020
            LEFT JOIN uf_rg
            ON  aih_covid_2020.uf_cod = uf_rg.uf_cod")
aih_covid_2020$uf_cod<-NULL

##################### ORGANIZE DATA BY AGE RANGE ##########################
#create the age of admission based on date of admission and date of birth
aih_covid_2020 <- aih_covid_2020 %>% 
  mutate(age_hosp = age_calc(aih_covid_2020$dt_birth,aih_covid_2020$dt_hosp, 
                              units = "years"))

#create the age range
aih_covid_2020 <- aih_covid_2020 %>% 
  mutate(age_range = case_when(age_hosp < 20 ~ '00 to 19',
                               age_hosp >= 20 & age_hosp < 40 ~ '20 to 39',
                               age_hosp >= 40 & age_hosp < 60 ~ '40 to 59',
                               age_hosp >= 60 & age_hosp < 80 ~ '60 to 79',
                               age_hosp >= 80 ~ '80 or more',
                                  ))

##################### INCLUDE ADDITIONAL FIELDS ################
#create the length of stay
aih_covid_2020 <- aih_covid_2020 %>% 
  mutate(length = case_when(DIAS_PERM <= 7 ~ 'a) 7 or less',
                             DIAS_PERM > 7 & DIAS_PERM <= 14 ~ 'b) Between 7 and 14',
                             DIAS_PERM > 14 ~ 'c) More than 14'
  ))

#criar variável usou uti
aih_covid_2020 <- aih_covid_2020 %>% 
  mutate(icu = case_when(MARCA_UTI != "Não utilizou UTI" ~ 'Yes',
                         MARCA_UTI == "Não utilizou UTI" ~ 'No'))

#translate sex
aih_covid_2020 <- aih_covid_2020 %>% 
  mutate(sex = case_when(SEXO == "Feminino" ~ 'Female',
                         SEXO == "Masculino" ~ 'Male'
  ))

#translate race_color
aih_covid_2020 <- aih_covid_2020 %>% 
  mutate(race_color = case_when(RACA_COR == "Amarela" ~ 'Yellow',
                                RACA_COR == "Branca" ~ 'White',
                                RACA_COR == "Indígena" ~ 'Native Brazilian',
                                RACA_COR == "Parda" ~ 'Brown',
                                RACA_COR == "Preta" ~ 'Black',
                                RACA_COR == "NA" ~ 'Not available'
  ))

#translate region name
aih_covid_2020 <- aih_covid_2020 %>% 
  mutate(rg_name = case_when(rg_name_pt == "Norte" ~ 'North',
                             rg_name_pt == "Nordeste" ~ 'Northeast',
                             rg_name_pt == "Sudeste" ~ 'Southeast',
                             rg_name_pt == "Sul" ~ 'South',
                             rg_name_pt == "Centro-Oeste" ~ 'Midwest',
                             rg_name_pt == "Ignorado" ~ 'Not available'
  ))

##################### GROUPING AND ANALYSIS #########################
#group admissions by week
admissions_covid_week_total <- aih_covid_2020 %>% 
  group_by(week_hosp) %>% 
  summarise(admissions_total = n()) %>% 
  arrange(-admissions_total)

#group admissions by week and age
admissions_covid_week_age <- aih_covid_2020 %>% 
    group_by(week_hosp, age_range) %>% 
    summarise(admissions = n()) %>% 
    arrange(-admissions)

#group deaths by week
deaths_covid_week_total <-aih_covid_2020 %>% 
  filter(MORTE =="Sim") %>%
  group_by(week_hosp) %>%  
  summarise(deaths_total = n()) %>% 
  arrange(-deaths_total)

#group deaths by week and age
deaths_covid_week_age <-aih_covid_2020 %>% 
  filter(MORTE =="Sim") %>%
  group_by(week_hosp, age_range) %>%  
  summarise(deaths = n()) %>% 
  arrange(-deaths)

#join and calculate the lethality per week of admission
admissions_covid_week_age <- admissions_covid_week_age %>% 
  left_join(deaths_covid_week_age, by = c("week_hosp","age_range"))
admissions_covid_week_age <- admissions_covid_week_age %>% 
  left_join(admissions_covid_week_total, by = c("week_hosp"))
admissions_covid_week_age <- admissions_covid_week_age %>% 
  left_join(deaths_covid_week_total, by = c("week_hosp"))

admissions_covid_week_age$lethality <-  admissions_covid_week_age$deaths/admissions_covid_week_age$admissions
admissions_covid_week_age$lethality_global <-  admissions_covid_week_age$deaths_total/admissions_covid_week_age$admissions_total


#### AGE-STANDARDIZED LETHALITY ###########
#define the standard population
#group admissions per week by age 
std_admissions_covid_week <- aih_covid_2020 %>% 
  group_by(age_range) %>% 
  summarise(hosp_age_std = n())

#group admissions by week
admissions_covid_week <- aih_covid_2020 %>% 
  group_by(week_hosp, age_range) %>% 
  summarise(admissions = n()) 

admissions_covid_week <- admissions_covid_week %>% 
  left_join(std_admissions_covid_week, by = c("age_range"))

#group deaths by week
deaths_covid_week <-aih_covid_2020 %>% 
  filter(MORTE =="Sim") %>%
  group_by(week_hosp, age_range) %>%  
  summarise(deaths = n())

admissions_covid_week <- admissions_covid_week %>% 
  left_join(deaths_covid_week, by = c("week_hosp","age_range"))
admissions_covid_week[is.na(admissions_covid_week$deaths),"deaths"] <- 0

#include sum of admissions, deaths per week and crude lethality
sum_death_week <-  admissions_covid_week %>%  #sum of deaths for the week by sex
  group_by(week_hosp) %>% 
  summarise(death_week = sum(deaths))
sum_hosp_week <-  admissions_covid_week %>%  #sum of admissions for the week by sex
  group_by(week_hosp) %>% 
  summarise(hosp_week = sum(admissions))
admissions_covid_week<- admissions_covid_week %>% 
  left_join(sum_death_week, by = c("week_hosp")) 
admissions_covid_week<- admissions_covid_week %>% 
  left_join(sum_hosp_week, by = c("week_hosp"))  

admissions_covid_week$crude_cfr <-  admissions_covid_week$death_week/ admissions_covid_week$hosp_week

#Include sum of expected deaths and adjusted lethality
admissions_covid_week$age_cfr <-  admissions_covid_week$deaths/admissions_covid_week$admissions
admissions_covid_week$exp_death_age <- admissions_covid_week$age_cfr*admissions_covid_week$hosp_age_std
sum_exp_death_week <-  admissions_covid_week %>%  
  group_by(week_hosp) %>% 
  summarise(exp_death_week = sum(exp_death_age)) 
admissions_covid_week <- admissions_covid_week %>% 
  left_join(sum_exp_death_week, by = c("week_hosp"))  

admissions_covid_week$adjust_cfr <-  admissions_covid_week$exp_death_week/sum(std_admissions_covid_week$hosp_age_std)

### PLOT ######
valid_data <- admissions_covid_week 

n_hosp <- aih_covid_2020 %>%
  summarise(n())

crude_cfr <- ggplot(valid_data, 
                    aes(x = week_hosp, y = crude_cfr)) +
  geom_point(size = 2)+
  geom_smooth(method = "loess") +
  labs(x = "Epidemiological week", y = "Crude Hospital case-fatality rate")+ 
  ylim(0.1, 0.4)


adj_cfr <- ggplot(valid_data, 
                                aes(x = week_hosp, y = adjust_cfr)) +
  geom_point(size = 2)+
  geom_smooth(method = "loess") +
  labs(x = "", y = "")+
  ylim(0.1, 0.4)

figure <- ggarrange(crude_cfr, adj_cfr,
                    #labels = c("CFR by sex (crude vs age-standardized)"), 
                    common.legend = TRUE, legend = "right",
                    ncol = 2, nrow = 1)

annotate_figure(figure,
                top = text_grob("Case-Fatality Rate in Brazil (Crude vs Age-standardized)", 
                                color = "black", face = "bold", size = 12, hjust = 1, x = 0.5)
)