setwd("~/Desktop/Sociology/Research/punitive regimes/nlsy79/")
options(mc.cores = parallel::detectCores())

library(arm)
library(tidyverse)
library(rstanarm)
library(lme4)

d_all79B <- read.csv("data/nlsy79_extraconfounders/nlsy79_extraconfounders.csv")

d_non_interview <- as_tibble(d_all79B) %>% 
  dplyr::select(starts_with("URBAN"), CASEID) %>%
  gather("variable", "non_interview", -CASEID) %>%
  mutate(variable=gsub("URBAN.", "", variable, fixed=T)) %>%
  separate(variable, c("garbage", "wave"), sep="\\.") %>%
  mutate(wave=as.numeric(wave)) %>%
  mutate(wave= replace(wave, is.na(wave), 0),
         non_interview = ifelse(non_interview== -5, 1, 0)) %>%
  dplyr::select(-garbage) 

d_all79B[d_all79B == -1] = NA  # Refused 
d_all79B[d_all79B == -2] = NA  # Dont know 
d_all79B[d_all79B == -3] = NA  # Invalid missing 
d_all79B[d_all79B == -4] = NA  # Valid missing 
d_all79B[d_all79B == -5] = NA  # Non-interview 

d_age <- as_tibble(d_all79B) %>% 
  dplyr::select(starts_with("AGEATIN"), CASEID) %>%
  gather("variable", "age", -CASEID) %>%
  separate(variable, c("garbage", "wave"), sep="\\.") %>%
  mutate(wave=as.numeric(wave)) %>%
  mutate(wave= replace(wave, is.na(wave), 0)) %>%
  dplyr::select(-garbage)

d_incarcerated <- as_tibble(d_all79B) %>% 
  dplyr::select(starts_with("hh1"), CASEID) %>%
  gather("variable", "incarcerated", -CASEID) %>%
  mutate(variable=gsub("H1.", "", variable, fixed=T))%>%
  separate(variable, c("garbage", "wave"), sep="\\.") %>%
  mutate(wave=as.numeric(wave)) %>%
  mutate(wave= replace(wave, is.na(wave), 0)) %>%
  dplyr::select(-garbage) %>%
  mutate(incarcerated = ifelse(incarcerated == 5, 1, 0)) %>%
  group_by(CASEID) %>%
  mutate(incarcerated= replace(incarcerated, is.na(incarcerated), 0)) %>%
  # mutate(ever_inc = dplyr::lag(incarcerated, order_by = wave)) %>%
  mutate(ever_inc = ifelse(incarcerated == 0, NA, incarcerated))%>%
  fill(ever_inc)%>%
  mutate(ever_inc = ifelse(is.na(ever_inc), 0, 1))%>%
  mutate(ever_inc = ifelse(incarcerated == 1 & ever_inc == 1, 0, ever_inc))

d_income <- as_tibble(d_all79B) %>% 
  dplyr::select(starts_with("Q13.5"), CASEID) %>%
  gather("variable", "income_past_year", -CASEID) %>%
  mutate(variable=gsub("3.5", "", variable, fixed=T)) %>%
  separate(variable, c("garbage", "wave"), sep="\\.") %>%
  mutate(wave = as.numeric(wave)) %>%
  mutate(wave = replace(wave, is.na(wave), 0)) %>%
  mutate(wave = case_when(garbage=="Q1_TRUNC_REVISED" ~ wave + 3,
                          garbage=="Q1_TRUNC" ~ wave + 19,
                          TRUE ~ wave)) %>%
  group_by(CASEID) %>%
  arrange(wave) %>%
  mutate(income_currentY = dplyr::lead(income_past_year)) %>%
  dplyr::select(-garbage)

d_poverty <-as_tibble(d_all79B) %>% 
  dplyr::select(starts_with("POVSTATUS"), CASEID) %>%
  gather("variable", "pov.status", -CASEID) %>%
  separate(variable, c("garbage", "wave"), sep="\\.") %>%
  mutate(wave = as.numeric(wave)) %>%
  mutate(wave = replace(wave, is.na(wave), 0)) %>%
  dplyr::select(-garbage)

d_urban <- as_tibble(d_all79B) %>% 
  dplyr::select(starts_with("URBAN"), CASEID) %>%
  gather("variable", "urban", -CASEID) %>%
  mutate(variable=gsub("URBAN.", "", variable, fixed=T)) %>%
  separate(variable, c("garbage", "wave"), sep="\\.") %>%
  mutate(wave=as.numeric(wave)) %>%
  mutate(wave= replace(wave, is.na(wave), 0)) %>%
  dplyr::select(-garbage) 

d_education <- as_tibble(d_all79B) %>% 
  dplyr::select(starts_with("HGC"), starts_with("ENROLLMTREV"), CASEID) %>%
  gather("variable", "value", -CASEID) %>%
  mutate(variable=gsub("HGC", "edyears", variable, fixed=T)) %>%
  mutate(variable=gsub("TREV", ".", variable, fixed=T)) %>%
  separate(variable, c("garbage", "wave"), sep="\\.") %>%
  mutate (wave = as.numeric(wave)) %>%
  mutate(wave = ifelse(garbage=="edyears", replace(wave, is.na(wave), 0), 
                       ifelse(garbage=="ENROLLM" & wave < 50, wave + 2000, 
                              ifelse(garbage=="ENROLLM" & wave > 50, wave + 1900,
                                     wave)))) %>%
  mutate(wave = ifelse(wave<1995 & wave > 30, wave - 1979, 
                       ifelse(wave>1995,wave/2 - 982, wave))) %>%
  spread(garbage, value)%>%
  mutate(enrolled = ifelse(ENROLLM == 2 | ENROLLM == 3, 1, 0)) %>%
  dplyr::select(-ENROLLM)

d_work <- as_tibble(d_all79B) %>% 
  dplyr::select(starts_with("WKSWK"),  CASEID) %>%
  gather("variable", "weeks_worked", -CASEID) %>%
  mutate(variable=gsub("K.PCY", "", variable, fixed=T)) %>%
  separate(variable, c("garbage", "wave"), sep="\\.") %>%
  mutate (wave = as.numeric(wave)) %>%
  mutate(wave = replace(wave, is.na(wave), 0)) %>%
  group_by(CASEID) %>%
  arrange(wave)%>%
  mutate(weeks_worked2 = replace(weeks_worked, is.na(weeks_worked), 0)) %>%
  mutate(total_weeks_worked = cumsum(weeks_worked2)) %>%
  mutate(lag_weeks_worked = dplyr::lag(total_weeks_worked)) %>%
  dplyr::select(-garbage, -weeks_worked2)
  

d_marriage <-  as_tibble(d_all79B) %>% 
  dplyr::select(starts_with("MARSTAT"),  CASEID) %>%
  gather("variable", "married", -CASEID) %>%
  mutate(variable=gsub("T.KEY", "", variable, fixed = T)) %>%
  separate(variable, c("garbage", "wave"), sep = "\\.") %>%
  mutate (wave = as.numeric(wave)) %>%
  mutate(wave= replace(wave, is.na(wave), 0),
         married = ifelse(married == 1, 1, 0)) %>%
  dplyr::select(-garbage)

d_union <- as_tibble(d_all79B) %>% 
  dplyr::select(starts_with("EMPLOYERS_ALL_UNION"),  CASEID) %>%
  gather("variable", "union", -CASEID) %>%
  mutate(variable=gsub("S_ALL_UNION", "", variable, fixed=T)) %>%
  separate(variable, c("garbage", "year"), sep = "\\_") %>%
  separate(year, c("year", "job"), sep = "\\.") %>%
  group_by(CASEID, year) %>%
  summarise(union = sum(union, na.rm = T)) %>%
  mutate(union = ifelse(union > 0, 1, 0)) %>%
  mutate (year = as.numeric(year)) %>%
  mutate(wave = ifelse(year < 1996, year - 1979, year/2 - 982))

d_static <- as_tibble(d_all79B) %>% 
  dplyr::select(SAMPLE_RACE, SAMPLE_SEX, AFQT.1, CASEID, starts_with("POLICE")) %>%
  mutate(black = ifelse(SAMPLE_RACE == 2, 1, 0),
         hispanic = ifelse(SAMPLE_RACE == 1, 1, 0),
         female = ifelse(SAMPLE_SEX == 2, 1, 0),
         incarcerated80 = ifelse(POLICE.7 == 1, 1, 0)) %>%
  rename(cog.ability = AFQT.1,
         charged80 = POLICE.2,
         stopped80 = POLICE.1,
         convicted80 = POLICE.3,
         probation80 = POLICE.6,
         race = SAMPLE_RACE) %>%
  mutate(incarcerated80 = replace (incarcerated80, is.na(incarcerated80) & !is.na(stopped80), 0),
         convicted80 = replace (convicted80, is.na(convicted80) & !is.na(stopped80), 0),
         probation80 = replace (probation80, is.na(probation80) & !is.na(stopped80), 0),
         charged80 = replace (charged80, is.na(charged80) & !is.na(stopped80), 0)) %>%
  dplyr::select(-SAMPLE_SEX, -starts_with("POLICE"))

d_employed <- as_tibble(d_all79B) %>% 
  dplyr::select(contains("ESR_KEY"),  CASEID) %>%
  gather("variable", "empkey", -CASEID) %>%
  separate(variable, c("garbage", "wave"), sep = "\\.") %>%
  mutate (wave = as.numeric(wave)) %>%
  mutate(wave = replace(wave, is.na(wave), 0),
         employed = ifelse(empkey == 1 | empkey == 2, 1, 0),
         unemployed = ifelse(empkey == 3, 1, 0),
         military = ifelse(empkey == 8, 1, 0),
         wave = ifelse(wave == 18, 21, wave)) %>%
  dplyr::select(-garbage, -empkey)
  
d_psector <- as_tibble(d_all79B) %>% 
  dplyr::select(contains("QES.56C"),  CASEID) %>%
  gather("variable", "psector", -CASEID) %>%
  mutate(variable=gsub("S.QES.56C", "", variable, fixed=T)) %>%
  mutate(variable=gsub("S.56C.01", "", variable, fixed=T)) %>%
  separate(variable, c("garbage", "wave"), sep="\\.") %>%
  mutate (wave = as.numeric(wave)) %>%
  mutate(wave = replace(wave, is.na(wave), 0)) %>%
  mutate(wave = ifelse(garbage == "QE", wave + 15, wave),
         psector = ifelse(garbage =="CP" & psector == 2, 1, 
                          ifelse(garbage =="QE" & psector == 1, 1, 0)))%>%
  mutate(psector = replace(psector, is.na(psector), 0)) %>%
  dplyr::select(-garbage) 

d_region <- as_tibble(d_all79B) %>% 
  dplyr::select(starts_with("REGION"),  CASEID) %>%
  gather("variable", "region", -CASEID) %>%
  separate(variable, c("garbage", "wave"), sep="\\.") %>%
  mutate (wave = as.numeric(wave)) %>%
  mutate(wave = replace(wave, is.na(wave), 0)) %>%
  mutate(region = as.factor(region)) %>%
  dplyr::select(-garbage)

d_industry <- as_tibble(d_all79B) %>% 
  dplyr::select(starts_with("INDALL.EMP"), starts_with("CPSIND70"),  CASEID) %>%
  gather("variable", "industry", -CASEID) %>%
  mutate(variable=gsub(".EMP.01", "", variable, fixed=T)) %>%
  separate(variable, c("garbage", "wave"), sep="\\.") %>%
  mutate(wave = as.numeric(wave)) %>%
  mutate(wave = replace(wave, is.na(wave), 0)) %>%
  filter(garbage == "INDALL" & wave>14 | garbage == "CPSIND70") %>%
  mutate(industry = ifelse(industry > 66 & industry < 399, 1, 
                           ifelse(industry < 58, 2,
                                  ifelse(industry > 406 & industry < 480, 3,
                                         ifelse(industry > 506 & industry < 699, 4,
                                                ifelse(industry > 706 & industry < 899, 5, 6)))))) %>%
  mutate(industry = as.factor(industry)) %>%
  dplyr::select(-garbage)

d_drugs <- as_tibble(d_all79B) %>% 
  dplyr::select(starts_with("DS"), starts_with("DRUG"),  CASEID) %>%
  gather("variable", "drugs", -CASEID) %>%
  mutate(variable=gsub("S.", "", variable, fixed=T)) %>%
  separate(variable, c("garbage", "wave"), sep="\\.") %>%
  mutate (wave = as.numeric(wave)) %>%
  mutate(wave = replace(wave, is.na(wave), 0)) %>%
  mutate(wave = ifelse((garbage == "D11" | garbage == "D15") & wave == 0,  5,
                       ifelse((garbage == "D11" | garbage == "D15") & wave == 1, 9,
                              ifelse((garbage == "D11" | garbage == "D15") & wave == 2, 13,
                                     ifelse((garbage == "D11" | garbage == "D15") & wave == 3, 15,
                                            ifelse((garbage == "D11" | garbage == "D15") & wave == 4, 19, wave)))))) %>%
  mutate(wave = ifelse(garbage == "D19" & wave == 0,  13,
                       ifelse(garbage == "D19" & wave == 1, 15,
                              ifelse(garbage == "D19" & wave == 2, 19, 5)))) %>%
  group_by(CASEID, wave) %>%
  summarise(drugs = sum(drugs, na.rm=T)) %>%
  mutate(drugs = ifelse(drugs > 0, 1, 0))

## There's CPSHRP and HRP1, Bruce uses HRP1, even though he reports using Current/Most recent,
##which would be CPSHRP

# d_wages <- as_tibble(d_all79B) %>% 
#   dplyr::select(starts_with("HRP1"), starts_with("CPSHRP"),  CASEID) %>%
#   gather("variable", "wages", -CASEID) %>%
#   separate(variable, c("garbage", "wave"), sep="\\.") %>%
#   mutate(wave = as.numeric(wave)) %>%
#   mutate(wave = replace(wave, is.na(wave), 0)) %>%
#   filter(garbage == "HRP1" & wave > 15 | garbage == "CPSHRP") %>%
#   dplyr::select(-garbage)

d_wages <- as_tibble(d_all79B) %>% 
  dplyr::select(starts_with("HRP1"),  CASEID) %>%
  gather("variable", "wages", -CASEID) %>%
  separate(variable, c("garbage", "wave"), sep="\\.") %>%
  mutate(wave = as.numeric(wave)) %>%
  mutate(wave = replace(wave, is.na(wave), 0)) %>%
  dplyr::select(-garbage)%>%
  group_by(CASEID) %>%
  arrange(wave) %>%
  mutate(wages_lead = dplyr::lead(wages))

d_welfare <- as_tibble(d_all79B) %>% 
  dplyr::select(starts_with("WELFARE"),  CASEID) %>%
  gather("variable", "welfare", -CASEID) %>%
  mutate(variable=gsub(".AMT", "", variable, fixed=T)) %>%
  separate(variable, c("garbage", "year"), sep="\\.") %>%
  separate(year, c("year", "garbage2"), sep="\\_") %>%
  mutate(welfare = replace(welfare, is.na(welfare), 0),
         welfare_indicator = ifelse(welfare < 1, 0, 1),
         welfare =  ifelse(welfare>median(welfare[welfare_indicator==1])*5, NA, welfare),
         year = as.numeric(year))%>%
  dplyr::select(-garbage, -garbage2)
 
d79B <- left_join(d_age, d_incarcerated) %>%
  left_join(d_wages) %>%
  left_join(d_drugs) %>%
  left_join(d_psector) %>%
  left_join(d_industry) %>%
  left_join(d_region) %>%
  left_join(d_union) %>%
  left_join(d_marriage) %>%
  left_join(d_work) %>%
  left_join(d_education) %>%
  left_join(d_urban) %>%
  left_join(d_income) %>%
  left_join(d_poverty) %>%
  left_join(d_welfare) %>%
  left_join(d_employed) %>%
  left_join(d_static)
  
  





