setwd("~/Desktop/Sociology/Research/punitive regimes/nlsy79/")
options(mc.cores = parallel::detectCores())

library(arm)
library(tidyverse)
library(rstanarm)
library(plm)
library(lme4)
library(imputeTS)
library(bartCause)
library(gridExtra)
library(stargazer)
library(tables)
library(treatSens)

source("nlsy79-cleaning.R")

`%notin%` <- Negate(`%in%`)

## data set up following Western (2001) ####

data.asr <- d79B %>%
  filter(female == 0  & wave > 3 & wave < 23) %>% 
  mutate(wages = replace(wages, is.na(wages), -10),
         race = as.factor(race),
         income_currentY = replace(income_currentY, is.na(income_currentY), -10),
         income_past_year = replace(income_past_year, is.na(income_past_year), -10),
         ever_inc = ifelse(wave == 1 & incarcerated80 == 1 & age > 17, 1, ever_inc),
         log_age = log(age), 
         log_weeks = log(lag_weeks_worked + 1),
         cog.ability=cog.ability/100,
         wages_deflated = case_when(wave == 4 ~ wages/0.978,
                                     wave == 5 ~ wages/1.019,
                                     wave == 6 ~ wages/1.055,
                                     wave == 7 ~ wages/1.096,
                                     wave == 8 ~ wages/1.112,
                                     wave == 9 ~ wages/1.157,
                                     wave == 10 ~ wages/1.211,
                                     wave == 11 ~ wages/1.274,
                                     wave == 12 ~ wages/1.346,
                                     wave == 13 ~ wages/1.381,
                                     wave == 14 ~ wages/1.426,
                                     wave == 15 ~ wages/1.462,
                                     wave == 16 ~ wages/1.544,
                                     wave == 17 ~ wages/1.616,
                                     wave == 18 ~ wages/1.688,
                                     wave == 19 ~ wages/1.771,
                                     wave == 20 ~ wages/1.852,
                                     TRUE ~ wages),
         wages = ifelse(wages<0, NA, wages),
         wages_deflated =ifelse(wages_deflated <0, NA, wages_deflated),
         income_currentY_d = case_when(wave == 4 ~ income_currentY/0.978,
                                      wave == 5 ~ income_currentY/1.019,
                                      wave == 6 ~ income_currentY/1.055,
                                      wave == 7 ~ income_currentY/1.096,
                                      wave == 8 ~ income_currentY/1.112,
                                      wave == 9 ~ income_currentY/1.157,
                                      wave == 10 ~ income_currentY/1.211,
                                      wave == 11 ~ income_currentY/1.274,
                                      wave == 12 ~ income_currentY/1.346,
                                      wave == 13 ~ income_currentY/1.381,
                                      wave == 14 ~ income_currentY/1.426,
                                      wave == 15 ~ income_currentY/1.462,
                                      wave == 16 ~ income_currentY/1.544,
                                      wave == 17 ~ income_currentY/1.616,
                                      wave == 18 ~ income_currentY/1.688,
                                      wave == 19 ~ income_currentY/1.771,
                                      wave == 20 ~ income_currentY/1.852,
                                      TRUE ~ NA_real_),
         income_currentY_d = ifelse(income_currentY_d < 0, NA, income_currentY_d),
         income_currentY = ifelse(income_currentY < 0, NA, income_currentY),
         income_past_year_d = case_when(wave == 4 ~ income_past_year/0.978,
                                        wave == 5 ~ income_past_year/1.019,
                                        wave == 6 ~ income_past_year/1.055,
                                        wave == 7 ~ income_past_year/1.096,
                                        wave == 8 ~ income_past_year/1.112,
                                        wave == 9 ~ income_past_year/1.157,
                                        wave == 10 ~ income_past_year/1.211,
                                        wave == 11 ~ income_past_year/1.274,
                                        wave == 12 ~ income_past_year/1.346,
                                        wave == 13 ~ income_past_year/1.381,
                                        wave == 14 ~ income_past_year/1.426,
                                        wave == 15 ~ income_past_year/1.462,
                                        wave == 16 ~ income_past_year/1.544,
                                        wave == 17 ~ income_past_year/1.616,
                                        wave == 18 ~ income_past_year/1.688,
                                        wave == 19 ~ income_past_year/1.771,
                                        wave == 20 ~ income_past_year/1.852,
                                        TRUE ~ NA_real_),
         income_past_year_d = ifelse(income_past_year_d < 0, NA, income_past_year_d),
         income_past_year = ifelse(income_past_year < 0, NA, income_past_year),
         log_wages = log(wages_deflated + 1),
         log_income_currentY = log(income_currentY_d + 1),
         log_income_pastY = log(income_past_year_d + 1),
         wages_NAs = replace(wages_deflated, is.na(wages_deflated), 0),
         income_currentY_NAs = replace(income_currentY_d, is.na(income_currentY_d), 0),
         income_past_year_NAs = replace(income_past_year_d, is.na(income_past_year_d), 0),
         industry_NAs = ifelse(is.na(industry), 0, industry),
         pov.status = replace(pov.status, is.na(pov.status), 0),
         urban_NAs = replace(urban, is.na(urban), 2)) %>%
  group_by(CASEID) %>%
  fill(drugs) %>%
  fill(psector) %>%
  mutate(drugs = ifelse(wave == 4 & is.na(drugs), drugs[wave==5], drugs)) %>%
  ungroup()

## Naive LM with original dataset (ignores correlated error) ####

naive_lm <- lm(log_wages ~ ever_inc + incarcerated + union + psector + region + 
                  married + edyears + enrolled + urban +
                  drugs + age + cog.ability + stopped80 + charged80 + probation80 + 
                  convicted80 + total_weeks_worked + welfare_indicator +
                  employed + unemployed + military +
                  weeks_worked + black + hispanic + pov.status + 
                  income_currentY + income_past_year + year, 
                data = data.asr)
display(naive_lm)


### Constructed sample ####

d_wgs<- data.asr %>% dplyr::select(wave, log_wages, CASEID)

create_data <- function(baseline_wave, d){
  out_baseline <- d %>% filter(((wave == baseline_wave) & (incarcerated == 1 | ever_inc == 1)) |
                                 (wave == baseline_wave+3 & incarcerated == 1)) %>% distinct(CASEID)
  
  d2 <- d %>% filter(CASEID %notin% out_baseline$CASEID)
  
  d3 <- d2 %>% filter(wave == baseline_wave) %>% 
    dplyr::select(CASEID, wave, industry_NAs, union, wages_NAs, psector, region, 
                  married, edyears, enrolled, urban_NAs,
                  drugs, age, cog.ability, stopped80, charged80, probation80, 
                  convicted80, total_weeks_worked, welfare_indicator,
                  employed, unemployed, military,
                  weeks_worked, black, hispanic, pov.status, 
                  income_currentY_NAs, income_past_year_NAs) %>% 
    rename(wages_t0 = wages_NAs) %>%
    left_join (d_incarcerated [d_incarcerated$wave==baseline_wave+1,], by="CASEID") %>% 
    rename (incarcerated_t1 = incarcerated,
            ever_inc_t1 = ever_inc) %>%
    left_join (d_incarcerated [d_incarcerated$wave==baseline_wave+2,], by="CASEID") %>% 
    rename (incarcerated_t2 = incarcerated,
            ever_inc_t2 = ever_inc) %>%
    left_join (d_incarcerated [d_incarcerated$wave==baseline_wave+3,], by="CASEID") %>% 
    rename (incarcerated_t3 = incarcerated,
            ever_inc_t3 = ever_inc) %>%
    left_join(d_wgs [d_wgs$wave==baseline_wave+3,], by="CASEID") %>%
    mutate(
           treatment = pmax(ever_inc_t1, ever_inc_t2, ever_inc_t3, na.rm = T)) %>% 
    drop_na() %>%
    dplyr::select(-starts_with("wave"), -contains("incar"), -contains("ever_inc"))
  return(d3)
}

model_dataframe <- tibble(baselines = c(4,8,12,16)) %>% 
  mutate(model_dfs = map(baselines, create_data, data.asr),
         model_dfs = map2(model_dfs, baselines, add_column),
         model_dfs = map(model_dfs, rename, bl = 31))

all_dfs <- bind_rows(model_dataframe$model_dfs[[1]], 
                     model_dataframe$model_dfs[[2]],
                     model_dataframe$model_dfs[[3]],
                     model_dataframe$model_dfs[[4]])

### Naive LM with constructed sample (ignores correlated error) + random effects model ####

naive_lm2 <- lm(log_wages ~ treatment + union + psector + region + 
                   married + edyears+ enrolled+ urban_NAs +
                   drugs + age + cog.ability + stopped80 + charged80 + probation80 + 
                   convicted80 + total_weeks_worked + welfare_indicator +
                   employed + unemployed + military +
                   weeks_worked+ black + hispanic+ pov.status + 
                   income_currentY_NAs + income_past_year_NAs + wages_t0, data = all_dfs)

display(naive_lm2)

naive_fit3 <- lmer(log_wages ~ treatment + + union + psector + region + 
                     married + edyears+ enrolled+ urban_NAs +
                     drugs + age + cog.ability + stopped80 + charged80 + probation80 + 
                     convicted80 + total_weeks_worked + welfare_indicator +
                     employed + unemployed + military +
                     weeks_worked+ black + hispanic+ pov.status + 
                     income_currentY_NAs + income_past_year_NAs + wages_t0 +
                     (1|CASEID), data=all_dfs)

display(naive_fit3)


## BART model with random effects for correlated person error####

confs <- all_dfs %>% dplyr::select(union, psector, region, 
                                   married, edyears, enrolled, urban_NAs,
                                   drugs, age, cog.ability, stopped80, charged80, probation80, 
                                   convicted80, total_weeks_worked, welfare_indicator,
                                   employed, unemployed, military,
                                   weeks_worked, black, hispanic, pov.status, 
                                   income_currentY_NAs, income_past_year_NAs, wages_t0, bl)

fit1 <- bartc(all_dfs$log_wages, all_dfs$treatment, 
              confs, estimand = "ate", method.trt = "bart",
              group.by=all_dfs$CASEID, use.rbart=TRUE, commonSup.rule = "sd")

posterior_ind <- extract(fit1, "indiv.diff")
trtd <- fit1$trt
posterior_trtd <- posterior_ind[trtd==1,]
posterior_trtd_means <-apply(posterior_trtd, 2, mean)

estimates <- tibble(att = mean(posterior_trtd_means), sd = sd(posterior_trtd_means))

## Sensitivity Analysis ####
fit_Sens <- treatSens.BART(log_wages ~ treatment + union + psector + region+ married + edyears + enrolled
                      + drugs + age + cog.ability + stopped80 + charged80 + probation80 + 
                        convicted80 + urban_NAs +
                        total_weeks_worked + welfare_indicator +
                        employed + unemployed + military + 
                        weeks_worked + black + hispanic + pov.status +
                        income_currentY_NAs + income_past_year_NAs + wages_t0, data=as.data.frame(all_dfs))

sensPlot(fit_Sens)

fit2 <- treatSens.BART(log_wages ~ treatment + union + psector + region+ married + edyears + enrolled
                      + drugs + age + cog.ability + stopped80 + charged80 + probation80 + 
                        convicted80 + 
                        total_weeks_worked + welfare_indicator +
                        employed + unemployed + military + 
                        weeks_worked + black + hispanic + pov.status +
                        income_currentY_NAs + income_past_year_NAs + wages_t0 + urban_NAs, data=as.data.frame(model_dataframe$model_dfs[[1]]))

sensPlot(fit2)
  
## Diagnostic plots ####

plot_support(fit1, xvar = "indiv.diff", yvar = "y1")
plot_sigma(fit1)
plot_est(fit1)
plot_indiv(fit1)
plot_est(fit1, main = paste("Traceplot", fit1$estimand),
         xlab = "iteration", ylab = fit1$estimand,
         lty = 1:fit1$n.chains, col = NULL)

plot(ests$sigma[1,], ylab="Sigma", xlab="Draws", cex=.5, pch=20, col=rgb(0,0,0,0.6))

