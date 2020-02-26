setwd("~/Desktop/Sociology/Research/punitive regimes/nlsy79/")
options(mc.cores = parallel::detectCores())

library(arm)
library(tidyverse)
library(rstanarm)
library(plm)
library(lme4)
library(imputeTS)
library(lfe)

source("nlsy79-cleaning.R")

data.asr.bruce <- d79B %>%
  filter(female == 0 & military == 0 & wave > 3 & wave < 18 & wages > 0 & wages < 5*median(wages, na.rm = T)) %>% 
  mutate(log_age = log(age), 
         education10 = edyears * 10,
         log_weeks = log(lag_weeks_worked + 1),
         cog.ability=cog.ability/100) %>%
  group_by(CASEID)%>%
  fill(drugs) %>%
  mutate(drugs = ifelse(wave == 4 & is.na(drugs), drugs[wave==5], drugs),
         wages_defleated = case_when(wave == 4 ~ wages/0.978,
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
                                     TRUE ~ NA_real_),
         log_wages = log(wages_defleated/100)) %>%
  ungroup() 

### Bruce models from his STATA code ####

# reg lhw 
# res (in prison?) pinc (ever_inc?) mar (married) lage (log age) un (local unemployment) hgc (highest grade) 
# llwk (log(lagged week + 1)) enr (enrolled) west (region) sth (region) mw (region) urb drug coll(union)
# afqt (cog.ability) stop (Stopped) charge (Charged) conv (convicted) sent prob psect (public sector worker)
# seca secb secc secd sece if flag1~=1 & flag2~=1;
# 
# reg lhw 
# res pinc mar lage un hgc llwk enr yr yhgc west sth mw urb drug coll 
# psect seca secb secc secd sece if flag1~=1 & flag2~=1 [iw=weight79];
# 
# xtreg lhw 
# res pinc mar lage un hgc llwk enr west sth mw urb drug coll 
# psect seca secb secc secd sece if flag1~=1 & flag2~=1, fe i(id);

#### My models ####

## model in his stata code
western1 <- lm(log_wages ~ incarcerated + ever_inc + married + log_age + edyears + lag_weeks_worked +
                  as.factor(region) + enrolled + urban + drugs + union + psector,
                data = data.asr.bruce)

display(western1)

## model reported
western2 <- lm(log_wages ~ incarcerated + ever_inc + married + log_age + edyears + lag_weeks_worked +
                 as.factor(region) + enrolled + urban + drugs + union + cog.ability + industry + 
               charged80 + incarcerated80  + psector + black + hispanic,
               data = data.asr.bruce)

display(western2)


## Fixed effects model

western_fe<- plm(log_wages ~ incarcerated + ever_inc + married + log_age + edyears + lag_weeks_worked +
                    as.factor(region) + enrolled + urban + drugs + union  + industry +
                    psector + year, data = data.asr.bruce, index ="CASEID", model="within", 
                 effect="individual")

western_fe<- felm(log_wages ~ incarcerated + ever_inc + married + log_age  
                  + lag_weeks_worked +
                   as.factor(region) + enrolled + urban + drugs + union + industry + 
                   psector + edyears + year | CASEID, data = data.asr.bruce)
                          
display(western_fe)
summary(western_fe, digits = 2)
predicted_values <-  western_M %*% western_fe$coefficients
residuals_ <- residuals(western_fe)
fitted_ <- fitted(western_fe)

par(mfrow=c(1,1))

plot(fitted_, residuals_,cex=.5, pch=20, xlab="predicted values of y",  ylab="residuals", 
      col=rgb(0,0,0,0.6))

plot(fitted_+residuals_, residuals_,cex=.5, pch=20, xlab="observed values of y",  ylab="residuals",
     col=rgb(0,0,0,0.6))

western_est <- tibble(att = western_fe$coefficients[2], sd = western_fe$se[2])
  mutate(ci.upper = att + 1.96*sd,
         ci.lower = att - 1.96*sd)
  
naive_est <- tibble(att = coef(naive_lm)[2], sd = se.coef(naive_lm)[2])
  mutate(ci.upper = att + 1.96*sd,
         ci.lower = att - 1.96*sd)

results <- estimates %>% bind_rows(western_est) %>% bind_rows(naive_est) %>% 
  mutate(ci.upper = att + 1.96*sd,
         ci.lower = att - 1.96*sd) %>%
  add_column(model = c("BART", "FE", "Naive LM")) 

results %>%
  ggplot(aes(x = model, y=att, ymin=ci.lower, ymax=ci.upper)) +
  geom_pointrange() + theme_bw() + coord_flip() + 
  labs(title = "Effect of incarceration on log wages") 




