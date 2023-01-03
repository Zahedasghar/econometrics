library(tidyverse)
library(haven) ## To load data from SPSS/STATA
hmda <- read_stata("hmda_sw.dta")

hmda_shortv<- hmda %>% select(s7,s13,s46)
glimpse(hmda_short)
library(vtable)
glimpse(hmda)
vtable(hmda)
# create the variables of interest
View(hmda)
hmda<-hmda %>% mutate(deny=ifelse(s7==3,1,0),pi_ratio=s46/100, 
                      black=ifelse(s13==3,1,0))

#hmda$deny = ifelse(hmda$s7==3, 1, 0)
#hmda$pi_ratio = hmda$s46/100
#hmda$black = ifelse(hmda$s13==3, 1, 0)
library(ggplot2)
figure11_1 = ggplot(data=hmda, aes(x=pi_ratio, y=deny))
figure11_1 = figure11_1 +
  geom_point(alpha=0.2, size=1) +
  geom_smooth(method="lm", se=F) +
  ylim(-0.4,1.4) + scale_x_continuous(breaks = seq(0,3,0.5)) +
  labs(x="P/I Ratio", y="Deny") +
  annotate("text",x=2.5,y=0.9,label="Mortgage denied") +
  annotate("text",x=2.5,y=-0.1,label="Mortgage approved") +
  theme_bw()
figure11_1



#### Trimming the data
# sample of the HMDA data with 150 observations
library(dplyr)
hmda_s1 = dplyr::sample_n(filter(hmda,s13==3 & s7==3 & s46/100<1), 35)
hmda_s2 = dplyr::sample_n(filter(hmda,s13==3 & s46/100<1), 15)
hmda_s3 = dplyr::sample_n(filter(hmda,s13==5 & s46/100<1), 50)
hmda_s = rbind(hmda_s1, hmda_s2, hmda_s3)
# create the variables of interest
hmda_s$deny = ifelse(hmda_s$s7==3, 1, 0)
hmda_s$pi_ratio = hmda_s$s46/100
hmda_s$black = ifelse(hmda_s$s13==3, 1, 0)
figure11_1mod = ggplot(data=hmda_s, aes(x=pi_ratio, y=deny))
figure11_1mod = figure11_1mod +
  geom_point(size=2, shape=1) +
  geom_smooth(method="lm", se=F) +
  ylim(-0.4,1.4) + scale_x_continuous(breaks = seq(0,1,0.1)) +
  labs(x="P/I Ratio", y="Deny") +
  annotate("text",x=0.6,y=1.1,label="Mortgage denied") +
  annotate("text",x=0.6,y=-0.1,label="Mortgage approved") +
  annotate("label",x=0.3,y=0.5,label="LPM") +
  theme_bw()
figure11_1mod
### LPM on the trimmed dataset


library(lmtest)
library(sandwich)
LPM = lm(deny ~ pi_ratio, data=hmda)
coeftest(LPM, vcov = sandwich)
##
## t test of coefficients:
##
## Estimate Std. Error t value Pr(>|t|)
## (Intercept) -0.079910 0.031953 -2.5008 0.01246 *
## pi_ratio 0.603535 0.098441 6.1309 1.02e-09 ***
## ---
## Signif. codes: 0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1


LPM2 = lm(deny ~ pi_ratio + black, data=hmda)
coeftest(LPM2, vcov=sandwich)


## Probit Chart Code

figure11_3mod = ggplot(data=hmda_s, aes(x=pi_ratio, y=deny))
figure11_3probit = figure11_3mod +
  geom_point(shape=1, size=2) +
  geom_smooth(method="glm",
              
              method.args=list(family=binomial(link="probit")),
              se=F) +
  ylim(-0.4,1.4) +
  scale_x_continuous(breaks = seq(0,1,0.1)) +
  labs(x="P/I Ratio", y="Deny") +
  annotate("label",x=0.3,y=0.5,label="Probit Regression") +
  theme_bw()
figure11_3probit

## Probit Regression
probit = glm(
  deny ~ black + pi_ratio,
  data=hmda,
  family=binomial(link="logit"))
coeftest(probit, vcov=sandwich)



## Logit Chart

figure11_3mod = ggplot(data=hmda_s, aes(x=pi_ratio, y=deny))
figure11_3logit = figure11_3mod +
  geom_point(shape=1, size=2) +
  geom_smooth(method="glm",
              
              method.args=list(family=binomial(link="probit")),
              se=F) +
  geom_smooth(method="glm",
              
              method.args=list(family=binomial(link="logit")),
              se=F,
              color="red") +
  ylim(-0.4,1.4) +
  scale_x_continuous(breaks = seq(0,1,0.1)) +
  labs(x="P/I Ratio", y="Deny") +
  annotate("label",x=0.3,y=0.5,label="Logit Regression \n(in red)") +
  theme_bw()
figure11_3logit


## Some additional variables for the regressions

# rename and create some variables for regression
hmda$hse_inc = hmda$s45/100
hmda$loan_val = hmda$s6/hmda$s50
hmda$ccred = hmda$s43
hmda$mcred = hmda$s42
hmda$pubrec = ifelse(hmda$s44>0,1,0)
hmda$denpmi = ifelse(hmda$s53==1,1,0)
hmda$selfemp = ifelse(hmda$s27a==1,1,0)
hmda$married = ifelse(hmda$s23a=="M",1,0)
hmda$single = ifelse(hmda$married==0,1,0)
hmda$hischl = ifelse(hmda$school>=12,1,0)
hmda$probunmp = hmda$uria
hmda$condo = ifelse(hmda$s51 == 1,1,0)



## Table 11.2: LPM, Logit, and Probit regressions
# LPM
model_1 = lm(
  deny ~ black + pi_ratio + hse_inc + loan_val + ccred +
    mcred + pubrec + + denpmi + selfemp,
  data=hmda)
# Logit
model_2 = glm(
  deny ~ black + pi_ratio + hse_inc + loan_val + ccred +
    mcred + pubrec + + denpmi + selfemp,
  data=hmda,
  family=binomial(link="logit"))
# Probit
model_3 = glm(
  deny ~ black + pi_ratio + hse_inc + loan_val + ccred +
    mcred + pubrec + + denpmi + selfemp,
  data=hmda,
  family=binomial(link="probit"))
library(modelsummary)
modelsummary(list(model_1,model_2,model_2))
