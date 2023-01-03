
library(tidyverse)
set.seed(256)
update_geom_defaults("label", list(family = "Fira Sans Condensed"))
library(gganimate)
library(broom)


## ----------------------------------------------
library(readxl)
phones<-read_excel("cellular_phone.xlsx")
View(phones)
phones<-phones %>%
  select(-state_numeric) %>%
  mutate(year_num = year) %>%
  mutate_at(c("year", "state", "cell_ban", "text_ban"), as.factor) %>%
  rename(deaths = DeathsPerBillionMiles,
         cell_plans = cell_per10thous_pop)
library(gt)

## ----------------------------------------------
phones %>%   
  filter(year=="2012") %>%
  select(state, year, deaths, cell_plans) %>%
  head() %>% gt() 


## ----------------------------------------------
phones %>%
  filter(state=="Maryland") %>%
  select(state, year, deaths, cell_plans) %>%
  head()


## ---- fig.height=4, fig.retina=3---------------
phones %>%
  filter(year=="2012") %>%
  ggplot(data = .)+
  aes(x = cell_plans,
      y = deaths)+
  geom_point(color = "red")+
  labs(x = "Cell Phones Per 10,000 People",
       y = "Deaths Per Billion Miles Driven")+
  theme_bw(base_family = "Fira Sans Condensed",
           base_size=18)


## ---- fig.height=4, fig.retina=3---------------
phones %>%
  filter(state=="Maryland") %>%
  ggplot(data = .)+
  aes(x = year_num,
      y = deaths)+
  geom_point(color="blue")+
  geom_path(color="blue", size = 1)+
  labs(x = "Year",
       y = "Deaths Per Billion Miles Driven")+
  theme_bw(base_family = "Fira Sans Condensed",
           base_size=18)


## ---- fig.retina=3, fig.width=14, fig.align="center"----
phones %>%
  filter(state!="District of Columbia") %>%
  #group_by(state) %>%
  ggplot(data = .)+
  aes(x = cell_plans,
      y = deaths,
      color = state)+
  geom_point()+
  scale_color_viridis_d()+
  geom_path(size = 1)+
  labs(x = "Cell Phones Per 10,000 People",
       y = "Deaths Per Billion Miles Driven")+
  theme_bw(base_family = "Fira Sans Condensed",
           base_size=18)+
  theme(legend.position = "none")


## ----------------------------------------------
phones %>%
  arrange(desc(deaths)) %>%
  select(state, year, deaths, cell_plans)


## ----------------------------------------------
phones %>%
  arrange(state) %>%
  select(state, year, deaths, cell_plans)


## ----------------------------------------------
phones %>%
  arrange(state) %>%
  select(state, year, deaths, cell_plans)


## ---- echo=T-----------------------------------
glimpse(phones)


## ---- echo=T-----------------------------------
phones %>%
  count(state)


## ---- echo=T-----------------------------------
phones %>%
  count(year)


## ---- echo=T-----------------------------------
phones %>%
  distinct(state)


## ---- echo=T-----------------------------------
phones %>%
  distinct(year)


## ---- echo=T-----------------------------------
phones %>%
  summarize(States = n_distinct(state),
            Years = n_distinct(year))


## ---- echo=T-----------------------------------
# install.packages("plm")
library(plm)

pdim(phones, index=c("state","year"))

library(tidyselect)
## ---- echo=T-----------------------------------
pooled <- lm(deaths ~ cell_plans, data = phones)
pooled %>% tidy()


## ----pooled-plot,echo=T, eval=F----------------
## ggplot(data = phones)+
##   aes(x = cell_plans,
##       y = deaths)+
##   geom_point()+
##   labs(x = "Cell Phones Per 10,000 People",
##        y = "Deaths Per Billion Miles Driven")+
##   theme_bw(base_family = "Fira Sans Condensed",
##            base_size=14)


## ---- ref.label="pooled-plot", fig.retina=3----



## ----pooled-plot2,echo=T, eval=F---------------
## ggplot(data = phones)+
##   aes(x = cell_plans,
##       y = deaths)+
##   geom_point()+
##   geom_smooth(method = "lm", color = "red")+ #<<
##   labs(x = "Cell Phones Per 10,000 People",
##        y = "Deaths Per Billion Miles Driven")+
##   theme_bw(base_family = "Fira Sans Condensed",
##            base_size=14)


## ---- ref.label="pooled-plot2", fig.retina=3----



## ----5state-plot1, echo=T, eval=F--------------
## phones %>%
##   filter(state %in% c("District of Columbia",
##                       "Maryland", "Texas",
##                       "California", "Kansas")) %>%
## ggplot(data = .)+
##   aes(x = cell_plans,
##       y = deaths,
##       color = state)+ #<<
##   geom_point()+ #<<
##   geom_smooth(method = "lm")+ #<<
##   labs(x = "Cell Phones Per 10,000 People",
##        y = "Deaths Per Billion Miles Driven",
##        color = NULL)+
##   theme_bw(base_family = "Fira Sans Condensed",
##            base_size=14)+
##   theme(legend.position = "top")


## ---- ref.label="5state-plot1", fig.retina=3----



## ----5state-plot2, echo=T, eval=F--------------
## phones %>%
##   filter(state %in% c("District of Columbia",
##                       "Maryland", "Texas",
##                       "California", "Kansas")) %>%
## ggplot(data = .)+
##   aes(x = cell_plans,
##       y = deaths,
##       color = state)+
##   geom_point()+
##   geom_smooth(method = "lm")+
##   labs(x = "Cell Phones Per 10,000 People",
##        y = "Deaths Per Billion Miles Driven",
##        color = NULL)+
##   theme_bw(base_family = "Fira Sans Condensed",
##            base_size=14)+
##   theme(legend.position = "none")+ #<<
##   facet_wrap(~state, ncol=3) #<<


## ---- ref.label="5state-plot2", fig.retina=3----



## ----all-state-plot, echo=T, eval=F------------
## ggplot(data = phones)+ #<<
##   aes(x = cell_plans,
##       y = deaths,
##       color = state)+
##   geom_point()+
##   geom_smooth(method = "lm")+
##   labs(x = "Cell Phones Per 10,000 People",
##        y = "Deaths Per Billion Miles Driven",
##        color = NULL)+
##   theme_bw(base_family = "Fira Sans Condensed")+
##   theme(legend.position = "none")+
##   facet_wrap(~state, ncol=7) #<<


## ---- ref.label="all-state-plot", fig.retina=3----



## ----------------------------------------------
library(ggdag)
dagify(Deaths~Phones+Cultr+Insts+Geog+Pop+Infr,
       Phones~Cultr+Insts+Geog+Pop+Infr,
       coords = list(x = c(Phones = 1, Cultr = 2, Insts = 2, Infr = 3, Geog = 4, Pop = 4, Deaths = 5),
                     y = c(Phones = 0, Cultr = 1, Insts = -1, Infr = 1, Geog = 1, Pop = -1, Deaths = 0)),
       exposure = "Phones",
       outcome = "Deaths") %>%
  tidy_dagitty(seed = 20) %>%
  ggdag_status()+theme_dag()+theme(legend.position = "none")


## ----------------------------------------------
dagify(Deaths~Phones+State,
       Phones~State,
       coords = list(x = c(Phones = 1, State = 3, Deaths = 5),
                     y = c(Phones = 0, State = 1, Deaths = 0)),
       exposure = "Phones",
       outcome = "Deaths") %>%
  tidy_dagitty(seed = 20) %>%
  ggdag_status()+theme_dag()+theme(legend.position = "none")

library(broom)
## ----fe-reg, echo=T, eval=T--------------------
fe_reg_1 <- lm(deaths ~ cell_plans + state, data = phones)
fe_reg_1 %>% tidy()


## ----demeaning, echo=T, eval=F-----------------
## # get means of Y and X by state
## means_state<-phones %>%
##   group_by(state) %>%
##   summarize(avg_deaths = mean(deaths),
##             avg_phones = mean(cell_plans))
## 
## # look at it
## means_state


## ---- ref.label="demeaning"--------------------



## ----demeaning-plot, echo=T, eval=F------------
## ggplot(data = means_state)+
##   aes(x = fct_reorder(state, avg_deaths),
##       y = avg_deaths,
##       color = state)+
##   geom_point()+
##   geom_segment(aes(y = 0,
##                    yend = avg_deaths,
##                    x = state,
##                    xend = state))+
##   coord_flip()+
##   labs(x = "Cell Phones Per 10,000 People",
##        y = "Deaths Per Billion Miles Driven",
##        color = NULL)+
##   theme_bw(base_family = "Fira Sans Condensed",
##            base_size=10)+
##   theme(legend.position = "none")


## ---- ref.label="demeaning-plot", fig.retina=3----



## ---- cache = F, fig.align="center", fig.width=15----
phones2<-phones %>%
filter(state %in% c("District of Columbia",
                      "Maryland", "Texas",
                      "California", "Kansas")) %>%
  group_by(state) %>%
  mutate(mean_phones = mean(cell_plans),
         mean_deaths = mean(deaths))

before_cor <- paste("1. Raw data: cor(cell plans, deaths) = ",round(cor(phones2$cell_plans,phones2$deaths),3),sep='')
after_cor <- paste("6. What's left: cor(cell plans, deaths) = ",round(cor(phones2$cell_plans-phones2$mean_phones,phones2$deaths-phones2$mean_deaths),3),sep='')

#Add step 2 in which X is demeaned, and 3 in which both X and Y are, and 4 which just changes label
dffull <- rbind(
  #Step 1: Raw data only
  phones2 %>% mutate(mean_phones=NA,mean_deaths=NA,time=before_cor),
  #Step 2: Add x-lines
  phones2 %>% mutate(mean_deaths=NA,time='2. Figure out any between-State differences in cell plans'),
  #Step 3: X de-meaned 
  phones2 %>% mutate(cell_plans = cell_plans - mean_phones,mean_phones=0,mean_deaths=NA,time="3. Remove all between-State differences in cell plans"),
  #Step 4: Remove X lines, add Y
  phones2 %>% mutate(cell_plans = cell_plans - mean_phones,mean_phones=NA,time="4. Figure out any between-State differences in deaths"),
  #Step 5: Y de-meaned
  phones2 %>% mutate(cell_plans = cell_plans - mean_phones,deaths = deaths - mean_deaths,mean_phones=NA,mean_deaths=0,time="5. Remove all between-State differences in deaths"),
  #Step 6: Raw demeaned data only
  phones2 %>% mutate(cell_plans = cell_plans - mean_phones,deaths = deaths - mean_deaths,mean_phones=NA,mean_deaths=NA,time=after_cor))


## ---- cache = F, fig.align="center", fig.width=15----
p <- ggplot(dffull,aes(y=deaths,x=cell_plans,color=as.factor(state)))+geom_point()+
  geom_vline(aes(xintercept=mean_phones,color=as.factor(state)))+
  geom_hline(aes(yintercept=mean_deaths,color=as.factor(state)))+
  labs(x = "Cell Phones Per 10,000 People",
       y = "Deaths Per Billion Miles Driven",
       title = 'The Relationship between Cell Plans and Deaths, with State Fixed Effects \n{next_state}',
       caption = "Animation inspired by Nick Huntington-Klein’s Causal Animations")+
  theme_bw(base_family = "Fira Sans Condensed", base_size = 12)+
  theme(legend.position = "none")+
  transition_states(time,transition_length=c(12,32,12,32,12,12),state_length=c(160,100,75,100,75,160),wrap=FALSE)+
  ease_aes('sine-in-out')+
  exit_fade()+enter_fade()
library(gganimate)
animate(p,nframes=200)


## ---- cache = F, fig.align="center", fig.width=15----
phones3<-phones %>%
  group_by(state) %>%
  mutate(mean_phones = mean(cell_plans),
         mean_deaths = mean(deaths))

before_cor <- paste("1. Raw data: cor(cell plans, deaths) = ",round(cor(phones3$cell_plans,phones3$deaths),3),sep='')
after_cor <- paste("6. What's left: cor(cell plans, deaths) = ",round(cor(phones3$cell_plans-phones3$mean_phones,phones3$deaths-phones3$mean_deaths),3),sep='')

#Add step 2 in which X is demeaned, and 3 in which both X and Y are, and 4 which just changes label
dffull2 <- rbind(
  #Step 1: Raw data only
  phones3 %>% mutate(mean_phones=NA,mean_deaths=NA,time=before_cor),
  #Step 2: Add x-lines
  phones3 %>% mutate(mean_deaths=NA,time='2. Figure out any between-State differences in cell plans'),
  #Step 3: X de-meaned 
  phones3 %>% mutate(cell_plans = cell_plans - mean_phones,mean_phones=0,mean_deaths=NA,time="3. Remove all between-State differences in cell plans"),
  #Step 4: Remove X lines, add Y
  phones3 %>% mutate(cell_plans = cell_plans - mean_phones,mean_phones=NA,time="4. Figure out any between-State differences in deaths"),
  #Step 5: Y de-meaned
  phones3 %>% mutate(cell_plans = cell_plans - mean_phones,deaths = deaths - mean_deaths,mean_phones=NA,mean_deaths=0,time="5. Remove all between-State differences in deaths"),
  #Step 6: Raw demeaned data only
  phones3 %>% mutate(cell_plans = cell_plans - mean_phones,deaths = deaths - mean_deaths,mean_phones=NA,mean_deaths=NA,time=after_cor))


## ---- cache = F, fig.align="center", fig.width=15----
p2 <- ggplot(dffull2,aes(y=deaths,x=cell_plans,color=as.factor(state)))+geom_point()+
  geom_vline(aes(xintercept=mean_phones,color=as.factor(state)))+
  geom_hline(aes(yintercept=mean_deaths,color=as.factor(state)))+
  labs(x = "Cell Phones Per 10,000 People",
       y = "Deaths Per Billion Miles Driven",
       title = 'The Relationship between Cell Plans and Deaths, with State Fixed Effects \n{next_state}',
       caption = "Animation inspired by Nick Huntington-Klein’s Causal Animations")+
  theme_bw(base_family = "Fira Sans Condensed", base_size = 12)+
  theme(legend.position = "none")+
  transition_states(time,transition_length=c(12,32,12,32,12,12),state_length=c(160,100,75,100,75,160),wrap=FALSE)+
  ease_aes('sine-in-out')+
  exit_fade()+enter_fade()

animate(p2,nframes=200)


## ----plm-fe, echo=T----------------------------
#install.packages("plm")
library(plm)
fe_reg_1_alt<-plm(deaths ~ cell_plans,
                  data = phones,
                  index = "state",
                  model = "within")


## ---- echo=T-----------------------------------
fe_reg_1_alt %>% tidy()


## ----------------------------------------------
library(ggdag)
dagify(Deaths~Phones+State+Macro+FedLaw,
       Phones~State+Macro+FedLaw,
       coords = list(x = c(Phones = 1, Macro = 2, State = 3, FedLaw = 4, Deaths = 5),
                     y = c(Phones = 0, Macro = 1, State = -1, FedLaw = 1, Geog = 1, Pop = -1, Deaths = 0)),
       exposure = "Phones",
       outcome = "Deaths") %>%
  tidy_dagitty(seed = 20) %>%
  ggdag_status()+theme_dag()+theme(legend.position = "none")


## ----------------------------------------------
dagify(Deaths~Phones+State+Year,
       Phones~State+Year,
       coords = list(x = c(Phones = 1, State = 3, Year = 3, Deaths = 5),
                     y = c(Phones = 0, State = -1, Year = 1, Deaths = 0)),
       exposure = "Phones",
       outcome = "Deaths") %>%
  tidy_dagitty(seed = 20) %>%
  ggdag_status()+theme_dag()+theme(legend.position = "none")


## ---- echo=T-----------------------------------
# find averages for years
means_year<-phones %>%
  group_by(year) %>%
  summarize(avg_deaths = mean(deaths),
            avg_phones = mean(cell_plans))
means_year


## ----years-plot,echo=T, eval=F-----------------
## ggplot(data = phones)+
##   aes(x = year,
##       y = deaths)+
##   geom_point(aes(color = year))+
## 
##   # Add the yearly means as black points
##   geom_point(data = means_year,
##              aes(x = year,
##                  y = avg_deaths),
##              size = 3,
##              color = "black")+
## 
##   geom_path(data = means_year,
##             aes(x = year,
##                 y = avg_deaths),
##             size = 1)+
##   theme_bw(base_family = "Fira Sans Condensed",
##            base_size = 14)+
##   theme(legend.position = "none")


## ---- ref.label="years-plot", fig.retina=3-----



## ----fe2-reg, echo=T, eval=T-------------------
fe2_reg_1 <- lm(deaths ~ cell_plans + state + year,
                data = phones)
fe2_reg_1 %>% tidy()


## ----fe2-reg2, echo=T, eval=T------------------
fe2_reg_2 <- plm(deaths ~ cell_plans,
                 index = c("state", "year"),
                 model = "within",
                 data = phones)
fe2_reg_2 %>% tidy()


## ----------------------------------------------
dagify(Deaths~Phones+State+Year+Bans+Urban,
       Phones~State+Year+Bans+Urban,
       coords = list(x = c(Phones = 1, Bans = 2, State = 2, Year = 4, Urban = 4, Deaths = 5),
                     y = c(Phones = 0, Bans = 1, State = -1, Year = -1, Urban = 1, Deaths = 0)),
       exposure = "Phones",
       outcome = "Deaths") %>%
  tidy_dagitty(seed = 20) %>%
  ggdag_status()+theme_dag()+theme(legend.position = "none")


## ---- echo=T-----------------------------------
fe2_controls_reg <- plm(deaths ~ cell_plans + text_ban + urban_percent + cell_ban,
                        data = phones,
                        index = c("state","year"),
                        model = "within",
                        effect = "twoways") 

fe2_controls_reg %>% tidy()


## ----huxout, echo=T, eval =F-------------------
 library(huxtable)
 huxreg("Pooled" = pooled,
        "State Effects" = fe_reg_1,
        "State & Year Effects" = fe2_reg_1,
        "With Controls" = fe2_controls_reg,
        coefs = c("Intercept" = "(Intercept)",
                  "Cell phones" = "cell_plans",
                  "Cell Ban" = "cell_ban1",
                  "Texting Ban" = "text_ban1",
                "Urbanization Rate" = "urban_percent"),
        statistics = c("N" = "nobs",
                       "R-Squared" = "r.squared",
                       "SER" = "sigma"),
        number_format = 4)


## ---- ref.label="huxout"-----------------------
huxout

