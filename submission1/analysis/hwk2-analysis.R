# Preliminaries -----------------------------------------------------------
if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, ggplot2, dplyr, lubridate)

#Call/Load Datasets
hcris.data.v1996 <- readRDS("data/output/HCRIS_Data_v1996.rds")
hcris.data.v2010 <- readRDS("data/output/HCRIS_Data_v2010.rds")
full.hcris.data <- readRDS("data/output/HCRIS_Data.rds")

#Objects
fig.hospital.reports <- hcris.data.v2010 %>% 
    group_by(provider_number, year) %>%
    select(provider_number, year, report) %>%
    ggplot(aes(x=as.factor(year),y=report)) + 
  labs(
    x="Year",
    y="Reports",
    title="Count of Hospitals Filing more than one Report per Year"
  ) + scale_y_continuous(labels=comma) +
  theme_bw()

plot(fig.hospital.reports)

#Get Count of Hospitals
unique.count <-n_distinct(full.hcris.data$provider_number)
print(unique.count)

#Violin Plot Total Charges/Year
fig.total.charges <- full.hcris.data %>% 
  ggplot(aes(x=as.factor(year), y = tot_charges)) +
  geom_jitter(alpha = .05) +
  geom_violin(alpha = .9, draw_quantiles = c(0.5))

plot(fig.total.charges)  

#Get Distribution of Estimated Prices

hcris.data <- full.hcris.data %>%
  mutate( discount_factor = 1-tot_discounts/tot_charges,
          price_num = (ip_charges + icu_charges + ancillary_charges)*discount_factor - tot_mcare_payment,
          price_denom = tot_discharges - mcare_discharges,
          price = price_num/price_denom)

#Violin Plot of Prices
fig.prices <- hcris.data %>% 
  ggplot(aes(x=as.factor(year), y = price)) +
  geom_jitter(alpha = .05) +
  geom_violin(alpha = .9, draw_quantiles = c(0.5))

plot(fig.prices) 

#Filter by Year 2012

final.hcris <- hcris.data %>% ungroup() %>%
  filter(price_denom>100, !is.na(price_denom), 
         price_num>0, !is.na(price_num),
         price<100000, 
         beds>30, year==2012) %>%  #<<
  mutate( hvbp_payment = ifelse(is.na(hvbp_payment),0,hvbp_payment),
          hrrp_payment = ifelse(is.na(hrrp_payment),0,abs(hrrp_payment)), #<<
    penalty = (hvbp_payment-hrrp_payment<0)) #<<


#Check Mean Price
final.hcris %>% group_by(penalty) %>%
summarise(mean.price = mean(price))

#Create Quartiles for Bed Size
hcris.beds <- final.hcris %>%
mutate(quartile = ntile(beds, 4))

#Table for Beds
table(hcris.beds$price,hcris.beds$beds)

#Inverse Matching
m.nn.var <- Matching::Match(Y=lp.vars$price,
                            Tr=lp.vars$penalty,
                            X=lp.covs,
                            M=4,  #<<
                            Weight=1,
                            estimand="ATE")

v.name=data.frame(new=c("Beds","Medicaid Discharges", "Inaptient Charges",
                   "Medicare Discharges", "Medicare Payments"))

#Mahalanobis
m.nn.md <- Matching::Match(Y=lp.vars$price,
                           Tr=lp.vars$penalty,
                           X=lp.covs,
                           M=1,
                           Weight=2,
                           estimand="ATE")  

#Propensity Score
logit.model <- glm(penalty ~ beds + mcaid_discharges + ip_charges + mcare_discharges +
            tot_mcare_payment, family=binomial, data=lp.vars)
ps <- fitted(logit.model)
m.nn.ps <- Matching::Match(Y=lp.vars$price,
                           Tr=lp.vars$penalty,
                           X=ps,
                           M=1,
                           estimand="ATE")

summary(final.hcris.data$price)
plot(density(hcris.data$price, na.rm=TRUE))