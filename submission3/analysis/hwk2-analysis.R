# Preliminaries -----------------------------------------------------------
if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, ggplot2, dplyr, lubridate, scales)

#Call/Load Datasets
final.hcris.v1996 <- readRDS("data/output/HCRIS_Data_v1996.rds")
final.hcris.v2010 <- readRDS("data/output/HCRIS_Data_v2010.rds")
full.hcris.data <- readRDS("data/output/HCRIS_Data.rds")


final.hcris.v1996 = final.hcris.v1996 %>%
  mutate(hvbp_payment=NA, hrrp_payment=NA)

#Combine Datasets
final.hcris=rbind(final.hcris.v1996,final.hcris.v2010) %>%
  mutate(fy_end=mdy(fy_end),fy_start=mdy(fy_start),
         date_processed=mdy(date_processed),date_created=mdy(date_created),
         tot_discounts=abs(tot_discounts), hrrp_payment=abs(hrrp_payment)) %>%
  mutate(fyear=year(fy_end)) %>%
  arrange(provider_number,fyear) %>%
  select(-year)

final.hcris %>% group_by(fyear) %>% count()

#Create Total Reports
final.hcris =
  final.hcris %>% 
  add_count(provider_number, fyear, name="total_reports")

#Identify Hospitals with Multiple Reports per Fiscal Year
duplicate.hcris = 
  final.hcris %>%
  filter(total_reports>1) %>%
  mutate(time_diff=fy_end-fy_start)

#Create Graph

hospital.reports <- duplicate.hcris %>%
  group_by(fyear) %>%
  count()

fig.hospital.reports <- ggplot(hospital.reports, aes(x = fyear, y = n))+
  geom_line() +
  geom_point() +
  theme_bw() + 
  labs(x= "Year",
       y="Count of Hospitals",
       title="Count of Hospitals with more than One Report"
       ) +
  theme(axis.text.x = element_text(angle = 90, hjust=1))


#Get Count of Hospitals
unique.count <-n_distinct(full.hcris.data$provider_number)
print(unique.count)


#Scale Total Charges Data
total.charges <- full.hcris.data %>%
mutate(tot_charges_low=quantile(tot_charges, prob=0.01, na.rm=TRUE))


#Violin Plot Total Charges/Year
fig.total.charges <- total.charges %>% 
  ggplot(aes(x=as.factor(year), y = tot_charges)) +
  geom_jitter(alpha = .05) +
  geom_violin(alpha = .9, draw_quantiles = c(0.5)) + 
  labs(x= "Year",
       y="Total Charges",
       title="Total Charges per Year"
  ) +
  theme(axis.text.x = element_text(angle = 90, hjust=1))

plot(fig.total.charges)

#Get Distribution of Estimated Prices

hcris.data <- full.hcris.data %>%
  mutate( discount_factor = 1-tot_discounts/tot_charges,
          price_num = (ip_charges + icu_charges + ancillary_charges)*discount_factor - tot_mcare_payment,
          price_denom = tot_discharges - mcare_discharges,
          price = price_num/price_denom)

hcris.price <- hcris.data %>% filter(price_denom>100, !is.na(price_denom), 
         price_num>0, !is.na(price_num),
         price<100000)

hcris.price <- hcris.price %>%
filter(beds < 250)

#Violin Plot of Prices
fig.prices <- hcris.price %>% 
  ggplot(aes(x=as.factor(year), y = price)) +
  geom_jitter(alpha = .05) +
  geom_violin(alpha = .9, draw_quantiles = c(0.5)) +
  labs(x= "Year",
       y="Estimated Prices",
       title="Estimated Price per Year"
  ) +
  theme(axis.text.x = element_text(angle = 90, hjust=1))

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
table.price <- final.hcris %>% group_by(penalty) %>%
summarise(mean.price = mean(price))

#Create Quartiles for Bed Size

final.hcris.quartiles <- final.hcris %>%
mutate(first.quartile = ifelse(beds <= quantile(final.hcris$beds, 0.25, na.rm = TRUE), 1, 0))%>%
mutate(second.quartile = ifelse(beds <= quantile(final.hcris$beds, 0.5, na.rm = TRUE) & beds > quantile(final.hcris$beds, 0.25, na.rm = TRUE), 1, 0)) %>%
mutate(third.quartile = ifelse(beds <= quantile(final.hcris$beds, 0.75, na.rm = TRUE) & beds > quantile(final.hcris$beds, 0.5, na.rm = TRUE), 1, 0)) %>%
mutate(fourth.quartile = ifelse(beds > quantile(final.hcris$beds, 0.75, na.rm = TRUE), 1, 0))

#Find Average Prices by Quartile

q1.mean <- final.hcris.quartiles %>%
filter(first.quartile == 1) %>%
group_by(penalty) %>%
summarise(first.mean = mean(price, na.rm = TRUE))

q2.mean <- final.hcris.quartiles %>%
filter(second.quartile == 1) %>%
group_by(penalty) %>%
summarise(second.mean = mean(price, na.rm = TRUE))

q3.mean <- final.hcris.quartiles %>%
filter(third.quartile == 1) %>%
group_by(penalty)%>%
summarise(third.mean = mean(price, na.rm = TRUE))

q4.mean <- final.hcris.quartiles %>%
filter(fourth.quartile == 1) %>%
group_by(penalty) %>%
summarise(foruth.mean = mean(price, na.rm = TRUE))

#Join into Table

table.quartiles <- q1.mean %>%
left_join(q2.mean, by = "penalty") %>%
left_join(q3.mean, by = "penalty") %>%
left_join(q4.mean, by = "penalty")


#Object for Matching
lp.vars <- final.hcris.quartiles %>% 
  ungroup() %>%
    select(penalty,price, first.quartile, second.quartile, third.quartile, fourth.quartile) %>%
    filter(complete.cases(.))
lp.covs <- lp.vars %>% select(first.quartile, second.quartile, third.quartile, fourth.quartile)

#Inverse Matching
m.nn.var <- Matching::Match(Y=lp.vars$price,
                            Tr=lp.vars$penalty,
                            X=lp.covs,
                            M=4,  #<<
                            Weight=1,
                            estimand="ATE")

summary(m.nn.var)

#Mahalanobis
m.nn.md <- Matching::Match(Y=lp.vars$price,
                           Tr=lp.vars$penalty,
                           X=lp.covs,
                           M=1,
                           Weight=2,
                           estimand="ATE")  

summary(m.nn.md)

#Propensity Score
logit.model <- glm(penalty ~ first.quartile + second.quartile + third.quartile + fourth.quartile, data=lp.vars)
ps <- fitted(logit.model)
m.nn.ps <- Matching::Match(Y=lp.vars$price,
                           Tr=lp.vars$penalty,
                           X=ps,
                           M=1,
                           estimand="ATE")

summary(m.nn.ps)

#Regression
reg.dat <- lp.vars %>% ungroup() %>% filter(complete.cases(.)) %>%
  mutate(q1.diff = penalty*(first.quartile - mean(first.quartile)),
         q2.diff = penalty*(second.quartile - mean(second.quartile)),
         q3.diff = penalty*(third.quartile - mean(third.quartile)),
         q4.diff = penalty*(fourth.quartile - mean(fourth.quartile)))
reg <- lm(price ~ penalty + first.quartile + second.quartile + third.quartile + fourth.quartile + q1.diff + q2.diff + q3.diff + q4.diff,
          data=reg.dat) 

summary(reg)

table.ates <- rbind(m.nn.md$est, m.nn.ps$est, m.nn.var$est, reg$penaltyTRUE)

print(table.ates)

save.image("submission2/hwk2_workspace.Rdata")
