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
  group_by(fyear)%>%
  count()

fig.hospital.reports <- ggplot(hospital.reports, aes(x = fyear, y = n))+
  geom_line()+
  geom_point() +
  theme_bw() + 
  theme(axis.text.x = element_text(angle = 90, hjust=1))


#Get Count of Hospitals
unique.count <-n_distinct(full.hcris.data$provider_number)
print(unique.count)

#Violin Plot Total Charges/Year
fig.total.charges <- full.hcris.data %>% 
  ggplot(aes(x=as.factor(year), y = tot_charges)) +
  geom_jitter(alpha = .05) +
  geom_violin(alpha = .9, draw_quantiles = c(0.5)) + 
  theme(axis.text.x = element_text(angle = 90, hjust=1))

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
  geom_violin(alpha = .9, draw_quantiles = c(0.5)) +
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
final.hcris %>% group_by(penalty) %>%
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

v.name=data.frame(new=c("Beds","Medicaid Discharges", "Inaptient Charges",
                   "Medicare Discharges", "Medicare Payments"))

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
logit.model <- glm(penalty ~ beds + mcaid_discharges + ip_charges + mcare_discharges +
            tot_mcare_payment, family=binomial, data=lp.vars)
ps <- fitted(logit.model)
m.nn.ps <- Matching::Match(Y=lp.vars$price,
                           Tr=lp.vars$penalty,
                           X=ps,
                           M=1,
                           estimand="ATE")

summary(m.nn.ps)

#Regression
reg.dat <- lp.vars %>% ungroup() %>% filter(complete.cases(.)) %>%
  mutate(beds_diff = penalty*(beds - mean(beds)),
         mcaid_diff = penalty*(mcaid_discharges - mean(mcaid_discharges)),
         ip_diff = penalty*(ip_charges - mean(ip_charges)),
         mcare_diff = penalty*(mcare_discharges - mean(mcare_discharges)),
         mpay_diff = penalty*(tot_mcare_payment - mean(tot_mcare_payment)))
reg <- lm(price ~ penalty + beds + mcaid_discharges + ip_charges + mcare_discharges + tot_mcare_payment + 
            beds_diff + mcaid_diff + ip_diff + mcare_diff + mpay_diff,
          data=reg.dat)

summary(reg)

table.ates <- rbind(m.nn.md$est, m.nn.ps$est, m.nn.var$est)

print(table.ates)

save.image("submission2/hwk2_workspace.Rdata")
