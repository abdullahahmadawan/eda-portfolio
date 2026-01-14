#setting the working directory#

getwd()

setwd(DIRECTORY OF DATA PATH)

#enabling packages#

readxl, tidyverse

library("readxl")
library("tidyverse")

#reading in the data#

bank_raw <- read_excel(Data Path)

summary(bank_raw)

#assigning the correct factors to character variables#

bank_raw$gender <- as.factor(bank_raw$gender)

bank_raw$occupation <- as.factor(bank_raw$occupation)

bank_raw$salary_level <- as.factor(bank_raw$salary_level)

bank_raw$marital_status <- as.factor(bank_raw$marital_status)

bank_raw$education_level <- as.factor(bank_raw$education_level)

bank_raw$credit_default <- as.factor(bank_raw$credit_default)

bank_raw$has_mortgage <- as.factor(bank_raw$has_mortgage)

bank_raw$has_car_insurance <- as.factor(bank_raw$has_car_insurance)

bank_raw$has_credit_card <- as.factor(bank_raw$has_credit_card)

bank_raw$has_life_insurance <- as.factor(bank_raw$has_life_insurance)

bank_raw$has_savings_account <- as.factor(bank_raw$has_savings_account)

bank_raw$has_personal_loan <- as.factor(bank_raw$has_personal_loan)

bank_raw$has_current_account <- as.factor(bank_raw$has_current_account)

bank_raw$contact_method <- as.factor(bank_raw$contact_method)

bank_raw$month <- as.factor(bank_raw$month)

bank_raw$day <- as.factor(bank_raw$day)

bank_raw$subscribed <- as.factor(bank_raw$subscribed)

#Checked and Fixed the Levels for Relevant Variables#

levels(bank_raw$salary_level)

levels(bank_raw$salary_level)[3] <- "medium"

bank_raw$salary_level <- factor(bank_raw$salary_level, 
                                levels = c("low", "medium", "high"))
levels(bank_raw$day)

levels(bank_raw$day)[2] <- "fri"

bank_raw$day <- factor(bank_raw$day, 
                       levels = c("mon", "tue", "wed", "thu", "fri"))

summary(bank_raw)

bank_clean <- bank_raw
