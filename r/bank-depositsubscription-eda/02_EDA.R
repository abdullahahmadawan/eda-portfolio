
#Required packages: ggplot2 and Hmisc#

library("ggplot2") 
library(Hmisc)

#Visualization#

#Categorical Frequency Plots using Base R visualization#

dir.create("plots", showWarnings = FALSE)

png("plots/subscribed_bar.png", width = 900, height = 600)
barplot(table(bank_clean$subscribed))

barplot(table(bank_clean$prev_campaign_outcome))

barplot(table(bank_clean$month))

barplot(table(bank_clean$occupation))

barplot(table(bank_clean$credit_default))

barplot(table(bank_clean$has_personal_loan))

barplot(table(bank_clean$has_savings_account))

#Bivariate relationship examination using GGplot#

#Subscription outcome compared with hypothesized variables#

ggplot(data = bank_clean, aes(x = prev_campaign_outcome, fill = subscribed)) +
  geom_bar(position = "fill") +
  labs(title="outcome by previous campaign outcome", x="previous campaign outcome")

ggsave("plots/outcome_by_previous_campaign_outcome.png", width = 8, height = 5)

ggplot(data = bank_clean, aes(x = has_current_account, fill = subscribed)) +
  geom_bar(position = "fill") +
  labs(title="outcome by whether customer has current account", x="current account?")

ggsave("plots/outcome_by_whether_customer_has_current_account.png", width = 8, height = 5)

ggplot(data = bank_clean, aes(x = has_mortgage, fill = subscribed)) +
  geom_bar(position = "fill") +
  labs(title="outcome by whether customer has mortgage", x="mortgage?")

ggsave("plots/outcome_by_whether_customer_has_mortgage.png", width = 8, height = 5) 

ggplot(data = bank_clean, aes(x = euribor_three_mth, y = subscribed == "yes")) +
  stat_summary_bin(
    fun = mean,
    bins = 20,
    geom = "point"
  ) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(x = "Three Month Interest Rate", y = "Probability of Subscription")


ggplot(data = bank_clean , aes(x = cons_conf_index, y = subscribed == "yes")) +
  stat_summary_bin(
    fun = mean,
    bins = 20,
    geom = "point") +
  geom_smooth(method = "loess", se = FALSE) +
  labs(x = "Consumer Confidence Index", y = "Probability of Subscription")

#Subscription outcome compared with additional variables#

ggplot(data = bank_clean, aes(x = salary_level, fill = subscribed)) +
  geom_bar(position = "fill") +
  labs(title="outcome by salary level", x="salary level")

ggsave("plots/outcome_by_whether_customer_has_mortgage.png", width = 8, height = 5)

ggplot(data = bank_clean, aes(x = has_personal_loan, fill = subscribed)) +
  geom_bar(position = "fill") +
  labs(title="outcome whether customer has personal loan", x="personal loan?")

ggsave("plots/outcome_by_whether_customer_has_mortgage.png", width = 8, height = 5)

ggplot(data = bank_clean, aes(x = has_savings_account, fill = subscribed)) +
  geom_bar(position = "fill")+
  labs(title="outcome whether customer has savings account", x="savings account?")

ggsave("plots/outcome_by_whether_customer_has_mortgage.png", width = 8, height = 5)

ggplot(data = bank_clean, aes(x = has_credit_card, fill = subscribed)) +
  geom_bar(position = "fill")+
  labs(title="outcome whether customer has credit card", x="credit card?")

ggsave("plots/outcome_by_whether_customer_has_mortgage.png", width = 8, height = 5)

ggplot(data = bank_clean, aes(x = contact_method, fill = subscribed)) +
  geom_bar(position = "fill")

ggplot(data = bank_clean, aes(x = month, fill = subscribed)) +
  geom_bar(position = "fill")

ggplot(data = bank_clean, aes(x = day, fill = subscribed)) +
  geom_bar(position = "fill")

table(bank_clean$subscribed, bank_clean$prev_campaign_outcome)

#Measures of Association#

#Between Target Variable and Hypothesized Predictors#

#Categorical Predictors#

chisq.test(bank_clean$subscribed, bank_clean$prev_campaign_outcome)

chisq.test(bank_clean$subscribed, bank_clean$has_current_account)

chisq.test(bank_clean$subscribed, bank_clean$has_mortgage)

#Numerical Predictors#

t.test(euribor_three_mth ~ subscribed, data = bank_clean, conf.level = 0.95, na.action = na.exclude)

t.test(cons_conf_index ~ subscribed, data = bank_clean, conf.level = 0.95, na.action = na.exclude)
