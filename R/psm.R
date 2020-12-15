library(tidyr)
library(dplyr)
library(readxl)
library(MatchIt)
library(ggplot2)

setwd("C:/github/Rcode-handbook-on-impact-eval-world-bank/")
data <- read_excel("data/dataset.xls")

# boxplots
data %>%
  gather(period, value, -treatment, -category, -product) %>%
  mutate(
    period = factor(period, levels = c("before", "after")),
    treatment = ifelse(treatment == 1, "treatment", "control") 
    ) %>%
  ggplot(aes(x = period, y = value, group = period)) +
  geom_boxplot() +
  facet_wrap(~ treatment) +
  xlab("Період (до та після втручання)") + ylab("Індекс споживчих цін") +
  theme_bw() 

# matching
m.out <- matchit(treatment ~ category + before, data = data, method="nearest")
m.out
summary(m.out)

balanced <- match.data(m.out)
balanced

# Matched control group 
balanced %>% filter(treatment == 0)


# Unmatched rows
left_join(data, balanced, by = "product") %>% filter(is.na(category.y))


# DiD
balanced %>%
  mutate(dif = after - before) %>%
  group_by(treatment) %>%
  summarise(mean_group_effect = mean(dif)) %>%
  mutate(mean_total_effect = mean_group_effect - lag(mean_group_effect, default = first(mean_group_effect)))

data %>%
  mutate(dif = after - before) %>%
  group_by(treatment) %>%
  summarise(mean_group_effect = mean(dif)) %>%
  mutate(mean_total_effect = mean_group_effect - lag(mean_group_effect, default = first(mean_group_effect)))

ttestdata <- balanced %>% 
  select(-weights, -subclass, -distance) %>%
  gather(period, value, -treatment, -category, -product) 

tb <- ttestdata %>% filter(period == "before")
ta <- ttestdata %>% filter(period != "before")

t.test(data=tb, value ~ treatment, var.equal = TRUE)
t.test(data=ta, value ~ treatment, var.equal = TRUE)

ttest_data <- balanced %>% mutate(dif = after - before)

t.test(data=ttest_data, dif ~ treatment, var.equal = TRUE)
