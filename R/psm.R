library(dplyr)
library(readxl)
library(MatchIt)


setwd("C:/github/Rcode-handbook-on-impact-eval-world-bank/")
data <- read_excel("data/dataset.xls")


# matching
m.out <- matchit(treatment ~ category + before, data = data, method="nearest")
m.out
summary(m.out)

balanced <- match.data(m.out)
balanced

# DiD
balanced %>%
  mutate(dif = after - before) %>%
  group_by(treatment) %>%
  summarise(mean_effect = mean(dif)) %>%
  mutate(total_effect = mean_effect - lag(mean_effect, default = first(mean_effect)))
