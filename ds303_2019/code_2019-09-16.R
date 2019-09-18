library(tidyverse)
# csv url: https://raw.githubusercontent.com/srvanderplas/
#          miscTeaching/master/ds303_2019/2017_Human_Dev_Index.csv

hdi_2017 <- read_csv("https://bit.ly/2mbYdfX")

names(hdi_2017)

ggplot(hdi_2017) +
  geom_point(aes(x = GDP, y = Happiness_Index))

ggplot(hdi_2017) +
  geom_point(aes(x = MtoF_Ratio_birth, y = Happiness_Index))


model_data <- select(hdi_2017, Country, Happiness_Index,
                     Ineq_education, Ineq_income, Ineq_Life_Exp,
                     Teen_Birth_Rate, Life_Exp_Birth,
                     GDP, Gross_National_Income) %>%
  mutate(log10GDP = log10(GDP)) %>%
  select(-GDP) %>%
  na.omit()

bad_model <- lm(Happiness_Index ~ ., data = select(model_data, -Country))
summary(bad_model)


model_data$bad_model_resid <- resid(bad_model)

ggplot(data = model_data) +
  geom_histogram(aes(x = bad_model_resid), binwidth =.2)
