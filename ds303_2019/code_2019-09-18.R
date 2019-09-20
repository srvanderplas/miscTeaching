# Get essential code from monday's class -
#   - read in data
#   - fit bad model
#   - save bad model predictions/residuals
source("http://bit.ly/ds303-monday")

meh_model <- lm(Happiness_Index ~ .,
                data = select(model_data, -Country,
                              -Gross_National_Income,
                              -matches("bad_model")))
summary(meh_model)
model_data$meh_model_resid <- resid(meh_model)

ggplot(data = model_data,
       aes(x = Happiness_Index, y = meh_model_resid)) +
  geom_point() + geom_smooth(method = "lm")

library(tidyverse)
data <- read_csv("http://bit.ly/ds303_gdp")

long_data <- select(data, 1:3, Happiness_Index, everything()) %>%
  tidyr::gather(key = "model", value = "value", matches("pred")) %>%
  mutate(resid = Happiness_Index - value)
ggplot(data = long_data) +
  geom_point(aes(x = Happiness_Index, y = value)) +
  geom_abline(slope = 1, intercept = 0) +
  facet_wrap(~model) +
  coord_fixed()
ggplot(data = long_data) +
  geom_density(aes(x = Happiness_Index - value)) +
  facet_wrap(~model)
ggplot(data = long_data) +
  geom_point(aes(x = Exports_Imports_PctGDP, y = resid)) +
  facet_wrap(~model)

ggplot(data = long_data) +
  geom_point(aes(x = Finance_Sector_Domestic_Credit_PctGDP, y = resid)) +
  facet_wrap(~model)

ggplot(data = long_data) +
  geom_point(aes(x = Foreign_Direct_Investment_PctGDP, y = resid)) +
  facet_wrap(~model)

ggplot(data = long_data) +
  geom_point(aes(x = Gross_Capital_PctGDP, y = resid)) +
  facet_wrap(~model)
