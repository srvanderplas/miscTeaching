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
