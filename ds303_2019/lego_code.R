library(tidyverse)
library(LegoR)

lego_sets <- read_csv("ds303_2019/rebrickable-lego-sets.csv")
lego_themes <- read_csv("ds303_2019/rebrickable-lego-themes.csv") %>%
  set_names(c("theme_id", "theme_name", "theme_parent"))

lego_sets <- left_join(lego_sets, lego_themes, by = "theme_id")

# if (exists(".brickset_key") & exists(".brickset_hash")) {
#   brickset_get_sets(theme = "Architecture", pageSize = "10")
# }
#
# sets_2019 <- brickset_get_sets(year = "2019", pageSize = 1000)
# sets_2018 <- brickset_get_sets(year = "2018", pageSize = 1000)
# sets_2017 <- brickset_get_sets(year = "2017", pageSize = 1000)
# sets_2016 <- brickset_get_sets(year = "2016", pageSize = 1000)
# sets_2015 <- brickset_get_sets(year = "2015", pageSize = 1000)
#
# sets <- purrr::map(2000:2019, ~brickset_get_sets(year = ., pageSize = 900))
#
# sets_df <- do.call("bind_rows", sets) %>%
#   bind_rows(sets_2019, sets_2018, sets_2017, sets_2016, sets_2015)
#
# write_csv(sets_df, "ds303_2019/brickset-lego-sets.csv")
sets_df <- read_csv("ds303_2019/brickset-lego-sets.csv")

lego <- sets_df %>%
  mutate(set_num = paste(number, numbervariant, sep = "-")) %>%
  semi_join(lego_sets) %>%
  select(set_num, name, year, theme, subtheme, pieces, minifigs,
         usretailprice, rating, reviewcount, agemin, agemax) %>%
  filter(!is.na(usretailprice) & !is.na(pieces)) %>%
  mutate(outside_brand = str_detect(theme,
                                    "Mickey|Star.Wars|Harry.Potter|Spider-Man|SpongeBob|Indiana.Jones|Atlantis|The.Lord.of.the.Rings|The.Hobbit|Marvel|DC.Comics|Pirates.of.the.Caribbean|Teenage.Mutant.Ninja.Turtles|Minecraft|The.Lone.Ranger|The.Simpsons|Stranger.Things|Jurassic.World|Overwatch|Powerpuff|Batman|DC.Super|Angry.Birds|Ghostbusters|Scooby-Doo")) %>%
  mutate(baby = str_detect(theme, "Duplo|Quatro"),
         education = str_detect(theme, "Education|Mindstorms|Serious.Play"),
         broad_theme = case_when(
           outside_brand ~ "Outside brand",
           baby ~ "Baby",
           education ~ "Educational",
           TRUE ~"Other"
         ))

write_csv(lego, "ds303_2019/lego-prices.csv")

ggplot(lego, aes(x = pieces, y = usretailprice)) + geom_point() +
  geom_smooth(method = "lm")


ggplot(lego, aes(x = minifigs, y = usretailprice)) + geom_jitter(alpha = .2) +
  geom_smooth(method = "lm")


rf_model <- rand_forest(mode = "regression") %>%
  set_engine("ranger") # set_engine specifies
# how the model is fit
linear_model <- linear_reg() %>%
  set_engine("lm")
knn_model <- nearest_neighbor(mode = "regression", weight_func = "triweight") %>%
  set_engine("kknn")

lego_split <- initial_split(lego, prop = .9, strata = usretailprice, breaks = 10)
lego_train <- training(lego_split)
lego_test <- testing(lego_split)

lego_models <- tibble::tibble(
  model_name = c("random_forest", "linear_regression", "knn"),
  model_fcn = list(rf_model, linear_model, knn_model)) %>%
  dplyr::mutate(
    # fit using the training set
    res = purrr::map(
      model_fcn,
      ~fit(., formula = usretailprice ~ pieces, data = lego_train)),
    # predict based on the test set
    pred = purrr::map(res, predict,
                      new_data = lego_test)
  )

lego_models$res[[1]]$fit

summary(lego_models$res[[2]]$fit)

lego_models$res[[3]]$fit

lego_test_preds <- lego_models %>%
  # Get predictions and model names
  select(model_name, pred) %>%
  # Make a long, skinny data frame
  unnest() %>%
  # rename variables
  rename(pred = .pred) %>%
  # For each model,
  group_by(model_name) %>%
  # number the rows 1:n(), where n() = 731
  mutate(row = 1:n()) %>%
  # Put each model in a different column
  spread(key = model_name, value = pred) %>%
  # Add in the actual test set data
  bind_cols(lego_test) %>%
  # Go back to a long-form dataset where predictions are in one column
  gather(key = "model_name", value = "pred",
         random_forest:knn)
ggplot(aes(x = usretailprice, y = pred),
       data = lego_test_preds) +
  geom_point() + facet_wrap(~model_name) +
  geom_abline(slope = 1, intercept = 0) +
  coord_fixed() +
  ggtitle("Test Set Predictions")


lego_cv <- vfold_cv(lego_train, v = 10)
lego_cv[1,]
lego_cv$splits[[1]]

analysis(lego_cv$splits[[1]])
assessment(lego_cv$splits[[1]])

# Create our model fitting function
lego_lm <- function(cv) {
  lm(formula = usretailprice ~ pieces,
     data = analysis(cv))
}

lego_cv_lm <- lego_cv %>%
  # map takes each set and applies a function
  mutate(model = purrr::map(splits, lego_lm)) %>%
  # get out the coefficients
  mutate(coefs = purrr::map(model, "coefficients"),
         slope = purrr::map_dbl(coefs, "pieces"),
         intercept = purrr::map_dbl(coefs, "(Intercept)"))

lego_loess <- function(cv) {
  loess(formula = usretailprice ~ pieces,
        data = analysis(cv))
}

# Make a table of possible predictive values
output_range <- tibble(
  pieces = seq(min(lego$pieces), max(lego$pieces), length.out = 100)
)

# Get regression lines and assessment set predictions
lego_cv_loess <- lego_cv %>%
  mutate(
    model = purrr::map(splits, lego_loess),
    pred_line = purrr::map(model, ~bind_cols(
      output_range,
      pred = predict(., newdata = output_range))),
    pred_assessment = purrr::map2(
      model, splits, ~bind_cols(
        assessment(.y),
        pred = predict(.x, newdata = assessment(.y))))
  )
select(lego_cv_loess, id, pred_line) %>% unnest() %>%
  ggplot() +
  geom_point(aes(x = pieces, y = usretailprice),
             data = lego_train, color = "red", alpha = .2) +
  geom_line(aes(x = pieces, y =  pred, group = id), alpha = .1)


# Get regression lines and assessment set predictions
lego_cv_lm <- lego_cv %>%
  mutate(
    model = purrr::map(splits, lego_lm),
    pred_line = purrr::map(model, ~bind_cols(
      output_range,
      pred = predict(., newdata = output_range))),
    pred_assessment = purrr::map2(
      model, splits, ~bind_cols(
        assessment(.y),
        pred = predict(.x, newdata = assessment(.y))))
  )
select(lego_cv_lm, id, pred_line) %>% unnest() %>%
  ggplot() +
  geom_point(aes(x = pieces, y = usretailprice),
             data = lego_train, color = "red", alpha = .2) +
  geom_line(aes(x = pieces, y =  pred, group = id), alpha = .1)
