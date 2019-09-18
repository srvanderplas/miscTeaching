library(tidyverse)
library(magrittr)
library(tidymodels)
library(kknn)
# csv url: https://raw.githubusercontent.com/srvanderplas/
#          miscTeaching/master/ds303_2019/2017_Human_Dev_Index.csv

hdi_2017 <- read_csv("https://bit.ly/2mbYdfX")
vars <- names(hdi_2017)

response <- "Happiness_Index ~"
formulas <- c(paste(vars[str_detect(vars, "^Idx") & !str_detect(vars, "male$")], collapse = "+"),
              paste(vars[str_detect(vars, "PctGDP")], collapse = "+"),
              "Infants_Missing_DPT_Vaccine + Infants_Missing_measles_Vaccine + Teen_Birth_Rate + Working_Poor_Pct_3.10day",
              # paste(vars[str_detect(vars, "^Empl|Unempl|Labor") &
              #              !str_detect(vars, "(total|male)$")], collapse = "+"),
              paste(vars[str_detect(vars, "[Ss]chool|education")], collapse = "+"))
used_vars <- purrr::map(formulas, ~str_split(., stringr::fixed("+")) %>% unlist() %>% str_trim() %>% unique())
hdi_data <- purrr::map(used_vars, ~select(hdi_2017, Happiness_Index, one_of(.)) %>%
  na.omit())

formulas <- paste(response, formulas)
formula_tbl <- tibble(formula = formulas, data = hdi_data, var_type = c("Index", "GDP", "Medical_Poverty", "Education"))
models <- tribble(~name, ~fcn,
                  "random_forest", rand_forest(mode = "regression") %>% set_engine("ranger"),
                  "linear", linear_reg(mode = "regression") %>% set_engine("lm"),
                  "knn", nearest_neighbor(mode = "regression", weight_func = "triweight"))

fit_models <- tidyr::crossing(models, formula_tbl) %>%
  mutate(formula = purrr::map(formula, as.formula)) %>%
  mutate(model = purrr::pmap(list(formula = formula, object = fcn, data = data), fit)) %>%
  mutate(estimate = purrr::map2(model, data, predict.model_fit)) %>%
  mutate(truth = purrr::map(data, "Happiness_Index")) %>%
  mutate(estimate = purrr::map(estimate, ".pred")) %>%
  mutate(rsq = purrr::map2_dbl(truth, estimate, ~rsq_vec(.x, .y)))

model_data <- fit_models %>%
  select(-model, -truth, -rsq, -fcn) %>%
  group_by(var_type) %>%
  mutate(name = paste0("pred_", name)) %>%
  tidyr::spread(key = name, value = estimate) %>%
  select(-formula) %>%
  nest(-var_type) %>%
  mutate(data = purrr::map(data, unnest)) %>%
  mutate(data = purrr::map(data, ~left_join(., hdi_2017)))

walk(split(model_data, model_data$var_type),
     ~write_csv(.$data[[1]], path = file.path("ds303_2019",
                                              paste0(.$var_type, "_models.csv"))))

