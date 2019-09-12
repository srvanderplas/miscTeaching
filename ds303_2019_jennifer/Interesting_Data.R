download.file("http://www.who.int/entity/immunization/monitoring_surveillance/data/coverage_estimates_series.xls", destfile = "ds303_2019_jennifer/WHO_Vaccination_Rates.xls", mode = "wb")
download.file("https://population.un.org/wpp/Download/Files/1_Indicators%20(Standard)/EXCEL_FILES/1_Population/WPP2019_POP_F01_1_TOTAL_POPULATION_BOTH_SEXES.xlsx", destfile = "ds303_2019_jennifer/World_Population_Rate.xlsx", mode = "wb")
download.file("http://ec2-54-174-131-205.compute-1.amazonaws.com/API/hdro_api_all.json", "ds303_2019_jennifer/human_dev_index.json")

library(readxl)
library(tidyverse)

# --- WHO Vaccine Data Cleaning ------------------------------------------------
who_vaccine_data <- purrr::map(1:16, ~read_xls("~/Downloads/WHO_Vaccination_Rates.xls", sheet = .))

who_vaccine_readme <- who_vaccine_data[[1]]
who_vaccine_aggregate <- who_vaccine_data[[16]]

vaccine_names <- who_vaccine_readme[13:26, 2:3] %>%
  set_names(c("Vaccine", "Description"))

vaccination_by_year <- who_vaccine_data %>%
  magrittr::extract(2:14) %>%
  bind_rows() %>%
  gather(key = "year", value = "rate", -Region, -ISO_code, -Cname, -Vaccine) %>%
  mutate(year = parse_number(year)) %>%
  filter(!is.na(rate))

vaccination_by_year %>%
  filter(Vaccine == "MCV1") %>%
  mutate(Region = str_replace_all(Region, c("AFR" = "Africa", "AMR" = "Americas", "EMR" = "Eastern Mediterranean", "EUR" = "Europe", "SEAR" = "SE Asia", "WPR" = "Western Pacific"))) %>%
ggplot(aes(x = year, y = rate, group = interaction(Cname, Vaccine))) +
  geom_line() +
  geom_smooth(aes(x = year, y = rate), inherit.aes = F) +
  facet_wrap(~Region) +
  ggtitle("Vaccination Rates for 1st dose of Measles vaccine")

smoothed_vacc_rate <- vaccination_by_year %>% mutate(halfdec = year - year %% 5) %>%
  group_by(halfdec, Vaccine, Cname, Region) %>%
  summarize(rate = mean(rate))

who_agg_vaccination <- who_vaccine_aggregate %>%
  gather(key = "year", value = "rate", -Grouping, - Affilifation, -Vaccine) %>%
  mutate(year = parse_number(year)) %>%
  filter(!is.na(rate))

who_agg_vaccination %>%
  mutate(Affiliation = str_replace_all(Affilifation, c("AFR" = "Africa", "AMR" = "Americas",
                                                 "EMR" = "Eastern Mediterranean",
                                                 "EUR" = "Europe", "SEAR|sear" = "Southeast Asia",
                                                 "WPR|wpr" = "Western Pacific"))) %>%
  mutate(Affiliation = str_to_title(Affiliation) %>%
           factor(levels = c("Africa", "Americas", "Eastern Mediterranean", "Europe",
                             "Southeast Asia", "Western Pacific",
                             "High Income", "Middle Income", "Low Income", "Global"), ordered = T)) %>%
  filter(!str_detect(Grouping, "[Gg][Aa][Vv][Ii]")) %>%
  ggplot(aes(x = year, y = rate, color = Vaccine)) +
  geom_line() +
  # geom_smooth(aes(x = year, y = rate), inherit.aes = F) +
  facet_wrap(~Affiliation, ncol = 3) +
  ggtitle("Coverage Rates by Region and Income Level, per vaccine")

# ------------------------------------------------------------------------------
# --- World Population Data ----------------------------------------------------
world_pop <- read_xlsx("~/Downloads/World_Population_Rate.xlsx", skip = 16,
                       trim_ws = T) %>%
  select(Region = `Region, subregion, country or area *`, Region_Type = Type,
         Country_code = `Country code`, matches("\\d{4}")) %>%
  mutate(Subregion = ifelse(Region_Type == "Subregion", Region, NA),
         SDG_region = ifelse(Region_Type == "SDG region", Region, NA),
         SDG_subregion = ifelse(Region_Type == "SDG subregion", Region, NA),
         Country = ifelse(Region_Type == "Country/Area", Region, NA)) %>%
  fill(Subregion, SDG_region, SDG_subregion, .direction = "down") %>%
  filter(!Region_Type == "Label/Separator") %>%
  select(matches("^\\D"), everything()) %>%
  gather(key = "year", value = "pop", -c(1:3),
         -Subregion, -SDG_region, -SDG_subregion, -Country)

# ------------------------------------------------------------------------------

# --- Human Development Index Data ---------------------------------------------
hdi_all <- jsonlite::fromJSON("http://ec2-54-174-131-205.compute-1.amazonaws.com/API/hdro_api_all.json")

hdi <- hdi_all %>%
  mutate(indicator_id = parse_integer(indicator_id),
         year = parse_integer(year),
         value = parse_number(value))
