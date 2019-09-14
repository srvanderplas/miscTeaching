# download.file("http://www.who.int/entity/immunization/monitoring_surveillance/data/coverage_estimates_series.xls", destfile = "ds303_2019/WHO_Vaccination_Rates.xls", mode = "wb")
# download.file("https://population.un.org/wpp/Download/Files/1_Indicators%20(Standard)/EXCEL_FILES/1_Population/WPP2019_POP_F01_1_TOTAL_POPULATION_BOTH_SEXES.xlsx", destfile = "ds303_2019/World_Population_Rate.xlsx", mode = "wb")
# download.file("http://ec2-54-174-131-205.compute-1.amazonaws.com/API/hdro_api_all.json", "ds303_2019/human_dev_index.json")

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
hdi_all <- jsonlite::fromJSON("ds303_2019/human_dev_index.json")

hdi <- hdi_all %>%
  mutate(indicator_id = parse_integer(indicator_id),
         year = parse_integer(year),
         value = parse_number(value))

aggregate_index <- c(38406, 68606, 71406, 71506, 71606, 73506, 101606, 101706, 101806, 103206, 103606, 103706, 135006, 136906, 137006, 137506, 137906, 138806, 146206, 181606)
population_count <- c(21806, 44206, 45106, 47906, 49006, 63106, 110806, 111106, 132706, 132806, 153706, 183406) # Population
education_vars <- c(23806, 23906, 24006, 24106, 24206, 45806, 46106, 46206, 63206, 63306, 63406, 69706, 101406, 103006, 103706, 123306, 123406, 133006, 147206, 147906, 149206, 176806, 176906, 177006, 179706)
mortality_health <- c(36806, 57206, 57506, 57806, 57906, 58006, 61006, 64306, 64406, 69206, 89006, 101806, 103206, 112506, 112606, 120606, 121106, 121206, 174506, 174606, 175206, 175506, 175606, 177106, 177206, 177706, 179706, 181206, 181806, 182106, 182206)
government_econ <- c(31706, 43006, 48706, 48806, 52606, 53506, 65606, 99106, 110906, 111306, 122006, 123506, 123606, 127606, 133206, 136706, 140606, 141706, 143306, 148206, 148306, 150606, 150706, 153706, 164406, 169706, 169806, 170006, 174206, 175006, 175106, 175706, 178306, 179406, 187306)
tech <- c(27706, 43606, 46006, 52306, 163906, 174306, 174406, 181706)
poverty <- c(38506, 38606, 101006, 102006, 117806, 117906, 118006, 142506)
environment <- c(27706, 97106, 100806, 174406)
gender <- c(68606, 137906, 23906, 24006, 24106, 24206, 48706, 48806, 49006, 57806, 57906, 112506, 112606, 120606, 121106, 123306, 124406, 123506, 123606, 136906, 137006, 169706, 169806, 175006, 175106, 175706)
hdi_aggregate <- filter(hdi, indicator_id %in% aggregate_index) %>%
  mutate(name = str_replace_all(indicator_name,
                                c("Gender Inequality Index.*" = "Gender_Inequality_Idx",
                                  "Inequality-adjusted " = "Inequality_adj_",
                                  "Overall loss.*" = "HDI_loss_Inequality",
                                  "Inequality in ([a-z]{1,}).*" = "\\1_Inequality_Pct",
                                  "Life expectancy" = "Life_expectancy",
                                  "Coefficient of human inequality" = "Human_Inequality_Coef",
                                  "Human Development Index .HDI." = "HDI",
                                  "HDI, (.*)" = "HDI_\\1",
                                  "Gender Development Index.*" = "GDI",
                                  " .IHDI." = "",
                                  "HDI rank" = "HDI_rank",
                                  "Red List.*" = "Red_List_Idx",
                                  "Multidimensional poverty index.*" = "Multidim_poverty_idx")) %>%
           str_replace_all(" ", "_") %>%
           gsub("^([a-z])", "\\U\\1", ., perl = T) %>%
           gsub("_([a-z])", "_\\U\\1", ., perl = T)) %>%
  select(-indicator_id, -indicator_name) %>%
  tidyr::spread(key = name, value = value)

hdi_education <- filter(hdi, indicator_id %in% education_vars) %>%
  mutate(name = str_replace_all(
    indicator_name,
    c("Population with at least some secondary education,? ?(.*)? .. ages.*" = "Secondary_Education.Pct_Pop_Over25 \\1",
      "Mean years of schooling,? ?(.*)? .years." = "Avg_Years_School \\1",
      "Expected years of schooling,? ?(.*)? .years." =  "Expected_Years_School \\1",
      "Pupil-teacher ratio.*" = "Primary_School_Pupil_Teacher_Ratio",
      "Gross enroll?ment ratio, (.*) .. of [a-z]{1,}( school)?-age population." = "\\1 Gross_Enrollment_Ratio",
      "Education index" = "Education_Idx",
      "Government expenditure on education.*" = "Govt_Exp_Education.Pct_GDP",
      "Youth not in school or employment.*" = "Youth_NoSchool_NoWork.Pct1524",
      "International student mobility.*" = "International_Student_Mobility.Pct_tertiary",
      "Literacy rate.*" = "Adult_Literacy",
      "Primary school dropout rate.*" = "Primary_Dropout_Rate",
      "Primary school teachers trained to teach.*" = "Primary_Teacher_Training_Rate",
      "Ratio of education and health.*" = "Edu_Health_to_Military_Spending",
      "Programme for International Student Assessment .PISA. score in (.*)" = "PISA_score_\\1")) %>%
      str_replace_all(" ", "_") %>%
      gsub("^([a-z])", "\\U\\1", ., perl = T) %>%
      gsub("_([a-z])", "_\\U\\1", ., perl = T)) %>%
  select(-indicator_id, -indicator_name) %>%
  tidyr::spread(key = name, value = value)

hdi_complete <- hdi %>% group_by(indicator_name, country_name) %>%
  summarize(years = sum(!is.na(value)), y2017 = sum(year == 2017), y2016 = sum(year == 2016)) %>%
  summarize(avg_years = mean(years), x5 = sum(years >= 5), n2017 = sum(y2017), n2016 = sum(y2016)) %>%
  filter(avg_years > 6 & x5 > 150) %>%
  left_join(hdi) %>%
  select(-avg_years, -x5)

hdi_complete$indicator_name %>% unique()

hdi_2017 <- hdi %>% filter(year == 2017) %>%
  group_by(year, indicator_id) %>%
  summarize(n_countries = n()) %>%
  filter(n_countries > 100) %>%
  left_join(hdi) %>%
  select(-n_countries) %>%
  mutate(name = indicator_name %>%
           str_remove_all(" ?\\([A-Z]{1,}\\)") %>%
           str_remove_all(" \\(years\\)") %>%
           str_replace_all(c("Adolescent birth.*" = "Teen_Birth_Rate",
                             "Coefficient of human inequality" = "Coef_Human_Inequality",
                             "Domestic credit.*" = "Finance_Sector_Domestic_Credit_PctGDP",
                             "Education index" = "Idx_Education",
                             "Employment in agricul.*" = "Empl_Agriculture",
                             "Employment in services.*" = "Empl_Services",
                             "Employment to population.*" = "Empl_Pop_Ratio",
                             "Estimated gross national income per capita, (.*) \\(.*$" = "GNIpc_\\1",
                             "Expected years of schooling, ([a-z]{1,}).*" = "School_Expected_Years_\\1",
                             "Exports and imports.*" = "Exports_Imports_PctGDP",
                             "Foreign direct investment.*" = "Foreign_Direct_Investment_PctGDP",
                             "Gender (.*) Index.*" = "Idx_Gender_\\1",
                             "Gross capital.*" = "Gross_Capital_PctGDP",
                             "Gross domestic product.*" = "GDP",
                             "GDP per capita.*" = "GDP_pc",
                             "Gross fixed capital.*" = "Gross_Fixed_Capital_PctGDP",
                             "Gross national income.*" = "Gross_National_Income",
                             "HDI rank" = "HDI_rank",
                             "Human Development Index,? ?(.*)?" = "Idx_Human_Development_\\1",
                             "Income index" = "Idx_Income",
                             "[Ll]ife expectancy" = "Life_Exp",
                             "Inequality-adjusted ([A-z]*) .?(?:IHDI|index).*" = "Idx_IneqAdj_\\1",
                             "Inequality in ([A-z]*).*" = "Ineq_\\1",
                             "Infants lacking immunization, ([A-z]*).*" = "Infants_Missing_\\1_Vaccine",
                             "Labour force participation rate .. ages 15 and older.,? ?(.*)?" = "Labor_Participation_\\1",
                             "Life_Exp at birth,? ?(.*) ?.*" = "Life_Exp_Birth_\\1",
                             "Mean years of schooling,? ?([A-z]*).*" = "School_Mean_Years_\\1",
                             "Old-age.* dependency.*" = "Elderly_Dependency_Pct",
                             "Overall loss in HDI.*" = "Ineq_HDI_Loss",
                             "Population ages ([\\d-–]*).*\\(millions\\)" = "Pop_\\1",
                             "Population under age 5.*" = "Pop_under5",
                             "Private capital flows.*" = "Private_capital_flows_PctGDP",
                             "Red List Index.*" = "Idx_Red_List",
                             "Refugees by country of origin.*" = "Refugees_from_country_1000s",
                             "Remittances, inflows.*" = "Remittances_inflows_PctGDP",
                             "Sex ratio at birth.*" = "MtoF_Ratio_birth",
                             "Share of employment in nonagriculture, female.*" = "Empl_Nonagriculture_Pct_Female",
                             "Share of seats in parliament.*" = "Parlaiment_Pct_Female",
                             "Total population.*" = "Pop_Total",
                             "Unemployment, (total|youth).*" = "Unemployment_\\1",
                             "Urban population" = "Pop_Urban_Pct",
                             "Vulnerable employment.*" = "Empl_Vulnerable_Pct",
                             "Women with account at .*" = "Female_Bank_Acct_Pct",
                             "Working poor at .*" = "Working_Poor_Pct_3.10day",
                             "Young age.* dependency ratio.*" = "Young_Dependency_Pct",
                             "Youth unemployment rate.*" = "Youth_Unemployment_FtoM")
           ) %>%
           str_remove_all("[_ ]{1,}$") %>%
           str_replace_all("[, ]{1,}", "_")) %>%
  select(-indicator_id, -indicator_name) %>%
  tidyr::spread(key = name, value = value)

write_csv(hdi_2017, path = "ds303_2019/2017_Human_Dev_Index.csv")

qplot(`Human Development Index, female`, `Human Development Index, male`, data = hdi_2017) +
  geom_abline(slope = 1, intercept = 0) +
  coord_fixed()

# ------------------------------------------------------------------------------

# --- World Happiness Index ----------------------------------------------------
url <- "https://countryeconomy.com/demography/world-happiness-index"

world_happiness_index <- rvest::html_table(xml2::read_html(url)) %>%
  `[[`(1) %>%
  as_tibble(.name_repair = "unique") %>%
  select(1:3) %>%
  mutate(Country = str_remove_all(Countries, " [[:punct:]]{1,}"),
         Happiness_Rank = str_remove_all(`World Happiness Ranking`, "\\D") %>% parse_number(),
         Happiness_Index = `World Happiness Index...3`) %>%
  select(-matches(" "), -Countries)


# ------------------------------------------------------------------------------
