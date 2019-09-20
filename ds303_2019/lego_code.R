library(tidyverse)
library(LegoR)

lego_sets <- read_csv("ds303_2019/rebrickable-lego-sets.csv")
lego_themes <- read_csv("ds303_2019/rebrickable-lego-themes.csv") %>%
  set_names(c("theme_id", "theme_name", "theme_parent"))

lego_sets <- left_join(lego_sets, lego_themes, by = "theme_id")

if (exists(".brickset_key") & exists(".brickset_hash")) {
  brickset_get_sets(theme = "Architecture", pageSize = "10")
}

sets_2019 <- brickset_get_sets(year = "2019", pageSize = 1000)
sets_2018 <- brickset_get_sets(year = "2018", pageSize = 1000)
sets_2017 <- brickset_get_sets(year = "2017", pageSize = 1000)
sets_2016 <- brickset_get_sets(year = "2016", pageSize = 1000)
sets_2015 <- brickset_get_sets(year = "2015", pageSize = 1000)

sets <- purrr::map(2000:2019, ~brickset_get_sets(year = ., pageSize = 900))

sets_df <- do.call("bind_rows", sets) %>%
  bind_rows(sets_2019, sets_2018, sets_2017, sets_2016, sets_2015)

brickset_sets <- sets_df %>%
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

write_csv(brickset_sets, "ds303_2019/lego-prices.csv")

ggplot(brickset_sets, aes(x = pieces, y = usretailprice, color = broad_theme)) + geom_point() +
  geom_smooth(method = "lm")

write_csv(sets_df, "ds303_2019/brickset-lego-sets.csv")

ggplot(brickset_sets, aes(x = minifigs, y = usretailprice, color = broad_theme)) + geom_point() +
  geom_smooth(method = "lm")
