# load libraries
library(ggplot2)
library(tidyverse)
library(visdat)
library(viridis)
# tuesdata <- tidytuesdayR::tt_load('2021-02-23')
# employed <- tuesdata$employed
# employed %>% View
# employed %>% visdat::vis
# Read the data and explore classand missingness
earn <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-02-23/earn.csv")
# lear
visdat::vis_dat(earn)
vis_miss(earn)

earn %>%
  filter(age %in% 
           c("16 years and over", 
             "25 years and over",
             "55 years and over",
             "65 years and over")) %>% 
  filter(sex != "Both Sexes") %>%
  ggplot(aes(
    x = as.character(year),
    y = median_weekly_earn,
    colour = sex
  )) +
  geom_boxplot()
 
# A: females and males both have higher weekly earning over years and females have had lower weekly income than males consistently over years. Further, the variability is more and increasing for men at a higher rate than female implying a more wealth gap over years.


# Q: is the same for all races?

# How are the gender pay gaps for different races?
earn %>%
  filter(sex != "Both Sexes") %>%
  ggplot(aes(x = as.character(year), y = median_weekly_earn, colour = sex, group = sex)) +
  stat_summary(fun = median, geom = "line") +
  facet_wrap(~race) +
  scale_color_brewer(palette = "Dark2")

# A: For whites, geneder gap has been amost constant over time. Seems like for African American people, gender gap is the lowest and for Asian it is the highest.


# Q: What is the % gap compared to female's earning for each races?

earn_tbl <- earn %>%
  ungroup() %>%
  filter(sex != "Both Sexes") %>%
  group_by(year, sex, race) %>%
  mutate(median_earn = median(median_weekly_earn)) %>%
  select(sex, year, median_earn, race) %>%
  unique() %>%
  pivot_wider(
    names_from = sex,
    values_from = median_earn
  ) %>%
  mutate(diff = (Men - Women) * 100 / Women)


earn_tbl %>%
  ggplot() +
  geom_line(aes(x = year, y = diff, color = race)) +
  scale_x_continuous(breaks = seq(2010, 2020, 1)) +
  ylab("% gender gap") +
  scale_color_viridis(discrete = TRUE) +
  theme_void() +
  theme_dark() +
  theme(legend.position = "bottom")

# A: For whites, the percentage gender gap has been constantly staying at aroiund $20%$ over years. For African American it is increasing from 10% to 15% from 2014 to 2020. For Asian people, this pattern is just the opposite where the percentage gender gap is decreasing from 2012 to 2016 and then increasing after 2016. This could be a result of the political situation back in the US, when during Obama's presidential term, the gender gap was decreasing and then again started increasing during the presidency of Trump.