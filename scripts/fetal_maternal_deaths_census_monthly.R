# load packages -----------------------------------------------------------

## Package names
packages <- c("tidyverse", "datasets","papaja" ,"here", "anytime")

## Install packages not yet installed
installed_packages <- packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(packages[!installed_packages])
}

## Packages loading
invisible(lapply(packages, library, character.only = TRUE))
library(ggplot2)
library(dplyr)

setwd('/Users/rpark/Desktop/Research/2. Excess Mortality/FetalMaternalMortality')

# clean datafiles ---------------------------------------------------------

## Import datafiles 
df_fetal <- read.csv('data/fetal_deaths_2018_2021_census_monthly.txt', sep = "\t")
df_maternal <- read.csv('data/maternal_deaths_2018_2021_census_monthly.txt', sep = "\t")

## Drop columns not needed
df_fetal2 = subset(df_fetal, select = -c(Notes, Year.Code, 
                                         Mother.s.Single.Race.6.Code, 
                                         Age.of.Mother.9))
df_maternal2 = subset(df_maternal, select = -c(Notes, Year.Code, Crude.Rate,
                                               Single.Race.6.Code,
                                               Five.Year.Age.Groups))

## Omit NA & rename columns
df_fetal3 <- df_fetal2 %>% 
  na.omit() %>%
  rename(
    Census.Region = Standard.Residence.Census.Region,
    Census.Region.Code = Standard.Residence.Census.Region.Code,
    Race = Mother.s.Single.Race.6,
    Age.Group = Age.of.Mother.9.Code
  ) %>%
  arrange(Month.Code, Year) %>% 
  unite(col='Year.Month', c('Year', 'Month.Code'), sep='-')

df_fetal3$Year.Month <- as.Date(anytime(df_fetal3$Year.Month))

df_maternal3 <- df_maternal2 %>% 
  na.omit() %>%
  rename(
    Race = Single.Race.6,
    Age.Group = Five.Year.Age.Groups.Code,
    Maternal.Deaths = Deaths 
  ) %>%
  arrange(Month.Code, Year)

df_maternal3$Month.Code <- as.Date(anytime(df_maternal3$Month.Code))

# group data -----------------------------------------------------------------

## Fetal 
agg_fetal_race <- df_fetal3 %>% group_by(Year.Month, Race) %>% 
  summarise(Sum.Fetal.Deaths=sum(Fetal.Deaths))

agg_fetal_age <- df_fetal3 %>% group_by(Year.Month, Age.Group) %>% 
  summarise(Sum.Fetal.Deaths=sum(Fetal.Deaths))

agg_fetal_region <- df_fetal3 %>% group_by(Year.Month, Census.Region.Code) %>% 
  summarise(Sum.Fetal.Deaths=sum(Fetal.Deaths))

agg_fetal_year <- df_fetal3 %>% group_by(Year.Month) %>% 
  summarise(Sum.Fetal.Deaths=sum(Fetal.Deaths))

## Maternal 
agg_maternal_race <- df_maternal3 %>% group_by(Month.Code, Race) %>% 
  summarise(Sum.Maternal.Deaths=sum(Maternal.Deaths))

agg_maternal_age <- df_maternal3 %>% group_by(Month.Code, Age.Group) %>% 
  summarise(Sum.Maternal.Deaths=sum(Maternal.Deaths))

agg_maternal_region <- df_maternal3 %>% group_by(Month.Code, Census.Region.Code) %>% 
  summarise(Sum.Maternal.Deaths=sum(Maternal.Deaths))

agg_maternal_year <- df_maternal3 %>% group_by(Month.Code) %>% 
  summarise(Sum.Maternal.Deaths=sum(Maternal.Deaths))

# plot data -----------------------------------------------------------------

## Fetal 
agg_fetal_race %>%
  ggplot(aes(x=Year.Month, y=Sum.Fetal.Deaths, group=Race, color=Race)) +
  geom_line() + scale_x_date(date_labels="%Y-%b",date_breaks="1 year")
ggsave("figs/region_month/agg_fetal_race_monthly.png")

agg_fetal_age %>%
  ggplot(aes(x=Year.Month, y=Sum.Fetal.Deaths, group=Age.Group, color=Age.Group)) +
  geom_line() + scale_x_date(date_labels="%Y-%b",date_breaks="1 year")
ggsave("figs/region_month/agg_fetal_age_monthly.png")

agg_fetal_region %>%
  ggplot(aes(x=Year.Month, y=Sum.Fetal.Deaths, group=Census.Region.Code, color=Census.Region.Code)) +
  geom_line() + scale_x_date(date_labels="%Y-%b",date_breaks="1 year")
ggsave("figs/region_month/agg_fetal_region_monthly.png")

agg_fetal_year %>%
  ggplot(aes(x=Year.Month, y=Sum.Fetal.Deaths)) +
  geom_line() + scale_x_date(date_labels="%Y-%b",date_breaks="1 year")
ggsave("figs/region_month/agg_fetal_monthly.png")

## Maternal 
agg_maternal_race %>%
  ggplot(aes(x=Month.Code, y=Sum.Maternal.Deaths, group=Race, color=Race)) +
  geom_line() + scale_x_date(date_labels="%Y-%b",date_breaks="1 year")
ggsave("figs/region_month/agg_maternal_race_monthly.png")

agg_maternal_age %>%
  ggplot(aes(x=Month.Code, y=Sum.Maternal.Deaths, group=Age.Group, color=Age.Group)) +
  geom_line() + scale_x_date(date_labels="%Y-%b",date_breaks="1 year")
ggsave("figs/region_month/agg_maternal_age_monthly.png")

agg_maternal_region %>%
  ggplot(aes(x=Month.Code, y=Sum.Maternal.Deaths, group=Census.Region.Code, color=Census.Region.Code)) +
  geom_line() + scale_x_date(date_labels="%Y-%b",date_breaks="1 year")
ggsave("figs/region_month/agg_maternal_region_monthly.png")

agg_maternal_year %>%
  ggplot(aes(x=Month.Code, y=Sum.Maternal.Deaths)) +
  geom_line()
ggsave("figs/region_month/agg_maternal_monthly.png")

