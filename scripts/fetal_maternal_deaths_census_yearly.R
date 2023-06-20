# load packages -----------------------------------------------------------

## Package names
packages <- c("tidyverse", "datasets","papaja" ,"here")

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
df_fetal <- read.csv('data/fetal_deaths_2018_2021_census_yearly.txt', sep = "\t")
df_maternal <- read.csv('data/maternal_deaths_2018_2021_census_yearly.txt', sep = "\t")

## Drop columns not needed
df_fetal2 = subset(df_fetal, select = -c(Notes, Year.Code, 
                                         Mother.s.Single.Race.6.Code, 
                                         Age.of.Mother.9))
df_maternal2 = subset(df_maternal, select = -c(Notes, Year.Code, Crude.Rate,
                                               Single.Race.6.Code, Population,
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
  arrange(Year)

df_maternal3 <- df_maternal2 %>% 
  na.omit() %>%
  rename(
    Race = Single.Race.6,
    Age.Group = Five.Year.Age.Groups.Code,
    Maternal.Deaths = Deaths 
  ) %>%
  arrange(Year)

## Merge two dataframes
df_mat_fet <- merge(df_maternal3, df_fetal3, 
                    by = c('Census.Region','Census.Region.Code','Race','Age.Group','Year'))

# group data -----------------------------------------------------------------

## Fetal 
agg_fetal_race <- df_fetal3 %>% group_by(Year, Race) %>% 
  summarise(Sum.Fetal.Deaths=sum(Fetal.Deaths))

agg_fetal_age <- df_fetal3 %>% group_by(Year, Age.Group) %>% 
  summarise(Sum.Fetal.Deaths=sum(Fetal.Deaths))

agg_fetal_region <- df_fetal3 %>% group_by(Year, Census.Region.Code) %>% 
  summarise(Sum.Fetal.Deaths=sum(Fetal.Deaths))

agg_fetal_year <- df_fetal3 %>% group_by(Year) %>% 
  summarise(Sum.Fetal.Deaths=sum(Fetal.Deaths))

## Maternal 
agg_maternal_race <- df_maternal3 %>% group_by(Year, Race) %>% 
  summarise(Sum.Maternal.Deaths=sum(Maternal.Deaths))

agg_maternal_age <- df_maternal3 %>% group_by(Year, Age.Group) %>% 
  summarise(Sum.Maternal.Deaths=sum(Maternal.Deaths))

agg_maternal_region <- df_maternal3 %>% group_by(Year, Census.Region.Code) %>% 
  summarise(Sum.Maternal.Deaths=sum(Maternal.Deaths))

agg_maternal_year <- df_maternal3 %>% group_by(Year) %>% 
  summarise(Sum.Maternal.Deaths=sum(Maternal.Deaths))

## Both
agg_maternal_fetal <- df_mat_fet %>% group_by(Year) %>%
  summarise(Sum.Maternal.Deaths=sum(Maternal.Deaths),
            Sum.Fetal.Deaths=sum(Fetal.Deaths))

# plot data -----------------------------------------------------------------

## Fetal 
agg_fetal_race %>%
  ggplot(aes(x=Year, y=Sum.Fetal.Deaths, group=Race, color=Race)) +
  geom_line()
ggsave("figs/region_year/agg_fetal_race.png")

agg_fetal_age %>%
  ggplot(aes(x=Year, y=Sum.Fetal.Deaths, group=Age.Group, color=Age.Group)) +
  geom_line()
ggsave("figs/region_year/agg_fetal_age.png")

agg_fetal_region %>%
  ggplot(aes(x=Year, y=Sum.Fetal.Deaths, group=Census.Region.Code, color=Census.Region.Code)) +
  geom_line()
ggsave("figs/region_year/agg_fetal_region.png")

agg_fetal_year %>%
  ggplot(aes(x=Year, y=Sum.Fetal.Deaths)) +
  geom_line()
ggsave("figs/region_year/agg_fetal_year.png")

## Maternal 
agg_maternal_race %>%
  ggplot(aes(x=Year, y=Sum.Maternal.Deaths, group=Race, color=Race)) +
  geom_line()
ggsave("figs/region_year/agg_maternal_race.png")

agg_maternal_age %>%
  ggplot(aes(x=Year, y=Sum.Maternal.Deaths, group=Age.Group, color=Age.Group)) +
  geom_line()
ggsave("figs/region_year/agg_maternal_age.png")

agg_maternal_age %>%
  ggplot(aes(x=Year, y=Sum.Maternal.Deaths, group=Census.Region.Code, color=Census.Region.Code)) +
  geom_line()
ggsave("figs/region_year/agg_maternal_region.png")

agg_maternal_year %>%
  ggplot(aes(x=Year, y=Sum.Maternal.Deaths)) +
  geom_line()
ggsave("figs/region_year/agg_maternal_year.png")

