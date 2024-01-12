# load packages -----------------------------------------------------------

## Package names
packages <- c("tidyverse","ggplot2","dplyr","hexbin",
              "data.table","geojsonio","RColorBrewer","rgdal",
              "broom","rgeos","maptools","viridis")

## Install packages not yet installed
installed_packages <- packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(packages[!installed_packages])
}

## Packages loading
invisible(lapply(packages, library, character.only = TRUE))

setwd('/Users/rpark/Desktop/Research/2. Fetal Maternal Mortality/FetalMaternalMortality')

# clean datafiles ---------------------------------------------------------

clean_df <- function(fname) {
  df <- read.csv(str_interp('data/${fname}.txt'), sep='\t') %>%
    filter((Notes != 'Total')) %>%
    subset(select = -c(Notes, Population, Crude.Rate)) %>%
    na.omit() 
}

df_inf1 <- clean_df('infant_mortality_census_00_10')
df_inf2a <- clean_df('infant_mortality_census_11_17')
df_inf2b <- clean_df('infant_mortality_census_18_19') %>%
  rename(
    Census.Region = Residence.Census.Region,
    Census.Region.Code = Residence.Census.Region.Code
  )
df_inf_covid <- clean_df('infant_mortality_census_20_22') %>%
  rename(
    Census.Region = Residence.Census.Region,
    Census.Region.Code = Residence.Census.Region.Code
  )

concat_df <- function(df2a, df2b) {
  df <- rbind(df2a, df2b) %>%
    group_by(Census.Region, Census.Region.Code) %>%
    summarise(Deaths=sum(Deaths))
}

df_inf2 <- concat_df(df_inf2a, df_inf2b)

clean_nat_df <- function(fname) {
  df_nat <- read.csv(str_interp('data/${fname}.txt'), sep='\t') %>%
    filter((Notes != 'Total')) %>%
    subset(select = -c(Notes)) %>%
    na.omit() %>%
    group_by(Year, Census.Region, Census.Region.Code) %>%
    summarise(Births=sum(Births))
}

df_nat1 <- clean_nat_df('natality_age_year_census_00_02') 
df_nat2 <- clean_nat_df('natality_age_year_census_03_06') 
df_nat3 <- clean_nat_df('natality_age_year_census_07_22') 

df_nat <- rbind(df_nat1, df_nat2, df_nat3)
df_nat_00_10 <- df_nat %>% 
  filter(Year < 2011) %>%
  group_by(Census.Region, Census.Region.Code) %>%
  summarise(Births=sum(Births))
df_nat_11_19 <- df_nat %>% 
  filter((Year > 2010) & (Year < 2020)) %>%
  group_by(Census.Region, Census.Region.Code) %>%
  summarise(Births=sum(Births))
df_nat_covid <- df_nat %>% 
  filter(Year > 2019) %>%
  group_by(Census.Region, Census.Region.Code) %>%
  summarise(Births=sum(Births))

mrg_births <- function(df_mort, df_nat) {
  df_mort <- merge(df_mort, df_nat, 
                  by=c('Census.Region',
                       'Census.Region.Code'))
}

df_inf1 <- mrg_births(df_inf1, df_nat_00_10)
df_inf2 <- mrg_births(df_inf2, df_nat_11_19)
df_inf_covid <- mrg_births(df_inf_covid, df_nat_covid)

create_crude <- function(df, rate, label) {
  df_new <- df %>% group_by(Census.Region) %>% 
    summarise(Deaths=sum(Deaths), Births=sum(Births)) %>%
    mutate({{rate}} := Deaths/Births*1000) %>%
    mutate(Type := {{label}}) %>%
    subset(select = -c(Births, Deaths))
}

df_crude_inf1 <- create_crude(df_inf1, Rate.00.10, 'Infant')
df_crude_inf2 <- create_crude(df_inf2, Rate.11.19, 'Infant')
df_crude_inf <- merge(df_crude_inf1, df_crude_inf2, by=c('Census.Region', 'Type'))

df_crude_inf_covid <- create_crude(df_inf_covid, Rate.Covid, 'Infant')
df_crude_inf_covid_chg <- merge(df_crude_inf2, df_crude_inf_covid, by=c('Census.Region', 'Type'))

df_crude_inf$Pct.Change = (df_crude_inf$Rate.11.19-df_crude_inf$Rate.00.10)/df_crude_inf$Rate.00.10*100
df_crude_inf_covid_chg$Pct.Change = (df_crude_inf_covid_chg$Rate.Covid-df_crude_inf_covid_chg$Rate.11.19)/df_crude_inf_covid_chg$Rate.11.19*100

library(stringr)

df_crude_inf %>%
  mutate(Region = word(Census.Region,-1)) %>%
  ggplot(aes(x=Region, y=Pct.Change)) +
  geom_bar(stat="identity", position=position_dodge(), fill="red") +
  theme_minimal() + 
  labs(y = "% Change in Rates per 1,000 Live Births", 
       x = "Census Region",
       title = "Percent Change in Rates of Infant Deaths",
       subtitle = "2000-2010 vs. 2011-2019") + 
  theme(plot.caption=element_text(hjust = 0)) + guides(fill=guide_legend(title="")) +
  theme(axis.text.x = element_text(angle = 80))
ggsave("figs/plt_pct_chg_inf_census_crude.png")

df_crude_inf_covid_chg %>%
  mutate(Region = word(Census.Region,-1)) %>%
  ggplot(aes(x=Region, y=Pct.Change)) +
  geom_bar(stat="identity", position=position_dodge(), fill="red") +
  theme_minimal() + 
  labs(y = "% Change in Rates per 1,000 Live Births", 
       x = "Census Region",
       title = "Percent Change in Rates of Infant Deaths",
       subtitle = "2011-2019 vs. 2020-2022") + 
  theme(plot.caption=element_text(hjust = 0)) + guides(fill=guide_legend(title="")) +
  theme(axis.text.x = element_text(angle = 80, vjust=.75))
ggsave("figs/plt_pct_chg_inf_census_crude_covid.png")
