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

df_mm1 <- clean_df('maternal_mortality_census_00_10')
df_mm2a <- clean_df('maternal_mortality_census_11_17')
df_mm2b <- clean_df('maternal_mortality_census_18_19')
df_mm_covid <- clean_df('maternal_mortality_census_20_22') %>%
  rename(
    Census.Region = Residence.Census.Region,
    Census.Region.Code = Residence.Census.Region.Code
  )

df_mmno1 <- clean_df('maternal_mortality_spec_census_00_10')
df_mmno2a <- clean_df('maternal_mortality_spec_census_11_17')
df_mmno2b <- clean_df('maternal_mortality_spec_census_18_19')
df_mmno_covid <- clean_df('maternal_mortality_spec_census_20_22') %>%
  rename(
    Census.Region = Residence.Census.Region,
    Census.Region.Code = Residence.Census.Region.Code
  )

df_mmot1 <- clean_df('other_mat_mortality_census_00_10')
df_mmot2a <- clean_df('other_mat_mortality_census_11_17')
df_mmot2b <- clean_df('other_mat_mortality_census_18_19')
df_mmot_covid <- clean_df('other_mat_mortality_census_20_22') %>%
  rename(
    Census.Region = Residence.Census.Region,
    Census.Region.Code = Residence.Census.Region.Code
  )

df_pr1 <- clean_df('pregrel_mortality_census_00_10')
df_pr2a <- clean_df('pregrel_mortality_census_11_17')
df_pr2b <- clean_df('pregrel_mortality_census_18_19')
df_pr_covid <- clean_df('pregrel_mortality_census_20_22') %>%
  rename(
    Census.Region = Residence.Census.Region,
    Census.Region.Code = Residence.Census.Region.Code
  )

concat_df <- function(df2a, df2b) {
  df <- rbind(df2a, df2b) %>%
    group_by(Census.Region, Census.Region.Code) %>%
    summarise(Deaths=sum(Deaths))
}

df_mm2 <- concat_df(df_mm2a, df_mm2b)
df_mmno2 <- concat_df(df_mmno2a, df_mmno2b)
df_mmot2 <- concat_df(df_mmot2a, df_mmot2b)
df_pr2 <- concat_df(df_pr2a, df_pr2b)

clean_nat_df <- function(fname, age_var, age_var_code) {
  df_nat <- read.csv(str_interp('data/${fname}.txt'), sep='\t') %>%
    filter((Notes != 'Total')) %>%
    subset(select = -c(Notes)) %>%
    na.omit() %>% 
    rename(
      Five.Year.Age.Groups = {{age_var}},
      Five.Year.Age.Groups.Code = {{age_var_code}}
    ) %>%
    group_by(Census.Region, Census.Region.Code, Year) %>%
    summarise(Births=sum(Births))
}

df_nat1 <- clean_nat_df('natality_age_year_census_00_02', 
                        Age.of.Mother, Age.of.Mother.Code) 
df_nat2 <- clean_nat_df('natality_age_year_census_03_06', 
                        Age.of.Mother.9, Age.of.Mother.9.Code) 
df_nat3 <- clean_nat_df('natality_age_year_census_07_22', 
                        Age.of.Mother.9, Age.of.Mother.9.Code)

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

df_mm1 <- mrg_births(df_mm1, df_nat_00_10)
df_mm2 <- mrg_births(df_mm2, df_nat_11_19)
df_mm_covid <- mrg_births(df_mm_covid, df_nat_covid)

df_mmno1 <- mrg_births(df_mmno1, df_nat_00_10)
df_mmno2 <- mrg_births(df_mmno2, df_nat_11_19)
df_mmno_covid <- mrg_births(df_mmno_covid, df_nat_covid)

df_mmot1 <- mrg_births(df_mmot1, df_nat_00_10)
df_mmot2 <- mrg_births(df_mmot2, df_nat_11_19)
df_mmot_covid <- mrg_births(df_mmot_covid, df_nat_covid)

df_pr1 <- mrg_births(df_pr1, df_nat_00_10)
df_pr2 <- mrg_births(df_pr2, df_nat_11_19)
df_pr_covid <- mrg_births(df_pr_covid, df_nat_covid)

create_crude <- function(df, rate, label) {
  df_new <- df %>% group_by(Census.Region) %>% 
    summarise(Deaths=sum(Deaths), Births=sum(Births)) %>%
    mutate({{rate}} := Deaths/Births*100000) %>%
    mutate(Type := {{label}}) %>%
    subset(select = -c(Births, Deaths))
}

df_crude_mm1 <- create_crude(df_mm1, Rate.00.10, 'Maternal')
df_crude_mm2 <- create_crude(df_mm2, Rate.11.19, 'Maternal')
df_crude_mm <- merge(df_crude_mm1, df_crude_mm2, by=c('Census.Region', 'Type'))

df_crude_mm_covid <- create_crude(df_mm_covid, Rate.Covid, 'Maternal')
df_crude_mm_covid_chg <- merge(df_crude_mm2, df_crude_mm_covid, by=c('Census.Region', 'Type'))

df_crude_mmno1 <- create_crude(df_mmno1, Rate.00.10, 'Cause-Specific Maternal')
df_crude_mmno2 <- create_crude(df_mmno2, Rate.11.19, 'Cause-Specific Maternal')
df_crude_mmno <- merge(df_crude_mmno1, df_crude_mmno2, by=c('Census.Region', 'Type'))

df_crude_mmno_covid <- create_crude(df_mmno_covid, Rate.Covid, 'Cause-Specific Maternal')
df_crude_mmno_covid_chg <- merge(df_crude_mmno2, df_crude_mmno_covid, by=c('Census.Region', 'Type'))

df_crude_mmot1 <- create_crude(df_mmot1, Rate.00.10, 'Unspecified Maternal')
df_crude_mmot2 <- create_crude(df_mmot2, Rate.11.19, 'Unspecified Maternal')
df_crude_mmot <- merge(df_crude_mmot1, df_crude_mmot2, by=c('Census.Region', 'Type'))

df_crude_mmot_covid <- create_crude(df_mmot_covid, Rate.Covid, 'Unspecified Maternal')
df_crude_mmot_covid_chg <- merge(df_crude_mmot2, df_crude_mmot_covid, by=c('Census.Region', 'Type'))

df_crude_pr1 <- create_crude(df_pr1, Rate.00.10, 'Late Maternal')
df_crude_pr2 <- create_crude(df_pr2, Rate.11.19, 'Late Maternal')
df_crude_pr <- merge(df_crude_pr1, df_crude_pr2, by=c('Census.Region', 'Type'))

df_crude_pr_covid <- create_crude(df_pr_covid, Rate.Covid, 'Late Maternal')
df_crude_pr_covid_chg <- merge(df_crude_pr2, df_crude_pr_covid, by=c('Census.Region', 'Type'))

df_crude <- rbind(df_crude_mm, df_crude_mmno, df_crude_mmot, df_crude_pr)
df_crude$Pct.Change = (df_crude$Rate.11.19-df_crude$Rate.00.10)/df_crude$Rate.00.10*100

df_crude_covid <- rbind(df_crude_mm_covid_chg, df_crude_mmno_covid_chg, df_crude_mmot_covid_chg, df_crude_pr_covid_chg)
df_crude_covid$Pct.Change = (df_crude_covid$Rate.Covid-df_crude_covid$Rate.11.19)/df_crude_covid$Rate.11.19*100

library(stringr)

df_crude %>%
  mutate(Region = word(Census.Region,-1)) %>%
  ggplot(aes(x=Region, y=Pct.Change, fill=Type)) +
  geom_bar(stat="identity", position=position_dodge()) +
  theme_minimal() + 
  labs(y = "% Change in Rates per 100,000 Live Births", 
       x = "Census Region",
       title = "Percent Change in Rates of Maternal Deaths",
       subtitle = "2000-2010 vs. 2011-2019") + 
  theme(plot.caption=element_text(hjust = 0)) + guides(fill=guide_legend(title="")) +
  theme(axis.text.x = element_text(angle = 80))
ggsave("figs/plt_pct_chg_mat_census_crude.png")

df_crude_covid %>%
  mutate(Region = word(Census.Region,-1)) %>%
  ggplot(aes(x=Region, y=Pct.Change, fill=Type)) +
  geom_bar(stat="identity", position=position_dodge()) +
  theme_minimal() + 
  labs(y = "% Change in Rates per 100,000 Live Births", 
       x = "Census Region",
       title = "Percent Change in Rates of Maternal Deaths",
       subtitle = "2011-2019 vs. 2020-2022") + 
  theme(plot.caption=element_text(hjust = 0)) + guides(fill=guide_legend(title="")) +
  theme(axis.text.x = element_text(angle = 80, vjust=.75))
ggsave("figs/plt_pct_chg_mat_census_crude_covid.png")
