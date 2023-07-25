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

df_mm1 <- clean_df('maternal_mortality_race_00_10')
df_mm2 <- clean_df('maternal_mortality_race_11_19')
df_mm_covid <- clean_df('maternal_mortality_race_20_21') %>%
  rename(Race = Single.Race.6, Race.Code = Single.Race.6.Code)

df_mmno1 <- clean_df('maternal_mortality_spec_race_00_10')
df_mmno2 <- clean_df('maternal_mortality_spec_race_11_19')
df_mmno_covid <- clean_df('maternal_mortality_spec_race_20_21') %>%
  rename(Race = Single.Race.6, Race.Code = Single.Race.6.Code)

df_mmot1 <- clean_df('other_mat_mortality_race_00_10')
df_mmot2 <- clean_df('other_mat_mortality_race_11_19')
df_mmot_covid <- clean_df('other_mat_mortality_race_20_21') %>%
  rename(Race = Single.Race.6, Race.Code = Single.Race.6.Code)

df_pr1 <- clean_df('pregrel_mortality_race_00_10')
df_pr2 <- clean_df('pregrel_mortality_race_11_19')
df_pr_covid <- clean_df('pregrel_mortality_race_20_21') %>%
  rename(Race = Single.Race.6, Race.Code = Single.Race.6.Code)

load(file="data/natality_race_00_10_clean.Rda")
load(file="data/natality_race_11_19_clean.Rda")
load(file="data/natality_race_20_21_clean.Rda")

mrg_births <- function(df_mort, df_nat) {
  df_mort[(df_mort$Hispanic.Origin == 'Hispanic or Latino') & 
            (df_mort$Race == 'Black or African American'), 'Race'] = 'Hispanic'
  df_mort[(df_mort$Hispanic.Origin == 'Not Hispanic or Latino') & 
            (df_mort$Race == 'Black or African American'), 'Race'] = 'Non-Hispanic Black'
  df_mort[(df_mort$Hispanic.Origin == 'Hispanic or Latino') & 
            (df_mort$Race == 'White'), 'Race'] = 'Hispanic'
  df_mort[(df_mort$Hispanic.Origin == 'Hispanic or Latino') & 
            (df_mort$Race == 'Black or African American'), 'Race'] = 'Hispanic'
  df_mort[(df_mort$Hispanic.Origin == 'Not Hispanic or Latino') & 
            (df_mort$Race == 'White'), 'Race'] = 'Non-Hispanic White'
  df_mort[(df_mort$Hispanic.Origin == 'Not Stated') & 
            ((df_mort$Race == 'White') | 
               (df_mort$Race == 'Black or African American')), 'Race'] = ''
  df_mort[(df_mort$Race == 'American Indian or Alaska Native'), 
          'Race'] = 'Native American/Alaskan'
  df_mort[(df_mort$Race == 'Native Hawaiian or Other Pacific Islander') | 
            (df_mort$Race == 'Asian'), 'Race'] = 'Asian American or Pacific Islander'
  df_mort <- df_mort %>% filter(Race != '') %>%
    group_by(Race) %>% summarise(Deaths=sum(Deaths)) %>%
    rename(Mother.s.Race = Race)
  
  df_mort_cln_race <- merge(df_mort, df_nat, 
                  by=c('Mother.s.Race'))
}

df_mm1 <- mrg_births(df_mm1, df_nat_race_00_10)
df_mm2 <- mrg_births(df_mm2, df_nat_race_11_19)
df_mm_covid <- mrg_births(df_mm_covid, df_nat_race_20_21)

df_mmno1 <- mrg_births(df_mmno1, df_nat_race_00_10)
df_mmno2 <- mrg_births(df_mmno2, df_nat_race_11_19)
df_mmno_covid <- mrg_births(df_mmno_covid, df_nat_race_20_21)

df_mmot1 <- mrg_births(df_mmot1, df_nat_race_00_10)
df_mmot2 <- mrg_births(df_mmot2, df_nat_race_11_19)
df_mmot_covid <- mrg_births(df_mmot_covid, df_nat_race_20_21)

df_pr1 <- mrg_births(df_pr1, df_nat_race_00_10)
df_pr2 <- mrg_births(df_pr2, df_nat_race_11_19)
df_pr_covid <- mrg_births(df_pr_covid, df_nat_race_20_21)

create_crude <- function(df, rate, label) {
  df_new <- df %>% 
    mutate({{rate}} := Deaths/Births*100000) %>%
    mutate(Type := {{label}}) %>%
    subset(select = -c(Births, Deaths))
}

df_crude_mm1 <- create_crude(df_mm1, Rate.00.10, 'Maternal Mortality')
df_crude_mm2 <- create_crude(df_mm2, Rate.11.19, 'Maternal Mortality')
df_crude_mm <- merge(df_crude_mm1, df_crude_mm2, by=c('Mother.s.Race', 'Type'))

df_crude_mm_covid <- create_crude(df_mm_covid, Rate.Covid, 'Maternal Mortality')
df_crude_mm_covid_chg <- merge(df_crude_mm2, df_crude_mm_covid, by=c('Mother.s.Race', 'Type'))

df_crude_mmno1 <- create_crude(df_mmno1, Rate.00.10, 'Maternal Mortality (Excl. Other)')
df_crude_mmno2 <- create_crude(df_mmno2, Rate.11.19, 'Maternal Mortality (Excl. Other)')
df_crude_mmno <- merge(df_crude_mmno1, df_crude_mmno2, by=c('Mother.s.Race', 'Type'))

df_crude_mmno_covid <- create_crude(df_mmno_covid, Rate.Covid, 'Maternal Mortality (Excl. Other)')
df_crude_mmno_covid_chg <- merge(df_crude_mmno2, df_crude_mmno_covid, by=c('Mother.s.Race', 'Type'))

df_crude_mmot1 <- create_crude(df_mmot1, Rate.00.10, 'Other Maternal Mortality')
df_crude_mmot2 <- create_crude(df_mmot2, Rate.11.19, 'Other Maternal Mortality')
df_crude_mmot <- merge(df_crude_mmot1, df_crude_mmot2, by=c('Mother.s.Race', 'Type'))

df_crude_mmot_covid <- create_crude(df_mmot_covid, Rate.Covid, 'Other Maternal Mortality')
df_crude_mmot_covid_chg <- merge(df_crude_mmot2, df_crude_mmot_covid, by=c('Mother.s.Race', 'Type'))

df_crude_pr1 <- create_crude(df_pr1, Rate.00.10, 'Pregnancy-Related Mortality')
df_crude_pr2 <- create_crude(df_pr2, Rate.11.19, 'Pregnancy-Related Mortality')
df_crude_pr <- merge(df_crude_pr1, df_crude_pr2, by=c('Mother.s.Race', 'Type'))

df_crude_pr_covid <- create_crude(df_pr_covid, Rate.Covid, 'Pregnancy-Related Mortality')
df_crude_pr_covid_chg <- merge(df_crude_pr2, df_crude_pr_covid, by=c('Mother.s.Race', 'Type'))

df_crude <- rbind(df_crude_mm, df_crude_mmno, df_crude_mmot, df_crude_pr)
df_crude$Pct.Change = (df_crude$Rate.11.19-df_crude$Rate.00.10)/df_crude$Rate.00.10*100

df_crude_covid <- rbind(df_crude_mm_covid_chg, df_crude_mmno_covid_chg, df_crude_mmot_covid_chg, df_crude_pr_covid_chg)
df_crude_covid$Pct.Change = (df_crude_covid$Rate.Covid-df_crude_covid$Rate.11.19)/df_crude_covid$Rate.11.19*100

df_crude %>%
  ggplot(aes(x=Mother.s.Race, y=Pct.Change, fill=Type)) +
  geom_bar(stat="identity", position=position_dodge()) +
  theme_minimal() + 
  labs(y = "% Change in Rates per 100,000 Live Births", 
       x = "Racial/Ethnic Group",
       title = "Percent Change in Rates of Maternal and Pregnancy-Related Deaths",
       subtitle = "2000-2010 vs. 2011-2019") + 
  theme(plot.caption=element_text(hjust = 0)) + guides(fill=guide_legend(title="")) +
  #theme(axis.text.x = element_text(angle = 60, vjust=0.6)) 
  coord_flip()
ggsave("figs/plt_census_pct_change_race_crude.png")

df_crude_covid %>%
  ggplot(aes(x=Mother.s.Race, y=Pct.Change, fill=Type)) +
  geom_bar(stat="identity", position=position_dodge()) +
  theme_minimal() + 
  labs(y = "% Change in Rates per 100,000 Live Births", 
       x = "Racial/Ethnic Group",
       title = "Percent Change in Rates of Maternal and Pregnancy-Related Deaths",
       subtitle = "2011-2019 vs. 2020-2021") + 
  theme(plot.caption=element_text(hjust = 0)) + guides(fill=guide_legend(title="")) +
  #theme(axis.text.x = element_text(angle = 80, vjust=.75))
  coord_flip()
ggsave("figs/plt_census_pct_change_race_crude_covid.png")
