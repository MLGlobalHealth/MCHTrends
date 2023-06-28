# load packages -----------------------------------------------------------

## Package names
packages <- c("tidyverse","ggplot2","dplyr","shiny")

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
  df <- read.csv(str_interp('data/${fname}.txt'), sep = "\t")
  subset(df, select = -c(Notes, Fertility.Rate, Female.Population)) %>%
  na.omit()
}

df_nat_year1 <- clean_df('natality_yearly_05_06')
df_nat_year2 <- clean_df('natality_yearly_07_21')
df_nat_year <- rbind(df_nat_year1, df_nat_year2) 
save(df_nat_year, file="data/natality_yearly_clean.Rda")

df_nat_age1 <- clean_df('natality_age_05_06')
df_nat_age2 <- clean_df('natality_age_07_21')
df_nat_age <- rbind(df_nat_age1, df_nat_age2) %>%
  filter(Age.of.Mother.9 != "") %>%
  group_by(Age.of.Mother.9.Code) %>% 
  summarise(Births=sum(Births))
save(df_nat_age, file="data/natality_age_clean.Rda")

df_nat_state1 <- clean_df('natality_state_05_06')
df_nat_state2 <- clean_df('natality_state_07_21')
df_nat_state <- rbind(df_nat_state1, df_nat_state2) %>%
  group_by(State) %>%
  summarise(Births=sum(Births))
save(df_nat_state, file="data/natality_state_clean.Rda")

df_nat_race1 <- clean_df('natality_race_05_06')
df_nat_race2 <- clean_df('natality_race_07_21')

clean_race <- function(df1, df2, ...) {
  df_nat_race <- rbind(df1, df2) %>%
    filter(Mother.s.Hispanic.Origin != "") %>%
    mutate_at('Births', as.numeric)
  
  df_nat_race[(df_nat_race$Mother.s.Hispanic.Origin == 'Hispanic or Latino') &
                (df_nat_race$Mother.s.Bridged.Race == 'Black or African American'), 
              'Mother.s.Bridged.Race'] = 'Hispanic'
  
  df_nat_race[(df_nat_race$Mother.s.Hispanic.Origin == 'Not Hispanic or Latino') &
                (df_nat_race$Mother.s.Bridged.Race == 'Black or African American'), 
              'Mother.s.Bridged.Race'] = 'Non-Hispanic Black'
  
  df_nat_race[(df_nat_race$Mother.s.Hispanic.Origin == 'Hispanic or Latino') &
                (df_nat_race$Mother.s.Bridged.Race == 'White'), 
              'Mother.s.Bridged.Race'] = 'Hispanic'
  
  df_nat_race[(df_nat_race$Mother.s.Hispanic.Origin == 'Not Hispanic or Latino') &
                (df_nat_race$Mother.s.Bridged.Race == 'White'), 
              'Mother.s.Bridged.Race'] = 'Non-Hispanic White'
  
  df_nat_race[df_nat_race$Mother.s.Bridged.Race == 'American Indian or Alaska Native',
              'Mother.s.Bridged.Race'] = "Native American/Alaskan"
  
  df_nat_race[df_nat_race$Mother.s.Bridged.Race == 'Native Hawaiian or Other Pacific Islander',
              'Mother.s.Bridged.Race'] = "Native Hawaiian/Other PI"
  
  df_nat_race[(df_nat_race$Mother.s.Hispanic.Origin == 'Unknown or Not Stated') &
                (df_nat_race$Mother.s.Bridged.Race == 'Black or African American'), 
              'Mother.s.Bridged.Race'] = 'Unknown Black'
  
  df_nat_race[(df_nat_race$Mother.s.Hispanic.Origin == 'Unknown or Not Stated') &
                (df_nat_race$Mother.s.Bridged.Race == 'White'), 
              'Mother.s.Bridged.Race'] = 'Unknown White'
  
  df_nat_race <- df_nat_race %>% 
    mutate_at(c('Births'), as.numeric) %>%
    group_by(...) %>% 
    summarise(Births=sum(Births))
}
df_nat_race <- clean_race(df_nat_race1, df_nat_race2, Mother.s.Bridged.Race)
save(df_nat_race, file="data/natality_race_clean.Rda")

df_nat_race_age1 <- clean_df('natality_race_age_05_06') %>%
  filter(Age.of.Mother.9.Code != "")
df_nat_race_age2 <- clean_df('natality_race_age_07_21') %>%
  filter(Age.of.Mother.9.Code != "")
df_nat_race_age <- clean_race(df_nat_race_age1, df_nat_race_age2,
                              Mother.s.Bridged.Race, Age.of.Mother.9.Code)
save(df_nat_race_age, file="data/natality_race_age_clean.Rda")

df_nat_race_year1 <- clean_df('natality_race_yearly_05_06')
df_nat_race_year2 <- clean_df('natality_race_yearly_07_21')
df_nat_race_year <- clean_race(df_nat_race_year1, df_nat_race_year2,
                               Mother.s.Bridged.Race, Year)
save(df_nat_race_year, file="data/natality_race_year_clean.Rda")

