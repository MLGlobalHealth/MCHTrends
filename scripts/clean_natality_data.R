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

df_nat_race1a <- clean_df('natality_race_05_06')
df_nat_race1b <- clean_df('natality_race_07_15')
df_nat_race1 <- rbind(df_nat_race1a, df_nat_race1b)
df_nat_race2 <- clean_df('natality_race_16_21')

df_nat_race_age1a <- clean_df('natality_race_age_05_06') %>% filter(Age.of.Mother.9.Code != "")
df_nat_race_age1b <- clean_df('natality_race_age_07_15') %>% filter(Age.of.Mother.9.Code != "")
df_nat_race_age1 <- rbind(df_nat_race_age1a, df_nat_race_age1b)
df_nat_race_age2 <- clean_df('natality_race_age_16_21') %>% filter(Age.of.Mother.9.Code != "")

df_nat_race_year1a <- clean_df('natality_race_yearly_05_06')
df_nat_race_year1b <- clean_df('natality_race_yearly_07_15')
df_nat_race_year1 <- rbind(df_nat_race_year1a, df_nat_race_year1b)
df_nat_race_year2 <- clean_df('natality_race_yearly_16_21')

df_nat_momage <- clean_df('natality_momage_yearly') %>%
  filter(Age.of.Mother.9 != '')

save(df_nat_momage, file="data/natality_momage_year_clean.Rda")

df_nat_age_state_year <- clean_df('natality_age_state_year')
save(df_nat_age_state_year, file="data/natality_age_state_year_clean.Rda")

clean_df2 <- function(fname, varname) {
  df <- read.csv(str_interp('data/${fname}.txt'), sep = "\t")
  subset(df, select = -c(Notes, Month.Code)) %>%
    filter(Month != '') %>%
    na.omit() %>%
    rename({{varname}} := Births)
}

df_nat_month1 <- clean_df2('natality_monthly_18_19', Births.Pre)
df_nat_month2 <- clean_df2('natality_monthly_20_21', Births.Post)
df_nat_month <- merge(df_nat_month1, df_nat_month2, by='Month')
save(df_nat_month, file="data/natality_month_clean.Rda")

clean_bridged_race <- function(df, ...) {
  df_nat_race <- df %>% 
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
    summarise(Births=sum(Births)) %>%
    rename(Mother.s.Race = Mother.s.Bridged.Race)
}
  
df_nat_race1 <- clean_bridged_race(df_nat_race1, Mother.s.Bridged.Race)
df_nat_race_age1 <- clean_bridged_race(df_nat_race_age1, Mother.s.Bridged.Race, Age.of.Mother.9.Code)
df_nat_race_year1 <- clean_bridged_race(df_nat_race_year1, Mother.s.Bridged.Race, Year)

clean_single_race <- function(df, ...) {
  df2 = df %>% filter((Mother.s.Single.Race != '') & 
                      (Mother.s.Hispanic.Origin != "")) 
  
  df2[(df2$Mother.s.Hispanic.Origin == 'Hispanic or Latino') &
        (df2$Mother.s.Single.Race == 'Black or African American'), 
      'Mother.s.Single.Race'] = 'Hispanic'
  
  df2[(df2$Mother.s.Hispanic.Origin == 'Not Hispanic or Latino') &
        (df2$Mother.s.Single.Race == 'Black or African American'), 
      'Mother.s.Single.Race'] = 'Non-Hispanic Black'
  
  df2[(df2$Mother.s.Hispanic.Origin == 'Unknown or Not Stated') &
        ((df2$Mother.s.Single.Race == 'Black or African American')), 
      'Mother.s.Single.Race'] = 'Unknown Black'
  
  df2[(df2$Mother.s.Hispanic.Origin == 'Hispanic or Latino') &
        (df2$Mother.s.Single.Race == 'White'), 
      'Mother.s.Single.Race'] = 'Hispanic'
  
  df2[(df2$Mother.s.Hispanic.Origin == 'Not Hispanic or Latino') &
        (df2$Mother.s.Single.Race == 'White'), 
      'Mother.s.Single.Race'] = 'Non-Hispanic White'
  
  df2[(df2$Mother.s.Hispanic.Origin == 'Unknown or Not Stated') &
        ((df2$Mother.s.Single.Race == 'White')), 
      'Mother.s.Single.Race'] = 'Unknown White'
  
  df2[df2$Mother.s.Single.Race == 'American Indian or Alaska Native',
      'Mother.s.Single.Race'] = "Native American/Alaskan"
  
  df2[df2$Mother.s.Single.Race == 'Native Hawaiian or Other Pacific Islander',
      'Mother.s.Single.Race'] = "Native Hawaiian/Other PI"
  
  df2[df2$Mother.s.Single.Race == 'Asian',
      'Mother.s.Single.Race'] = "Asian or Pacific Islander"
  
  df2 <- df2 %>% 
    mutate_at('Births', as.numeric) %>%
    group_by(...) %>% 
    summarise(Births=sum(Births)) %>%
    rename(Mother.s.Race = Mother.s.Single.Race)
}

df_nat_race2 <- clean_single_race(df_nat_race2, Mother.s.Single.Race)
df_nat_race_age2 <- clean_single_race(df_nat_race_age2, Mother.s.Single.Race, Age.of.Mother.9.Code)
df_nat_race_year2 <- clean_single_race(df_nat_race_year2, Mother.s.Single.Race, Year)

df_nat_race <- rbind(df_nat_race1, df_nat_race2) %>%
  group_by(Mother.s.Race) %>%
  summarise(Births=sum(Births))

save(df_nat_race, file="data/natality_race_clean.Rda")

df_nat_race_age <- rbind(df_nat_race_age1, df_nat_race_age2)  %>%
  group_by(Mother.s.Race, Age.of.Mother.9.Code) %>%
  summarise(Births=sum(Births))

save(df_nat_race_age, file="data/natality_race_age_clean.Rda")

df_nat_race_year <- rbind(df_nat_race_year1, df_nat_race_year2) %>%
  group_by(Mother.s.Race, Year) %>%
  summarise(Births=sum(Births))

save(df_nat_race_year, file="data/natality_race_year_clean.Rda")

