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

setwd('/Users/rpark/Desktop/Research/Rotations/2. Fetal Maternal Mortality/FetalMaternalMortality')

# clean datafiles ---------------------------------------------------------

clean_df <- function(fname) {
  df <- read.csv(str_interp('data/${fname}.txt'), sep = "\t")
  subset(df, select = -c(Notes, Fertility.Rate, Female.Population)) %>%
  na.omit()
}

clean_df23 <- function(fname) {
  df <- read.csv(str_interp('data/${fname}.txt'), sep = "\t")
  subset(df, select = -c(Notes)) %>%
    na.omit() %>%
      mutate(Births = Births * 2)
}

df_nat_year1a <- read.csv('data/natality_yearly_00_02.txt', sep = "\t")
df_nat_year1a <- subset(df_nat_year1a, select = -c(Notes)) %>%
  na.omit()
df_nat_year1b <- clean_df('natality_yearly_03_04')
df_nat_year1c <- clean_df('natality_yearly_05_06')
df_nat_year2 <- clean_df('natality_yearly_07_22')
df_nat_year3 <- clean_df23('natality_yearly_first_half_23')
df_nat_year <- rbind(df_nat_year1a, df_nat_year1b, df_nat_year1c, df_nat_year2, df_nat_year3) 
save(df_nat_year, file="data/natality_yearly_clean.Rda")

df_nat_age1 <- clean_df('natality_age_05_06')
df_nat_age2 <- clean_df('natality_age_07_22')
df_nat_age3 <- clean_df23('natality_age_first_half_23')
df_nat_age <- rbind(df_nat_age1, df_nat_age2, df_nat_age3) %>%
  filter(Age.of.Mother.9 != "") %>%
  group_by(Age.of.Mother.9.Code) %>% 
  summarise(Births=sum(Births))
save(df_nat_age, file="data/natality_age_clean.Rda")

df_nat_state1 <- clean_df('natality_state_05_06')
df_nat_state2 <- clean_df('natality_state_07_22')
df_nat_state3 <- clean_df23('natality_state_first_half_23')  %>%
  rename(
    State.Code = State.of.Residence.Code,
    State = State.of.Residence
  )
df_nat_state <- rbind(df_nat_state1, df_nat_state2, df_nat_state3) %>%
  group_by(State) %>%
  summarise(Births=sum(Births))
save(df_nat_state, file="data/natality_state_clean.Rda")

df_nat_race1a <- clean_df('natality_race_05_06')
df_nat_race1b <- clean_df('natality_race_07_10')
df_nat_race1c <- clean_df('natality_race_11_15')
df_nat_race1 <- rbind(df_nat_race1a, df_nat_race1b, df_nat_race1c)
df_nat_race2a <- clean_df('natality_race_16_19')
df_nat_race2b <- clean_df('natality_race_20_22')
df_nat_race2c <- clean_df23('natality_race_first_half_23') %>%
  rename(
    Mother.s.Single.Race = Mother.s.Single.Race.6,
    Mother.s.Single.Race.Code = Mother.s.Single.Race.6.Code
  )
df_nat_race2 <- rbind(df_nat_race2a, df_nat_race2b, df_nat_race2c) %>%
  filter(Mother.s.Single.Race != 'More than one race')

clean_df3 <- function(fname) {
  df <- read.csv(str_interp('data/${fname}.txt'), sep = "\t")
  subset(df, select = -c(Notes)) %>%
  na.omit()
}

# df_nat_age_census_year <- clean_df('natality_age_year_census_00_02')
# save(df_nat_age_census_year, file="data/natality_age_census_year_clean.Rda")

df_nat_race_03_04 <- clean_df('natality_race_03_04') %>% 
  filter(Mother.s.Hispanic.Origin != '')
df_nat_race_05_06 <- clean_df('natality_race_05_06') %>% 
  filter(Mother.s.Hispanic.Origin != '')
df_nat_race_07_10 <- clean_df('natality_race_07_10') %>% 
  filter(Mother.s.Hispanic.Origin != '')
df_nat_race_03_10 <- rbind(df_nat_race_03_04, df_nat_race_05_06, df_nat_race_07_10) %>%
  group_by(Mother.s.Bridged.Race, Mother.s.Hispanic.Origin) %>%
  summarise(Births = sum(Births))
df_nat_race_11_15 <- clean_df('natality_race_11_15') %>% 
  filter(Mother.s.Hispanic.Origin != '')

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
df_nat_race_03_10 <- clean_bridged_race(df_nat_race_03_10, Mother.s.Bridged.Race)
df_nat_race_11_15 <- clean_bridged_race(df_nat_race_11_15, Mother.s.Bridged.Race)

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
      'Mother.s.Single.Race'] = "Asian or Pacific Islander"
  
  df2[df2$Mother.s.Single.Race == 'Asian',
      'Mother.s.Single.Race'] = "Asian or Pacific Islander"
  
  df2 <- df2 %>% 
    mutate_at('Births', as.numeric) %>%
    group_by(...) %>% 
    summarise(Births=sum(Births)) %>%
    rename(Mother.s.Race = Mother.s.Single.Race)
}

df_nat_race2 <- clean_single_race(df_nat_race2, Mother.s.Single.Race)

df_nat_race_16_19 <- clean_df('natality_race_16_19') %>% 
  filter(Mother.s.Hispanic.Origin != '')
df_nat_race_16_19 <- clean_single_race(df_nat_race_16_19, Mother.s.Single.Race)

df_nat_race_20_22 <- clean_df('natality_race_20_22') %>% 
  filter(Mother.s.Hispanic.Origin != '')
df_nat_race_20_22 <- clean_single_race(df_nat_race_20_22, Mother.s.Single.Race)

df_nat_race_23 <- clean_df23('natality_race_first_half_23') %>% 
  filter(Mother.s.Hispanic.Origin != '') %>%
  rename(
    Mother.s.Single.Race = Mother.s.Single.Race.6,
    Mother.s.Single.Race.Code = Mother.s.Single.Race.6.Code
  )
df_nat_race_23 <- clean_single_race(df_nat_race_23, Mother.s.Single.Race)

df_nat_race_11_19 <- rbind(df_nat_race_11_15, df_nat_race_16_19) %>% 
  mutate_at(c('Births'), as.numeric) %>%
  group_by(Mother.s.Race) %>% 
  summarise(Births=sum(Births)) %>%
  filter(Mother.s.Race != "More than one race")

df_nat_race_20_22 <- df_nat_race_20_22 %>%
  filter(Mother.s.Race != "More than one race")

df_nat_race_20_23 <- rbind(df_nat_race_20_22, df_nat_race_23) %>%
  mutate_at(c('Births'), as.numeric) %>%
  group_by(Mother.s.Race) %>% 
  summarise(Births=sum(Births)) %>%
  filter(Mother.s.Race != "More than one race")

df_nat_race_00_02 <- clean_df3('natality_race_00_02')
df_nat_race_00_02[(df_nat_race_00_02$Mother.s.Hispanic.Origin == 'Origin unknown or not stated') &
      (df_nat_race_00_02$Mother.s.Race == 'Black or African American'), 
      'Mother.s.Race'] = 'Unknown Black'
df_nat_race_00_02[(df_nat_race_00_02$Mother.s.Hispanic.Origin == 'Non-Hispanic Black') &
                    (df_nat_race_00_02$Mother.s.Race == 'Black or African American'), 
                  'Mother.s.Race'] = 'Non-Hispanic Black'
df_nat_race_00_02[((df_nat_race_00_02$Mother.s.Hispanic.Origin != 'Non-Hispanic Black') &
                    (df_nat_race_00_02$Mother.s.Hispanic.Origin != 'Origin unknown or not stated')) &
                    (df_nat_race_00_02$Mother.s.Race == 'Black or African American'), 
                  'Mother.s.Race'] = 'Hispanic'
df_nat_race_00_02[(df_nat_race_00_02$Mother.s.Hispanic.Origin == 'Origin unknown or not stated') &
                    (df_nat_race_00_02$Mother.s.Race == 'White'), 
                  'Mother.s.Race'] = 'Unknown White'
df_nat_race_00_02[(df_nat_race_00_02$Mother.s.Hispanic.Origin == 'Non-Hispanic White') &
                    (df_nat_race_00_02$Mother.s.Race == 'White'), 
                  'Mother.s.Race'] = 'Non-Hispanic White'
df_nat_race_00_02[((df_nat_race_00_02$Mother.s.Hispanic.Origin != 'Non-Hispanic White') &
                     (df_nat_race_00_02$Mother.s.Hispanic.Origin != 'Origin unknown or not stated')) &
                    (df_nat_race_00_02$Mother.s.Race == 'White'), 
                  'Mother.s.Race'] = 'Hispanic'
df_nat_race_00_02[((df_nat_race_00_02$Mother.s.Race == 'Chinese') | 
                     (df_nat_race_00_02$Mother.s.Race == 'Filipino') |
                     (df_nat_race_00_02$Mother.s.Race == 'Hawaiian') |
                     (df_nat_race_00_02$Mother.s.Race == 'Japanese') |
                     (df_nat_race_00_02$Mother.s.Race == 'Other Asian ')), 
                  'Mother.s.Race'] = 'Asian or Pacific Islander'
df_nat_race_00_02[df_nat_race_00_02$Mother.s.Race == 'American Indian or Alaska Native',
                  'Mother.s.Race'] = 'Native American/Alaskan'

df_nat_race_00_02 <- df_nat_race_00_02 %>% group_by(Mother.s.Race) %>% 
  summarise(Births = sum(Births))

df_nat_race_00_10 <- rbind(df_nat_race_00_02, df_nat_race_03_10) %>% 
  group_by(Mother.s.Race) %>% 
  summarise(Births = sum(Births))

save(df_nat_race_00_10, file="data/natality_race_00_10_clean.Rda")
save(df_nat_race_11_19, file="data/natality_race_11_19_clean.Rda")
save(df_nat_race_20_22, file="data/natality_race_20_22_clean.Rda")
save(df_nat_race_20_23, file="data/natality_race_20_23_clean.Rda")

df_nat_race <- rbind(df_nat_race1, df_nat_race2) %>%
  group_by(Mother.s.Race) %>%
  summarise(Births=sum(Births))

save(df_nat_race, file="data/natality_race_clean.Rda")

