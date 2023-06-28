# load packages -----------------------------------------------------------

## Package names
packages <- c("tidyverse","ggplot2","dplyr","shiny","data.table")

## Install packages not yet installed
installed_packages <- packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(packages[!installed_packages])
}

## Packages loading
invisible(lapply(packages, library, character.only = TRUE))

setwd('/Users/rpark/Desktop/Research/2. Fetal Maternal Mortality/FetalMaternalMortality')

# clean datafiles ---------------------------------------------------------

# Import and omit NA
clean_df <- function(fname, droplist) {
  df <- read.csv(str_interp('data/${fname}.txt'), sep = "\t")
    subset(df, select = -c(Notes)) %>%
    na.omit() 
}

df_fet_race_age <- clean_df('fetal_deaths_race_age')
df_fet_race_year <- clean_df('fetal_deaths_race_yearly')

df_mat_race_age <- clean_df('maternal_deaths_race_age')
df_mat_race_year <- clean_df('maternal_deaths_race_yearly')

df_pregrel_race_age <- clean_df('pregrel_deaths_race_age')
df_pregrel_race_year <- clean_df('pregrel_deaths_race_yearly')

load(file="data/natality_race_age_clean.Rda")
load(file="data/natality_race_year_clean.Rda")

# These have to be cleaned and attached to each other

clean_mat_race <- function(df, group, varname) {
  df2 = subset(df, select = -c(Single.Race.6.Code, Crude.Rate,
                               Hispanic.Origin.Code, Population)) %>% 
    na.omit() %>%
    filter((Single.Race.6 != '') & (Deaths != 'Suppressed')) 
  
  df2[(df2$Hispanic.Origin == 'Hispanic or Latino') &
        (df2$Single.Race.6 == 'Black or African American'), 
      'Single.Race.6'] = 'Hispanic'
  
  df2[(df2$Hispanic.Origin == 'Not Hispanic or Latino') &
        (df2$Single.Race.6 == 'Black or African American'), 
      'Single.Race.6'] = 'Non-Hispanic Black'
  
  df2[(df2$Hispanic.Origin == 'Hispanic or Latino') &
        (df2$Single.Race.6 == 'White'), 
      'Single.Race.6'] = 'Hispanic'
  
  df2[(df2$Hispanic.Origin == 'Not Hispanic or Latino') &
        (df2$Single.Race.6 == 'White'), 
      'Single.Race.6'] = 'Non-Hispanic White'
  
  df2[df2$Single.Race.6 == 'American Indian or Alaska Native',
      'Single.Race.6'] = "Asian or Pacific Islander"
  
  df2[df2$Single.Race.6 == 'Native Hawaiian or Other Pacific Islander',
      'Single.Race.6'] = "Asian or Pacific Islander"
  
  df2[df2$Single.Race.6 == 'Asian',
      'Single.Race.6'] = "Asian or Pacific Islander"
  
  df2 <- df2 %>% 
    mutate_at('Deaths', as.numeric) %>%
    group_by({{group}}, Single.Race.6) %>% 
    summarise(Deaths=sum(Deaths)) %>%
    rename({{varname}} := Deaths)
}

cln_df_mat_race_year <- clean_mat_race(df_mat_race_year, Year, MMR.Deaths)
cln_df_mat_race_age <- clean_mat_race(df_mat_race_age, Five.Year.Age.Groups.Code, MMR.Deaths)
cln_df_pregrel_race_year <- clean_mat_race(df_pregrel_race_year, Year, PRMR.Deaths)
cln_df_pregrel_race_age <- clean_mat_race(df_pregrel_race_age, Five.Year.Age.Groups.Code, PRMR.Deaths)

mrg_mat_race_year0 <- merge(cln_df_mat_race_year, cln_df_pregrel_race_year, 
                           by = c('Year','Single.Race.6'), all.x = TRUE)
mrg_mat_race_age0 <- merge(cln_df_mat_race_age, cln_df_pregrel_race_age, 
                          by = c('Five.Year.Age.Groups.Code','Single.Race.6'), 
                          all.x = TRUE)

mrg_mat_race_year <- merge(mrg_mat_race_year0, df_nat_race_year, all.x=TRUE,
                           by.x = c('Year','Single.Race.6'),
                           by.y = c('Year', 'Mother.s.Bridged.Race'))

mrg_mat_race_age <- merge(mrg_mat_race_age0, df_nat_race_age, all.x=TRUE,
                          by.x = c('Five.Year.Age.Groups.Code','Single.Race.6'),
                          by.y = c('Age.of.Mother.9.Code', 'Mother.s.Bridged.Race'))

mrg_mat_race_year$MMR.Deaths.by.Births = (
  mrg_mat_race_year$MMR.Deaths*100000)/mrg_mat_race_year$Births
mrg_mat_race_year$PRMR.Deaths.by.Births = (
  mrg_mat_race_year$PRMR.Deaths*100000)/mrg_mat_race_year$Births

mrg_mat_race_age$MMR.Deaths.by.Births = (
  mrg_mat_race_age$MMR.Deaths*100000)/mrg_mat_race_age$Births
mrg_mat_race_age$PRMR.Deaths.by.Births = (
  mrg_mat_race_age$PRMR.Deaths*100000)/mrg_mat_race_age$Births

# This has to be cleaned
clean_fet_race <- function(df, ...) {
  df_fet_race2 = subset(df, select = -c(Mother.s.Bridged.Race.Code, 
                                        Mother.s.Bridged.Race.Hispanic.Origin.Code)) %>% 
    na.omit() 

  df_fet_race2[(df_fet_race2$Mother.s.Bridged.Race.Hispanic.Origin != 'Origin unknown or not stated') &
                 (df_fet_race2$Mother.s.Bridged.Race.Hispanic.Origin != 'Not Available') &
                 (df_fet_race2$Mother.s.Bridged.Race.Hispanic.Origin != 'Non-Hispanic Black') &
                 (df_fet_race2$Mother.s.Bridged.Race == 'Black or African American'), 
               'Mother.s.Bridged.Race'] = 'Hispanic'
  
  df_fet_race2[(df_fet_race2$Mother.s.Bridged.Race.Hispanic.Origin == 'Non-Hispanic Black') &
                 (df_fet_race2$Mother.s.Bridged.Race == 'Black or African American'), 
               'Mother.s.Bridged.Race'] = 'Non-Hispanic Black'
  
  df_fet_race2[((df_fet_race2$Mother.s.Bridged.Race.Hispanic.Origin == 'Origin unknown or not stated') |
                 (df_fet_race2$Mother.s.Bridged.Race.Hispanic.Origin == 'Not Available')) &
                 ((df_fet_race2$Mother.s.Bridged.Race == 'Black or African American') |
                 (df_fet_race2$Mother.s.Bridged.Race == 'White')),
               'Mother.s.Bridged.Race'] = ''
  
  df_fet_race2[((df_fet_race2$Mother.s.Bridged.Race.Hispanic.Origin == 'Non-Hispanic other races') |
                 (df_fet_race2$Mother.s.Bridged.Race.Hispanic.Origin == 'Non-Hispanic White')) &
                 (df_fet_race2$Mother.s.Bridged.Race == 'White'), 
               'Mother.s.Bridged.Race'] = 'Non-Hispanic White'
  
  df_fet_race2[df_fet_race2$Mother.s.Bridged.Race == 'American Indian or Alaska Native',
               'Mother.s.Bridged.Race'] = "Native American/Alaskan"
  
  df_fet_race2[df_fet_race2$Mother.s.Bridged.Race == 'Native Hawaiian or Other Pacific Islander',
               'Mother.s.Bridged.Race'] = "Asian or Pacific Islander"
  
  df_fet_race2 <- df_fet_race2 %>% 
    group_by(Mother.s.Bridged.Race, ...) %>% 
    summarise(Deaths=sum(Fetal.Deaths))
}

cln_df_fet_race_year <- clean_fet_race(df_fet_race_year, Year)
cln_df_fet_race_age <- clean_fet_race(df_fet_race_age, Age.of.Mother.9.Code)

mrg_fet_race_year <- merge(cln_df_fet_race_year, df_nat_race_year, all.x=TRUE,
                           by = c('Year', 'Mother.s.Bridged.Race'))

mrg_fet_race_age <- merge(cln_df_fet_race_age, df_nat_race_age, all.x=TRUE,
                          by = c('Age.of.Mother.9.Code', 'Mother.s.Bridged.Race'))

mrg_fet_race_year$Deaths.by.Births = (
  mrg_fet_race_year$Deaths*1000)/mrg_fet_race_year$Births
mrg_fet_race_age$Deaths.by.Births = (
  mrg_fet_race_age$Deaths*1000)/mrg_fet_race_age$Births

# reshape for plots --------------------------------------------------------

reshape_long <- function(df, drop_list) {
  df_long <- reshape(
    df, direction='long', 
    varying=c('MMR.Deaths', 'MMR.Deaths.by.Births', 
              'PRMR.Deaths', 'PRMR.Deaths.by.Births'), 
    timevar='Type',
    times=c('MMR', 'PRMR'),
    v.names=c('Deaths', 'Deaths.by.Births'),
    idvar='sbj') %>%
    subset(select = -c(Births, sbj))
}

long_mat_race_age <- reshape_long(mrg_mat_race_age)

# visualisations -----------------------------------------------------------

mrg_mat_race_age %>% 
  filter(Single.Race.6 %in% c('Non-Hispanic Black', 'Non-Hispanic White')) %>%
  ggplot(aes(x=Five.Year.Age.Groups.Code, y=MMR.Deaths.by.Births, fill=Single.Race.6)) +
  geom_bar(stat="identity", position=position_dodge()) +
  geom_text(aes(label=MMR.Deaths, group=Single.Race.6), 
            position = position_dodge(width = 0.9), vjust = -0.3) +
  theme_minimal() + 
  labs(y = "Rate per 100,000 Live Births", 
       x = "Age Groups",
       title = "Rates of Maternal Deaths (2018-2021)",
       subtitle = "Count of Deaths Above Each Bar",
       caption = "Note: Analysis includes deaths during pregnancy and up to 42 days after birth. PRMR stands for Pregnancy-Related Mortality Rate, which includes deaths 
         between 43-365 days after birth.") + 
  theme(plot.caption=element_text(hjust = 0), axis.text.x = element_text(angle = 80, hjust=1)) +
  guides(fill=guide_legend(title=""))
ggsave('figs/plt_mat_race_age.png')

mrg_fet_race_age %>% 
  filter(Mother.s.Bridged.Race %in% c('Non-Hispanic Black', 'Non-Hispanic White')) %>%
  ggplot(aes(x=Age.of.Mother.9.Code, y=Deaths.by.Births, fill=Mother.s.Bridged.Race)) +
  geom_bar(stat="identity", position=position_dodge()) +
  geom_text(aes(label=Deaths, group=Mother.s.Bridged.Race), 
            position = position_dodge(width = 0.9), vjust = -0.3) +
  theme_minimal() + 
  labs(y = "Rate per 1,000 Live Births", 
       x = "Age Groups",
       title = "Rates of Fetal Deaths (2005-2021)",
       subtitle = "Count of Deaths Above Each Bar") + 
  theme(plot.caption=element_text(hjust = 0), axis.text.x = element_text(angle = 80, hjust=1)) +
  guides(fill=guide_legend(title="")) 
ggsave('figs/plt_fet_race_age.png')

mrg_fet_race_year %>% 
  filter(Mother.s.Bridged.Race %in% c('Non-Hispanic Black', 'Non-Hispanic White')) %>%
  ggplot(aes(x=Year, y=Deaths.by.Births, group=Mother.s.Bridged.Race, colour=Mother.s.Bridged.Race)) +
  geom_line() + 
  theme_minimal() + 
  labs(y = "Rate per 1,000 Live Births", 
       x = "Year",
       title = "Rates of Fetal Deaths (2005-2018)",
       subtitle = "Count of Deaths Above Each Bar") + 
  theme(plot.caption=element_text(hjust = 0), axis.text.x = element_text(angle = 80, hjust=1)) +
  guides(colour=guide_legend(title="")) + scale_fill_brewer(palette = "Dark2") +
  ylim(0,15)
ggsave('figs/plt_fet_race_year.png')

