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

## Import datafiles 
df_mat_year <- read.csv('data/maternal_deaths_yearly.txt', sep = "\t")
df_mat_state <- read.csv('data/maternal_deaths_state.txt', sep = "\t")
df_mat_age <- read.csv('data/maternal_deaths_age.txt', sep = "\t")
df_mat_race <- read.csv('data/maternal_deaths_race.txt', sep = "\t")

df_pregrel_year <- read.csv('data/pregrel_deaths_yearly.txt', sep = "\t")
df_pregrel_state <- read.csv('data/pregrel_deaths_state.txt', sep = "\t")
df_pregrel_age <- read.csv('data/pregrel_deaths_age.txt', sep = "\t")
df_pregrel_race <- read.csv('data/pregrel_deaths_race.txt', sep = "\t")

load(file="data/natality_yearly_clean.Rda")
load(file="data/natality_age_clean.Rda")
load(file="data/natality_race_clean.Rda")
load(file="data/natality_state_clean.Rda")

## Data cleaning - Year
clean_year <- function(df, varname) {
  df2 = subset(df, select = -c(Notes, Year.Code, 
                               Population, Crude.Rate)) %>% 
        filter(df$Year != "") %>%
        na.omit() %>%
        rename({{varname}} := Deaths)
}
df_mat_year2 <- clean_year(df_mat_year, MMR.Deaths)
df_pregrel_year2 <- clean_year(df_pregrel_year, PRMR.Deaths)
df_mat_pr_year <- merge(df_mat_year2, df_pregrel_year2, by='Year')

df_mat_year3 <- merge(df_mat_pr_year, df_nat_year, by='Year', all.x = TRUE)
df_mat_year3[(df_mat_year3$Year == '2022 (provisional)'), 
             'Births'] = df_mat_year3$Births[df_mat_year3$Year == '2021']
df_mat_year3[(df_mat_year3$Year == '2022 (provisional)'), 
             'Year'] = 2022
  
df_mat_year3$MMR.Deaths.by.Births = (df_mat_year3$MMR.Deaths*100000)/df_mat_year3$Births
df_mat_year3$PRMR.Deaths.by.Births = (df_mat_year3$PRMR.Deaths*100000)/df_mat_year3$Births

## Data cleaning - State
clean_state <- function(df, varname) {
  df2 = subset(df, select = -c(Notes, Population, Crude.Rate)) %>% 
    na.omit() %>%
    rename({{varname}} := Deaths)
}
df_mat_state2 <- clean_state(df_mat_state, MMR.Deaths)
df_pregrel_state2 <- clean_state(df_pregrel_state, PRMR.Deaths)
df_mat_pr_state <- merge(df_mat_state2, df_pregrel_state2, 
                         by='Residence.State', all.x=TRUE)

df_mat_state3 <- merge(df_mat_pr_state, df_nat_state, 
                       by.x='Residence.State', by.y='State')

df_mat_state3$MMR.Deaths.by.Births = (df_mat_state3$MMR.Deaths*100000)/df_mat_state3$Births
df_mat_state3$PRMR.Deaths.by.Births = (df_mat_state3$PRMR.Deaths*100000)/df_mat_state3$Births

## Data cleaning - Age
clean_age <- function(df, varname) {
  df2 = subset(df, select = -c(Notes, Population, Crude.Rate)) %>% 
    na.omit() %>% 
    filter(Five.Year.Age.Groups != "") %>%
    rename({{varname}} := Deaths)
}

df_mat_age2 <- clean_age(df_mat_age, MMR.Deaths)
df_pregrel_age2 <- clean_age(df_pregrel_age, PRMR.Deaths)
df_mat_pr_age <- merge(df_mat_age2, df_pregrel_age2, 
                       by='Five.Year.Age.Groups.Code')

df_mat_age3 <- merge(df_mat_pr_age, df_nat_age, 
                     by.x='Five.Year.Age.Groups.Code',
                     by.y='Age.of.Mother.9.Code')

df_mat_age3$MMR.Deaths.by.Births = (df_mat_age3$MMR.Deaths*100000)/df_mat_age3$Births
df_mat_age3$PRMR.Deaths.by.Births = (df_mat_age3$PRMR.Deaths*100000)/df_mat_age3$Births

## Data cleaning - Race
clean_race <- function(df, varname) {
  df2 = subset(df, select = -c(Notes, Crude.Rate, Single.Race.6.Code, 
                               Population, Hispanic.Origin.Code)) %>% 
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
    group_by(Single.Race.6) %>% 
    summarise(Deaths=sum(Deaths)) %>%
    rename({{varname}} := Deaths)
}

df_mat_race2 <- clean_race(df_mat_race, MMR.Deaths)
df_pregrel_race2 <- clean_race(df_pregrel_race, PRMR.Deaths)
df_mat_pr_race <- merge(df_mat_race2, df_pregrel_race2, 
                        by='Single.Race.6', all.x=TRUE)

df_mat_race3 <- merge(df_mat_pr_race, df_nat_race,
                      by.x = 'Single.Race.6',
                      by.y = 'Mother.s.Bridged.Race')

df_mat_race3$MMR.Deaths.by.Births = (df_mat_race3$MMR.Deaths*100000)/df_mat_race3$Births
df_mat_race3$PRMR.Deaths.by.Births = (df_mat_race3$PRMR.Deaths*100000)/df_mat_race3$Births

# reshape for plots ------------------------------------------------------

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

df_long_mat_year <- reshape_long(df_mat_year3)
df_long_mat_state <- reshape_long(df_mat_state3)
df_long_mat_age <- reshape_long(df_mat_age3)
df_long_mat_race <- reshape_long(df_mat_race3)

# visualisations ---------------------------------------------------------

df_long_mat_year %>%
  ggplot(aes(x=Year, y=Deaths.by.Births, fill=Type)) +
  geom_bar(stat="identity", position=position_dodge()) +
  geom_text(aes(label=Deaths, group=Type), 
            position = position_dodge(width = 0.9), vjust = -0.3) +
  theme_minimal() + 
  labs(y = "Rate per 100,000 Live Births", 
       title = "Rates of Maternal and Pregnancy-Related Deaths by Year (2018-2022)",
       subtitle = "Count of Deaths Above Each Bar",
       caption = "Note: The 2022 count is provisional. We use 2021 live births to compute the 2022 mortality rate as the 2022 live birth count is not available. MMR 
          stands for Maternal Mortality Rate, which includes deaths during pregnancy and up to 42 days after birth. PRMR stands for Pregnancy-
          Related Mortality Rate, which includes deaths between 43-365 days after birth.") + 
  theme(plot.caption=element_text(hjust = 0))
ggsave("figs/plt_mat_year.png")

df_long_mat_age %>%
  ggplot(aes(x=Five.Year.Age.Groups.Code, y=Deaths.by.Births, fill=Type)) +
  geom_bar(stat="identity", position=position_dodge()) +
  geom_text(aes(label=Deaths, group=Type), 
            position = position_dodge(width = 0.9), vjust = -0.3) +
  theme_minimal() + 
  labs(y = "Rate per 100,000 Live Births", 
       x = "Age Groups",
       title = "Rates of Maternal and Pregnancy-Related Deaths by Age (2018-2021)",
       subtitle = "Count of Deaths Above Each Bar",
       caption = "Note: MMR stands for Maternal Mortality Rate, which includes deaths during pregnancy and up to 42 days after birth. PRMR stands for Pregnancy-
          Related Mortality Rate, which includes deaths between 43-365 days after birth.") + 
  theme(plot.caption=element_text(hjust = 0))
ggsave("figs/plt_mat_age.png")

df_long_mat_race %>%
  ggplot(aes(x=Single.Race.6, y=Deaths.by.Births, fill=Type)) +
  geom_bar(stat="identity", position=position_dodge()) +
  geom_text(aes(label=Deaths, group=Type), 
            position = position_dodge(width = 0.9), vjust = -0.3) +
  theme_minimal() + 
  labs(y = "Rate per 100,000 Live Births", 
       x = "Racial/Ethnic Group",
       title = "Rates of Maternal and Pregnancy-Related Deaths by Racial/Ethnic Group (2018-2021)",
       subtitle = "Count of Deaths Above Each Bar",
       caption = "Note: MMR stands for Maternal Mortality Rate, which includes deaths during pregnancy and up to 42 days after birth. PRMR stands for Pregnancy-
          Related Mortality Rate, which includes deaths between 43-365 days after birth.") + 
  theme(plot.caption=element_text(hjust = 0), axis.text.x = element_text(angle = 80, hjust=1))
ggsave("figs/plt_mat_race.png")

mmr_national_avg = (sum(df_mat_state3$MMR.Deaths)*100000)/sum(df_mat_state3$Births)

df_mat_state3 %>%
  mutate(Residence.State = fct_reorder(Residence.State, 
                                       desc(MMR.Deaths.by.Births))) %>%
  ggplot(aes(x=Residence.State, y=MMR.Deaths.by.Births)) +
  geom_bar(stat="identity", fill="steelblue") + coord_flip() +
  geom_hline(yintercept = mmr_national_avg, color = "red") +
  theme_minimal() + 
  labs(y = "Rate per 100,000 Live Births", 
       x = "State",
       title = "Rates of Maternal Deaths by State (2018-2021)",
       subtitle = "National Average Rate in Red (5.1)",
       caption = "Note: MMR stands for Maternal Mortality Rate, which includes deaths during pregnancy and up to 42 days after birth. PRMR stands for Pregnancy-
          Related Mortality Rate, which includes deaths between 43-365 days after birth.") + 
  theme(plot.caption=element_text(hjust = 0))
ggsave("figs/plt_mat_mmr_state.png")

prmr_national_avg = (sum(df_mat_state3$PRMR.Deaths, na.rm=TRUE)*100000)/sum(df_mat_state3$Births)

df_mat_state3 %>%
  na.omit() %>%
  mutate(Residence.State = fct_reorder(Residence.State, 
                                       desc(PRMR.Deaths.by.Births))) %>%
  ggplot(aes(x=Residence.State, y=PRMR.Deaths.by.Births)) +
  geom_bar(stat="identity", fill="steelblue", na.rm = TRUE) + coord_flip() +
  geom_hline(yintercept = prmr_national_avg, color = "red") +
  theme_minimal() + 
  labs(y = "Rate per 100,000 Live Births", 
       x = "State",
       title = "Rates of Pregnancy-Related Deaths by State (2018-2021)",
       subtitle = "National Average Rate in Red (2.1)",
       caption = "Note: MMR stands for Maternal Mortality Rate, which includes deaths during pregnancy and up to 42 days after birth. PRMR stands for Pregnancy-
          Related Mortality Rate, which includes deaths between 43-365 days after birth.") + 
  theme(plot.caption=element_text(hjust = 0))
ggsave("figs/plt_mat_prmr_state.png")

ggplot(df_long_mat_state) + 
  geom_histogram(aes(x=Deaths.by.Births, colour=Type, fill=Type), 
                 alpha=0.5, binwidth = 0.2) +
  theme_minimal() + 
  labs(y = "Density", 
       x = "Rate per 100,000 Live Births",
       title = "Rates of Maternal and Pregnancy-Related Deaths Across States (2018-2021)",
       caption = "Note: MMR stands for Maternal Mortality Rate, which includes deaths during pregnancy and up to 42 days after birth. PRMR stands for Pregnancy-Related
          Mortality Rate, which includes deaths between 43-365 days after birth.") + 
  theme(plot.caption=element_text(hjust = 0))
ggsave("figs/plt_mat_state.png")
