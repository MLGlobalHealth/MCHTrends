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

## Import datafiles 
df_fet_year <- read.csv('data/fetal_deaths_yearly_full.txt', sep = "\t")
df_fet_state <- read.csv('data/fetal_deaths_state_full.txt', sep = "\t")
df_fet_age <- read.csv('data/fetal_deaths_age_full.txt', sep = "\t")
df_fet_race <- read.csv('data/fetal_deaths_race_full.txt', sep = "\t")

load(file="data/natality_yearly_clean.Rda")
load(file="data/natality_age_clean.Rda")
load(file="data/natality_race_clean.Rda")
load(file="data/natality_state_clean.Rda")

## Data cleaning - Year
df_fet_year2 = subset(df_fet_year, select = -c(Notes)) %>% 
  na.omit() 

df_fet_year3 <- merge(df_fet_year2, df_nat_year, by='Year')
df_fet_year3$Deaths.by.Births = (df_fet_year3$Fetal.Deaths*100000)/df_fet_year3$Births

## Data cleaning - State
df_fet_state2 = subset(df_fet_state, select = -c(Notes)) %>% 
  na.omit()

df_fet_state3 <- merge(df_fet_state2, df_nat_state, 
                       by.x='Standard.Residence.States',
                       by.y='State')
df_fet_state3$Deaths.by.Births = (df_fet_state3$Fetal.Deaths*100000)/df_fet_state3$Births

## Data cleaning - Age
df_fet_age2 = subset(df_fet_age, select = -c(Notes)) %>% 
  na.omit() %>% 
  filter(Age.of.Mother.9 != "") 

df_fet_age3 <- merge(df_fet_age2, df_nat_age, by='Age.of.Mother.9.Code')
df_fet_age3$Deaths.by.Births = (df_fet_age3$Fetal.Deaths*100000)/df_fet_age3$Births

## Data cleaning - Race
df_fet_race2 = subset(df_fet_race, 
                      select = -c(Notes, 
                                  Mother.s.Bridged.Race.Code,
                                  Mother.s.Hispanic.Origin.Code)) %>% 
  na.omit() 

df_fet_race2[(df_fet_race2$Mother.s.Hispanic.Origin != 'Non-Hispanic') &
             (df_fet_race2$Mother.s.Hispanic.Origin != 'Unknown or Not Stated') &
             (df_fet_race2$Mother.s.Bridged.Race == 'Black or African American'), 
             'Mother.s.Bridged.Race'] = 'Hispanic'

df_fet_race2[(df_fet_race2$Mother.s.Hispanic.Origin == 'Non-Hispanic') &
               (df_fet_race2$Mother.s.Hispanic.Origin != 'Unknown or Not Stated') &
             (df_fet_race2$Mother.s.Bridged.Race == 'Black or African American'), 
             'Mother.s.Bridged.Race'] = 'Non-Hispanic Black'

df_fet_race2[(df_fet_race2$Mother.s.Hispanic.Origin == 'Unknown or Not Stated') &
             (df_fet_race2$Mother.s.Bridged.Race == 'Black or African American'), 
             'Mother.s.Bridged.Race'] = 'Unknown Black'

df_fet_race2[(df_fet_race2$Mother.s.Hispanic.Origin != 'Non-Hispanic') &
             (df_fet_race2$Mother.s.Hispanic.Origin != 'Unknown or Not Stated') &
             (df_fet_race2$Mother.s.Bridged.Race == 'White'), 
             'Mother.s.Bridged.Race'] = 'Hispanic'

df_fet_race2[(df_fet_race2$Mother.s.Hispanic.Origin == 'Non-Hispanic') &
             (df_fet_race2$Mother.s.Hispanic.Origin != 'Unknown or Not Stated') &
             (df_fet_race2$Mother.s.Bridged.Race == 'White'), 
             'Mother.s.Bridged.Race'] = 'Non-Hispanic White'

df_fet_race2[(df_fet_race2$Mother.s.Hispanic.Origin == 'Unknown or Not Stated') &
             (df_fet_race2$Mother.s.Bridged.Race == 'White'), 
             'Mother.s.Bridged.Race'] = 'Unknown White'

df_fet_race2[df_fet_race2$Mother.s.Bridged.Race == 'American Indian or Alaska Native',
             'Mother.s.Bridged.Race'] = "Native American/Alaskan"

df_fet_race2[df_fet_race2$Mother.s.Bridged.Race == 'Native Hawaiian or Other Pacific Islander',
             'Mother.s.Bridged.Race'] = "Asian or Pacific Islander"

df_fet_race2 <- df_fet_race2 %>% 
  group_by(Mother.s.Bridged.Race) %>% 
  summarise(Fetal.Deaths=sum(Fetal.Deaths))

df_fet_race3 <- merge(df_fet_race2, df_nat_race, by='Mother.s.Bridged.Race')
df_fet_race3$Deaths.by.Births = (df_fet_race3$Fetal.Deaths*100000)/df_fet_race3$Births

# visualisations ---------------------------------------------------------

df_fet_year3 %>%
  ggplot(aes(x=Year, y=Deaths.by.Births)) +
  geom_line(color="steelblue") +
  theme_minimal() + 
  labs(y = "Rate per 100,000 Live Births", 
       title = "Rates of Fetal Deaths by Year (2005-2021)") 
ggsave('figs/plt_fet_year.png')

df_fet_age3 %>%
  ggplot(aes(x=Age.of.Mother.9.Code, y=Deaths.by.Births)) +
  geom_bar(stat="identity", fill="steelblue") +
  geom_text(aes(label=Fetal.Deaths), vjust=-0.3, color="black", size=3.5) +
  theme_minimal() + 
  labs(y = "Rate per 100,000 Live Births", 
       x = "Age Groups",
       title = "Rates of Fetal Deaths by Age (2005-2021)",
       subtitle = "Count of Deaths Above Each Bar") 
ggsave('figs/plt_fet_age.png')

df_fet_race3 %>%
  ggplot(aes(x=Mother.s.Bridged.Race, y=Deaths.by.Births)) +
  geom_bar(stat="identity", fill="steelblue") +
  geom_text(aes(label=Fetal.Deaths), vjust=-0.3, color="black", size=3.5) +
  theme_minimal() + 
  labs(y = "Rate per 100,000 Live Births", 
       x = "Race",
       title = "Rates of Fetal Deaths by Race (2005-2021)",
       subtitle = "Count of Deaths Above Each Bar") +
  theme(axis.text.x = element_text(angle = 80, hjust=1)) 
ggsave('figs/plt_fet_race.png')

national_avg = (sum(df_fet_state3$Fetal.Deaths)*100000)/sum(df_fet_state3$Births)

df_fet_state3 %>%
  ggplot(aes(x=Standard.Residence.States, y=Deaths.by.Births)) +
  geom_bar(stat="identity", fill="steelblue") +
  geom_hline(yintercept = national_avg, color = "red") +
  theme_minimal() + 
  labs(y = "Rate per 100,000 Live Births", 
       x = "State",
       title = "Rates of Fetal Deaths by State (2005-2021)",
       subtitle = "National Average Rate in Red (599.52)") +
  theme(axis.text.x = element_text(angle = 90, vjust=0.3, hjust=1)) 
ggsave("figs/plt_fet_state.png")

