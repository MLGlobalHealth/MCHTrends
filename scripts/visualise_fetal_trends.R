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

## Data cleaning - Year
df_fet_year2 = subset(df_fet_year, select = -c(Notes)) %>% 
  na.omit() %>% 
  filter(Year != "2023 (provisional and partial)") 

df_fet_year2[df_fet_year2$Year == '2022 (provisional)', 'Year'] = '2022'

## Data cleaning - State
df_fet_state2 = subset(df_fet_state, select = -c(Notes)) %>% 
  na.omit()

## Data cleaning - Age
df_fet_age2 = subset(df_fet_age, select = -c(Notes)) %>% 
  na.omit() %>% 
  filter(Age.of.Mother.9 != "") 

## Data cleaning - Race
df_fet_race2 = subset(df_fet_race, 
                      select = -c(Notes, 
                                  Mother.s.Bridged.Race.Code,
                                  Mother.s.Hispanic.Origin.Code)) %>% 
  na.omit() 

df_fet_race2[(df_fet_race2$Mother.s.Hispanic.Origin != 'Non-Hispanic') &
             (df_fet_race2$Mother.s.Hispanic.Origin != 'Unknown or Not Stated') &
             (df_fet_race2$Mother.s.Bridged.Race == 'Black or African American'), 
             'Mother.s.Bridged.Race'] = 'Black Hispanic'

df_fet_race2[(df_fet_race2$Mother.s.Hispanic.Origin == 'Non-Hispanic') &
               (df_fet_race2$Mother.s.Hispanic.Origin != 'Unknown or Not Stated') &
             (df_fet_race2$Mother.s.Bridged.Race == 'Black or African American'), 
             'Mother.s.Bridged.Race'] = 'Black Non-Hispanic'

df_fet_race2[(df_fet_race2$Mother.s.Hispanic.Origin == 'Unknown or Not Stated') &
             (df_fet_race2$Mother.s.Bridged.Race == 'Black or African American'), 
             'Mother.s.Bridged.Race'] = 'Black Unknown'

df_fet_race2[(df_fet_race2$Mother.s.Hispanic.Origin != 'Non-Hispanic') &
             (df_fet_race2$Mother.s.Hispanic.Origin != 'Unknown or Not Stated') &
             (df_fet_race2$Mother.s.Bridged.Race == 'White'), 
             'Mother.s.Bridged.Race'] = 'White Hispanic'

df_fet_race2[(df_fet_race2$Mother.s.Hispanic.Origin == 'Non-Hispanic') &
             (df_fet_race2$Mother.s.Hispanic.Origin != 'Unknown or Not Stated') &
             (df_fet_race2$Mother.s.Bridged.Race == 'White'), 
             'Mother.s.Bridged.Race'] = 'White Non-Hispanic'

df_fet_race2[(df_fet_race2$Mother.s.Hispanic.Origin == 'Unknown or Not Stated') &
             (df_fet_race2$Mother.s.Bridged.Race == 'White'), 
             'Mother.s.Bridged.Race'] = 'White Unknown'

df_fet_race2[df_fet_race2$Mother.s.Bridged.Race == 'American Indian or Alaska Native',
             'Mother.s.Bridged.Race'] = "Native American/Alaskan"

df_fet_race2[df_fet_race2$Mother.s.Bridged.Race == 'Native Hawaiian or Other Pacific Islander',
             'Mother.s.Bridged.Race'] = "Native Hawaiian/Other PI"

df_fet_race2 <- df_fet_race2 %>% 
  group_by(Mother.s.Bridged.Race) %>% 
  summarise(Deaths=sum(Fetal.Deaths))

# visualisations ---------------------------------------------------------

df_fet_year2 %>%
  ggplot(aes(x=Year, y=Fetal.Deaths)) +
  geom_line(color="steelblue") +
  theme_minimal() + 
  labs(y = "Deaths", 
       title = "Fetal Deaths by Year (2005-2021)",
       subtitle = "Count of Fetal Deaths Above Each Bar") 
ggsave('figs/plt_fet_year2.png')

df_fet_age2 %>%
  ggplot(aes(x=Age.of.Mother.9, y=Fetal.Deaths)) +
  geom_bar(stat="identity", fill="steelblue") +
  geom_text(aes(label=Fetal.Deaths), vjust=-0.3, color="black", size=3.5) +
  theme_minimal() + 
  labs(y = "Deaths", 
       x = "Age Groups",
       title = "Fetal Deaths by Age (2005-2021)",
       subtitle = "Count of Fetal Deaths Above Each Bar") 
ggsave('figs/plt_fet_age2.png')

df_fet_race2 %>%
  ggplot(aes(x=Mother.s.Bridged.Race, y=Deaths)) +
  geom_bar(stat="identity", fill="steelblue") +
  geom_text(aes(label=Deaths), vjust=-0.3, color="black", size=3.5) +
  theme_minimal() + 
  labs(y = "Deaths", 
       x = "Race",
       title = "Fetal Deaths by Race (2005-2021)",
       subtitle = "Fetal Deaths Above Each Bar") +
  theme(axis.text.x = element_text(angle = 80, hjust=1)) 
ggsave('figs/plt_fet_race2.png')

df_fet_state2 %>%
  ggplot(aes(x=Standard.Residence.States, y=Fetal.Deaths)) +
  geom_bar(stat="identity", fill="steelblue") +
  theme_minimal() + 
  labs(y = "Deaths", 
       x = "State",
       title = "Fetal Deaths by State (2005-2021)",
       subtitle = "Fetal Deaths Above Each Bar") +
  theme(axis.text.x = element_text(angle = 90, vjust=0.3, hjust=1)) 
ggsave('figs/plt_fet_state2.png')
