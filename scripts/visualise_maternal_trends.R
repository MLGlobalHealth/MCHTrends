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
df_mat_year <- read.csv('data/maternal_deaths_yearly.txt', sep = "\t")
df_mat_state <- read.csv('data/maternal_deaths_state.txt', sep = "\t")
df_mat_age <- read.csv('data/maternal_deaths_age.txt', sep = "\t")
df_mat_race <- read.csv('data/maternal_deaths_race.txt', sep = "\t")

load(file="data/natality_yearly_clean.Rda")
load(file="data/natality_age_clean.Rda")
load(file="data/natality_race_clean.Rda")
load(file="data/natality_state_clean.Rda")

## Data cleaning - Year
df_mat_year2 = subset(df_mat_year, select = -c(Notes, Year.Code, 
                                               Population, Crude.Rate)) %>% 
  filter(df_mat_year$Year != "") %>%
  na.omit()
  

df_mat_year3 <- merge(df_mat_year2, df_nat_year, by='Year', all.x = TRUE)
df_mat_year3[(df_mat_year3$Year == '2022 (provisional)'), 
             'Births'] = df_mat_year3$Births[df_mat_year3$Year == '2021']
  
df_mat_year3$Deaths.by.Births = (df_mat_year3$Deaths*100000)/df_mat_year3$Births

## Data cleaning - State
df_mat_state2 = subset(df_mat_state, select = -c(Notes, Population, Crude.Rate)) %>% 
  na.omit()

df_mat_state3 <- merge(df_mat_state2, df_nat_state, 
                       by.x='Residence.State', by.y='State')

df_mat_state3$Deaths.by.Births = (df_mat_state3$Deaths*100000)/df_mat_state3$Births

## Data cleaning - Age
df_mat_age2 = subset(df_mat_age, select = -c(Notes, Population, Crude.Rate)) %>% 
  na.omit() %>% 
  filter(Five.Year.Age.Groups != "") 

df_mat_age3 <- merge(df_mat_age2, df_nat_age, 
                     by.x='Five.Year.Age.Groups.Code',
                     by.y='Age.of.Mother.9.Code')

df_mat_age3$Deaths.by.Births = (df_mat_age3$Deaths*100000)/df_mat_age3$Births

## Data cleaning - Race
df_mat_race2 = subset(df_mat_race, 
                      select = -c(Notes, Crude.Rate, 
                                  Single.Race.6.Code, Population,
                                  Hispanic.Origin.Code)) %>% 
  na.omit() %>%
  filter((Single.Race.6 != '') & (Deaths != 'Suppressed'))

df_mat_race2[(df_mat_race2$Hispanic.Origin == 'Hispanic or Latino') &
             (df_mat_race2$Single.Race.6 == 'Black or African American'), 
             'Single.Race.6'] = 'Hispanic'

df_mat_race2[(df_mat_race2$Hispanic.Origin == 'Not Hispanic or Latino') &
             (df_mat_race2$Single.Race.6 == 'Black or African American'), 
             'Single.Race.6'] = 'Non-Hispanic Black'

df_mat_race2[(df_mat_race2$Hispanic.Origin == 'Hispanic or Latino') &
               (df_mat_race2$Single.Race.6 == 'White'), 
             'Single.Race.6'] = 'Hispanic'

df_mat_race2[(df_mat_race2$Hispanic.Origin == 'Not Hispanic or Latino') &
               (df_mat_race2$Single.Race.6 == 'White'), 
             'Single.Race.6'] = 'Non-Hispanic White'

df_mat_race2[df_mat_race2$Single.Race.6 == 'American Indian or Alaska Native',
             'Single.Race.6'] = "Asian or Pacific Islander"

df_mat_race2[df_mat_race2$Single.Race.6 == 'Native Hawaiian or Other Pacific Islander',
             'Single.Race.6'] = "Asian or Pacific Islander"

df_mat_race2[df_mat_race2$Single.Race.6 == 'Asian',
             'Single.Race.6'] = "Asian or Pacific Islander"

df_mat_race2 <- df_mat_race2 %>% 
  mutate_at('Deaths', as.numeric) %>%
  group_by(Single.Race.6) %>% 
  summarise(Deaths=sum(Deaths))

df_mat_race3 <- merge(df_mat_race2, df_nat_race,
                      by.x = 'Single.Race.6',
                      by.y = 'Mother.s.Bridged.Race')

df_mat_race3$Deaths.by.Births = (df_mat_race3$Deaths*100000)/df_mat_race3$Births

# visualisations ---------------------------------------------------------

df_mat_year3 %>%
  ggplot(aes(x=Year, y=Deaths.by.Births)) +
  geom_bar(stat="identity", fill="steelblue") +
  geom_text(aes(label=Deaths), vjust=-0.3, color="black", size=3.5) +
  theme_minimal() + 
  labs(y = "Rate per 100,000 Live Births", 
       title = "Rates of Maternal Deaths by Year (2018-2022)",
       subtitle = "Count of Deaths Above Each Bar",
       caption = "Note: 2022 counts are provisional. We use 2021 live births to compute the 2022 mortality rate as the 2022 live birth count\nis not available.") + 
  theme(plot.caption=element_text(hjust = 0))
ggsave("figs/plt_mat_year.png")

df_mat_age3 %>%
  ggplot(aes(x=Five.Year.Age.Groups.Code, y=Deaths.by.Births)) +
  geom_bar(stat="identity", fill="steelblue") +
  geom_text(aes(label=Deaths), vjust=-0.3, color="black", size=3.5) +
  theme_minimal() + 
  labs(y = "Rate per 100,000 Live Births", 
       x = "Age Groups",
       title = "Rates of Maternal Deaths by Age (2018-2021)",
       subtitle = "Count of Deaths Above Each Bar") 
ggsave("figs/plt_mat_age.png")

df_mat_race3 %>%
  ggplot(aes(x=Single.Race.6, y=Deaths.by.Births)) +
  geom_bar(stat="identity", fill="steelblue") +
  geom_text(aes(label=Deaths), vjust=-0.3, color="black", size=3.5) +
  theme_minimal() + 
  labs(y = "Rate per 100,000 Live Births", 
       x = "Racial/Ethnic Group",
       title = "Rates of Maternal Deaths by Racial/Ethnic Group (2018-2021)",
       subtitle = "Count of Deaths Above Each Bar") +
  theme(axis.text.x = element_text(angle = 80, hjust=1)) 
ggsave("figs/plt_mat_race.png")

national_avg = (sum(df_mat_state3$Deaths)*100000)/sum(df_mat_state3$Births)

df_mat_state3 %>%
  mutate(Residence.State = fct_reorder(Residence.State, 
                                       desc(Deaths.by.Births))) %>%
  ggplot(aes(x=Residence.State, y=Deaths.by.Births)) +
  geom_bar(stat="identity", fill="steelblue") + coord_flip() +
  geom_hline(yintercept = national_avg, color = "red") +
  theme_minimal() + 
  labs(y = "Rate per 100,000 Live Births", 
       x = "State",
       title = "Rates of Maternal Deaths by State (2018-2021)",
       subtitle = "National Average Rate in Red (5.1)") 
ggsave("figs/plt_mat_state.png")

