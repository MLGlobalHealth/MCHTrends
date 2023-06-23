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

## Data cleaning - Year
df_mat_year2 = subset(df_mat_year, select = -c(Notes)) %>% 
  na.omit() %>% 
  filter(Year != "2023 (provisional and partial)") 

df_mat_year2[df_mat_year2$Year == '2022 (provisional)', 'Year'] = '2022'

## Data cleaning - State
df_mat_state2 = subset(df_mat_state, select = -c(Notes, Crude.Rate)) %>% 
  na.omit()

df_mat_state2$Calc.Rate = round(
  (df_mat_state2$Deaths*100000) / (df_mat_state2$Population), digits=1)

## Data cleaning - Age
df_mat_age2 = subset(df_mat_age, select = -c(Notes)) %>% 
  na.omit() %>% 
  filter(Five.Year.Age.Groups != "") 

## Data cleaning - Race
df_mat_race2 = subset(df_mat_race, 
                      select = -c(Notes, Crude.Rate,
                                  Single.Race.6.Code,
                                  Hispanic.Origin.Code)) %>% 
  na.omit() 

df_mat_race2[(df_mat_race2$Hispanic.Origin == 'Hispanic or Latino') &
             (df_mat_race2$Single.Race.6 == 'Black or African American'), 
             'Single.Race.6'] = 'Black Hispanic'

df_mat_race2[(df_mat_race2$Hispanic.Origin == 'Not Hispanic or Latino') &
             (df_mat_race2$Single.Race.6 == 'Black or African American'), 
             'Single.Race.6'] = 'Black Non-Hispanic'

df_mat_race2[(df_mat_race2$Hispanic.Origin == 'Hispanic or Latino') &
               (df_mat_race2$Single.Race.6 == 'White'), 
             'Single.Race.6'] = 'White Hispanic'

df_mat_race2[(df_mat_race2$Hispanic.Origin == 'Not Hispanic or Latino') &
               (df_mat_race2$Single.Race.6 == 'White'), 
             'Single.Race.6'] = 'White Non-Hispanic'

df_mat_race2[df_mat_race2$Single.Race.6 == 'American Indian or Alaska Native',
             'Single.Race.6'] = "Native American/Alaskan"

df_mat_race2[df_mat_race2$Single.Race.6 == 'Native Hawaiian or Other Pacific Islander',
             'Single.Race.6'] = "Native Hawaiian/Other PI"

df_mat_race2 <- df_mat_race2 %>% 
  group_by(Single.Race.6) %>% 
  summarise(Deaths=sum(Deaths), Population=sum(Population))
  
  
df_mat_race2$Calc.Rate = round(
  (df_mat_race2$Deaths*100000) / (df_mat_race2$Population), digits=1)

df_mat_race2 <- df_mat_race2 %>% arrange(desc(Calc.Rate))

# visualisations ---------------------------------------------------------

df_mat_year2 %>%
  ggplot(aes(x=Year, y=Crude.Rate)) +
  geom_bar(stat="identity", fill="steelblue") +
  geom_text(aes(label=Deaths), vjust=-0.3, color="black", size=3.5) +
  theme_minimal() + 
  labs(y = "Rate per 100,000", 
       title = "Rates of Maternal Deaths by Year (2018-2022)",
       subtitle = "Count of Maternal Deaths Above Each Bar") 
ggsave("figs/plt_mat_year.png")

df_mat_age2 %>%
  ggplot(aes(x=Five.Year.Age.Groups.Code, y=Crude.Rate)) +
  geom_bar(stat="identity", fill="steelblue") +
  geom_text(aes(label=Deaths), vjust=-0.3, color="black", size=3.5) +
  theme_minimal() + 
  labs(y = "Rate per 100,000", 
       x = "Age Groups",
       title = "Rates of Maternal Deaths by Age (2018-2022)",
       subtitle = "Count of Maternal Deaths Above Each Bar") 
ggsave("figs/plt_mat_age.png")

df_mat_race2 %>%
  ggplot(aes(x=Single.Race.6, y=Calc.Rate)) +
  geom_bar(stat="identity", fill="steelblue") +
  geom_text(aes(label=Deaths), vjust=-0.3, color="black", size=3.5) +
  theme_minimal() + 
  labs(y = "Rate per 100,000", 
       x = "Race",
       title = "Rates of Maternal Deaths by Race (2018-2022)",
       subtitle = "Count of Maternal Deaths Above Each Bar") +
  theme(axis.text.x = element_text(angle = 80, hjust=1)) 
ggsave("figs/plt_mat_race.png")

national_avg = sum(df_mat_state2$Deaths)*100000/sum(df_mat_state2$Population)

df_mat_state2 %>%
  ggplot(aes(x=Residence.State, y=Calc.Rate)) +
  geom_bar(stat="identity", fill="steelblue") +
  geom_hline(yintercept = national_avg, color = "red") +
  theme_minimal() + 
  labs(y = "Rate per 100,000", 
       x = "State",
       title = "Rates of Maternal Deaths by State (2018-2022)",
       subtitle = "National Rate in Red (1.5)") +
  theme(axis.text.x = element_text(angle = 90, vjust=0.3, hjust=1)) 
ggsave("figs/plt_mat_state.png")

