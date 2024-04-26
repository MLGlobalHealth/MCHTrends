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

df_inf1 <- clean_df('infant_mortality_race_00_10')
df_inf2a <- clean_df('infant_mortality_race_11_17')
df_inf2b <- clean_df('infant_mortality_race_18_19') %>%
  rename(Race = Single.Race.6, Race.Code = Single.Race.6.Code)
df_inf_covid <- clean_df('infant_mortality_race_20_22') %>%
  rename(Race = Single.Race.6, Race.Code = Single.Race.6.Code)

load(file="data/natality_race_00_10_clean.Rda")
load(file="data/natality_race_11_19_clean.Rda")
load(file="data/natality_race_20_22_clean.Rda")

concat_df <- function(df2a, df2b) {
  df <- rbind(df2a, df2b) %>%
    group_by(Race, Race.Code, 
             Hispanic.Origin, Hispanic.Origin.Code) %>%
    summarise(Deaths=sum(Deaths))
}

df_inf2 <- concat_df(df_inf2a, df_inf2b)

mrg_births <- function(df_mort, df_nat) {
  df_mort[df_mort$Race == 'Native Hawaiian or Other Pacific Islander',
          'Race'] = 'Asian or Pacific Islander'
  df_mort[df_mort$Race == 'Asian','Race'] = 'Asian or Pacific Islander'
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
  df_mort <- df_mort %>% filter(Race != '') %>%
    group_by(Race) %>% summarise(Deaths=sum(Deaths)) %>%
    rename(Mother.s.Race = Race)
  
  df_mort_cln_race <- merge(df_mort, df_nat, 
                  by=c('Mother.s.Race'))
}

df_inf1 <- mrg_births(df_inf1, df_nat_race_00_10)
df_inf2 <- mrg_births(df_inf2, df_nat_race_11_19)
df_inf_covid <- mrg_births(df_inf_covid, df_nat_race_20_22)

create_crude <- function(df, rate, label) {
  df_new <- df %>% 
    mutate({{rate}} := Deaths/Births*1000) %>%
    mutate(Type := {{label}}) # %>%
    # subset(select = -c(Births, Deaths))
}

df_crude_inf1 <- create_crude(df_inf1, Rate.00.10, 'Infant Mortality')
df_crude_inf2 <- create_crude(df_inf2, Rate.11.19, 'Infant Mortality')
df_crude_inf <- merge(df_crude_inf1, df_crude_inf2, by=c('Mother.s.Race', 'Type'))

df_crude_inf_covid <- create_crude(df_inf_covid, Rate.Covid, 'Infant Mortality')
df_crude_inf_covid_chg <- merge(df_crude_inf2, df_crude_inf_covid, by=c('Mother.s.Race', 'Type'))

df_crude_inf$Pct.Change = (df_crude_inf$Rate.11.19-df_crude_inf$Rate.00.10)/df_crude_inf$Rate.00.10*100
df_crude_inf_covid_chg$Pct.Change = (df_crude_inf_covid_chg$Rate.Covid-df_crude_inf_covid_chg$Rate.11.19)/df_crude_inf_covid_chg$Rate.11.19*100

# compute CIs ---------------------------------------------------------

df_crude_long <- pivot_longer(
  df_crude_inf, cols = starts_with("Rate"), 
  names_to = "time_period", 
  values_to = "value") 

df_crude_covid_long <- pivot_longer(
  df_crude_inf_covid_chg, 
  cols = starts_with("Rate"), 
  names_to = "time_period", 
  values_to = "value")

byars_conf_interval <- function(race, x, n, mult=1000, alpha=0.05) {
  O <- x
  z <- qnorm(1 - alpha/2)
  lower <- (O*(1 - (1/(9*O)) - (z/(3*sqrt(O))))**3)/n
  upper <- ((O+1)*(1 - (1/(9*(O+1))) + (z/(3*sqrt(O+1))))**3)/n
  return(data.frame(Mother.s.Race = race, lower = as.numeric(lower*mult), upper = as.numeric(upper*mult)))
}

df_ci_00_10 <- distinct(byars_conf_interval(df_crude_long$Mother.s.Race, df_crude_long$Deaths.x, df_crude_long$Births.x, 1000))
df_ci_00_10$time_period <- 'Rate.00.10'

df_ci_11_19 <- distinct(byars_conf_interval(df_crude_long$Mother.s.Race, df_crude_long$Deaths.y, df_crude_long$Births.y, 1000))
df_ci_11_19$time_period <- 'Rate.11.19'

df_crude_long_mrg1 <- merge(df_crude_long, df_ci_00_10, by = c("time_period",'Mother.s.Race'))
df_crude_long_mrg2 <- merge(df_crude_long, df_ci_11_19, by = c("time_period",'Mother.s.Race'))
df_crude_long <- rbind(df_crude_long_mrg1,df_crude_long_mrg2)

df_ci_covid <- distinct(byars_conf_interval(df_crude_covid_long$Mother.s.Race, df_crude_covid_long$Deaths.y, df_crude_covid_long$Births.y, 1000))
df_ci_covid$time_period <- 'Rate.Covid'
df_crude_long_mrg3 <- merge(df_crude_covid_long, df_ci_covid, by = c("time_period",'Mother.s.Race'))
df_crude_covid_long <- rbind(df_crude_long_mrg2,df_crude_long_mrg3)

df_crude_all <- distinct(rbind(df_crude_long, df_crude_covid_long))
df_crude_all <- df_crude_all %>%
  mutate(Period = case_when(
    time_period == 'Rate.00.10' ~ "2000-2010",
    time_period == 'Rate.11.19' ~ "2011-2019",
    time_period == 'Rate.Covid' ~ "2020-2022",
  ))

cbPalette <- c("#CC79A7", "#0072B2", "#009E73", "#E69F00", "#D55E00", "#56B4E9", "#F0E442", "#999999")

ggplot(df_crude_all, aes(x = Mother.s.Race, y = value, fill = Period)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  geom_errorbar(aes(ymin = lower, ymax = upper), 
                position = position_dodge(width = 0.9), 
                width = 0.25) +
  labs(x = "Mother's Racial/Ethnic Group",
       y = "Rate per 1,000 Live Births") +
  scale_color_manual(values = cbPalette) + scale_fill_manual(values = cbPalette) + 
  theme_minimal()
ggsave("figs/plt_pct_chg_cause_spec_inf_race_crude.png")

# df_crude_inf %>%
#   ggplot(aes(x=Mother.s.Race, y=Pct.Change)) +
#   geom_bar(stat="identity", position=position_dodge(), fill="red") +
#   theme_minimal() + 
#   labs(y = "% Change in Rates per 1,000 Live Births", 
#        x = "Racial/Ethnic Group",
#        title = "Percent Change in Rates of Infant Mortality",
#        subtitle = "2000-2010 vs. 2011-2019") + 
#   theme(plot.caption=element_text(hjust = 0)) + guides(fill=guide_legend(title="")) +
#   #theme(axis.text.x = element_text(angle = 60, vjust=0.6)) 
#   coord_flip()
# ggsave("figs/plt_census_pct_chg_inf_race_crude.png")
# 
# df_crude_inf_covid_chg %>%
#   ggplot(aes(x=Mother.s.Race, y=Pct.Change, fill=Type)) +
#   geom_bar(stat="identity", position=position_dodge(), fill="red") +
#   theme_minimal() + 
#   labs(y = "% Change in Rates per 1,000 Live Births", 
#        x = "Racial/Ethnic Group",
#        title = "Percent Change in Rates of Infant Deaths",
#        subtitle = "2011-2019 vs. 2020-2022") + 
#   theme(plot.caption=element_text(hjust = 0)) + guides(fill=guide_legend(title="")) +
#   #theme(axis.text.x = element_text(angle = 80, vjust=.75))
#   coord_flip()
# ggsave("figs/plt_census_pct_chg_inf_race_crude_covid.png")
