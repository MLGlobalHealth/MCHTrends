# load packages -----------------------------------------------------------

## Package names
packages <- c("tidyverse","ggplot2","dplyr","hexbin",
              "data.table","geojsonio","RColorBrewer",
              "broom")

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
  df <- read.csv(str_interp('data/${fname}.txt'), sep='\t') %>%
    filter((Notes != 'Total')) %>%
    subset(select = -c(Notes, Population, Crude.Rate)) %>%
    na.omit() 
}

df_inf1 <- clean_df('infant_mortality_census_00_10')
df_inf2a <- clean_df('infant_mortality_census_11_17')
df_inf2b <- clean_df('infant_mortality_census_18_19') %>%
  rename(
    Census.Region = Residence.Census.Region,
    Census.Region.Code = Residence.Census.Region.Code
  )
df_inf_covid <- clean_df('infant_mortality_census_20_22') %>%
  rename(
    Census.Region = Residence.Census.Region,
    Census.Region.Code = Residence.Census.Region.Code
  )

concat_df <- function(df2a, df2b) {
  df <- rbind(df2a, df2b) %>%
    group_by(Census.Region, Census.Region.Code) %>%
    summarise(Deaths=sum(Deaths))
}

df_inf2 <- concat_df(df_inf2a, df_inf2b)

clean_nat_df <- function(fname) {
  df_nat <- read.csv(str_interp('data/${fname}.txt'), sep='\t') %>%
    filter((Notes != 'Total')) %>%
    subset(select = -c(Notes)) %>%
    na.omit() %>%
    group_by(Year, Census.Region, Census.Region.Code) %>%
    summarise(Births=sum(Births))
}

df_nat1 <- clean_nat_df('natality_age_year_census_01_02') 
df_nat2 <- clean_nat_df('natality_age_year_census_03_06') 
df_nat3 <- clean_nat_df('natality_age_year_census_07_22') 

df_nat <- rbind(df_nat1, df_nat2, df_nat3)
df_nat_00_10 <- df_nat %>% 
  filter(Year < 2011) %>%
  group_by(Census.Region, Census.Region.Code) %>%
  summarise(Births=sum(Births))
df_nat_11_19 <- df_nat %>% 
  filter((Year > 2010) & (Year < 2020)) %>%
  group_by(Census.Region, Census.Region.Code) %>%
  summarise(Births=sum(Births))
df_nat_covid <- df_nat %>% 
  filter(Year > 2019) %>%
  group_by(Census.Region, Census.Region.Code) %>%
  summarise(Births=sum(Births))

mrg_births <- function(df_mort, df_nat) {
  df_mort <- merge(df_mort, df_nat, 
                  by=c('Census.Region',
                       'Census.Region.Code'))
}

df_inf1 <- mrg_births(df_inf1, df_nat_00_10)
df_inf2 <- mrg_births(df_inf2, df_nat_11_19)
df_inf_covid <- mrg_births(df_inf_covid, df_nat_covid)

create_crude <- function(df, rate, label) {
  df_new <- df %>% group_by(Census.Region) %>% 
    summarise(Deaths=sum(Deaths), Births=sum(Births)) %>%
    mutate({{rate}} := Deaths/Births*1000) %>%
    mutate(Type := {{label}}) # %>%
    # subset(select = -c(Births, Deaths))
}

df_crude_inf1 <- create_crude(df_inf1, Rate.01.10, 'Infant')
df_crude_inf2 <- create_crude(df_inf2, Rate.11.19, 'Infant')
df_crude_inf <- merge(df_crude_inf1, df_crude_inf2, by=c('Census.Region', 'Type'))

df_crude_inf_covid <- create_crude(df_inf_covid, Rate.Covid, 'Infant')
df_crude_inf_covid_chg <- merge(df_crude_inf2, df_crude_inf_covid, by=c('Census.Region', 'Type'))

df_crude_inf$Pct.Change = (df_crude_inf$Rate.11.19-df_crude_inf$Rate.01.10)/df_crude_inf$Rate.01.10*100
df_crude_inf_covid_chg$Pct.Change = (df_crude_inf_covid_chg$Rate.Covid-df_crude_inf_covid_chg$Rate.11.19)/df_crude_inf_covid_chg$Rate.11.19*100

library(stringr)

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

byars_conf_interval <- function(census, x, n, mult=1000, alpha=0.05) {
  O <- x
  z <- qnorm(1 - alpha/2)
  lower <- (O*(1 - (1/(9*O)) - (z/(3*sqrt(O))))**3)/n
  upper <- ((O+1)*(1 - (1/(9*(O+1))) + (z/(3*sqrt(O+1))))**3)/n
  return(data.frame(Census.Region = census, lower = as.numeric(lower*mult), upper = as.numeric(upper*mult)))
}

df_ci_00_10 <- distinct(byars_conf_interval(df_crude_long$Census.Region, df_crude_long$Deaths.x, df_crude_long$Births.x, 1000))
df_ci_00_10$time_period <- 'Rate.01.10'

df_ci_11_19 <- distinct(byars_conf_interval(df_crude_long$Census.Region, df_crude_long$Deaths.y, df_crude_long$Births.y, 1000))
df_ci_11_19$time_period <- 'Rate.11.19'

df_crude_long_mrg1 <- merge(df_crude_long, df_ci_00_10, by = c("time_period",'Census.Region'))
df_crude_long_mrg2 <- merge(df_crude_long, df_ci_11_19, by = c("time_period",'Census.Region'))
df_crude_long <- rbind(df_crude_long_mrg1,df_crude_long_mrg2)

df_ci_covid <- distinct(byars_conf_interval(df_crude_covid_long$Census.Region, df_crude_covid_long$Deaths.y, df_crude_covid_long$Births.y, 1000))
df_ci_covid$time_period <- 'Rate.Covid'
df_crude_long_mrg3 <- merge(df_crude_covid_long, df_ci_covid, by = c("time_period",'Census.Region'))
df_crude_covid_long <- rbind(df_crude_long_mrg2,df_crude_long_mrg3)

df_crude_all <- distinct(rbind(df_crude_long, df_crude_covid_long))
df_crude_all <- df_crude_all %>%
  mutate(Period = case_when(
    time_period == 'Rate.01.10' ~ "2001-2010",
    time_period == 'Rate.11.19' ~ "2011-2019",
    time_period == 'Rate.Covid' ~ "2020-2022",
  ))

cbPalette <- c("#CC79A7", "#0072B2", "#009E73", "#E69F00", "#D55E00", "#56B4E9", "#F0E442", "#999999")

ggplot(df_crude_all, aes(x = Census.Region, y = value, fill = Period)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  geom_errorbar(aes(ymin = lower, ymax = upper), 
                position = position_dodge(width = 0.9), 
                width = 0.25) +
  labs(x = "Census Region",
       y = "Rate per 1,000 Live Births") +
  scale_color_manual(values = cbPalette) + scale_fill_manual(values = cbPalette) + 
  theme_minimal() +
  theme(axis.title.x=element_blank())
ggsave("figs/plt_pct_chg_cause_spec_inf_census_crude.png")


# df_crude_inf %>%
#   mutate(Region = word(Census.Region,-1)) %>%
#   ggplot(aes(x=Region, y=Pct.Change)) +
#   geom_bar(stat="identity", position=position_dodge(), fill="red") +
#   theme_minimal() + 
#   labs(y = "% Change in Rates per 1,000 Live Births", 
#        x = "Census Region",
#        title = "Percent Change in Rates of Infant Deaths",
#        subtitle = "2000-2010 vs. 2011-2019") + 
#   theme(plot.caption=element_text(hjust = 0)) + guides(fill=guide_legend(title="")) +
#   theme(axis.text.x = element_text(angle = 80))
# ggsave("figs/plt_pct_chg_inf_census_crude.png")
# 
# df_crude_inf_covid_chg %>%
#   mutate(Region = word(Census.Region,-1)) %>%
#   ggplot(aes(x=Region, y=Pct.Change)) +
#   geom_bar(stat="identity", position=position_dodge(), fill="red") +
#   theme_minimal() + 
#   labs(y = "% Change in Rates per 1,000 Live Births", 
#        x = "Census Region",
#        title = "Percent Change in Rates of Infant Deaths",
#        subtitle = "2011-2019 vs. 2020-2022") + 
#   theme(plot.caption=element_text(hjust = 0)) + guides(fill=guide_legend(title="")) +
#   theme(axis.text.x = element_text(angle = 80, vjust=.75))
# ggsave("figs/plt_pct_chg_inf_census_crude_covid.png")
