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

df_mm1 <- clean_df('maternal_mortality_age_00_10')
df_mm2a <- clean_df('maternal_mortality_age_11_17')
df_mm2b <- clean_df('maternal_mortality_age_18_19')
df_mm_covid <- clean_df('maternal_mortality_age_20_22')

df_mmno1 <- clean_df('maternal_mortality_spec_age_00_10')
df_mmno2a <- clean_df('maternal_mortality_spec_age_11_17')
df_mmno2b <- clean_df('maternal_mortality_spec_age_18_19')
df_mmno_covid <- clean_df('maternal_mortality_spec_age_20_22') 

df_mmot1 <- clean_df('other_mat_mortality_age_00_10')
df_mmot2a <- clean_df('other_mat_mortality_age_11_17')
df_mmot2b <- clean_df('other_mat_mortality_age_18_19')
df_mmot_covid <- clean_df('other_mat_mortality_age_20_22')

df_pr1 <- clean_df('pregrel_mortality_age_00_10')
df_pr2a <- clean_df('pregrel_mortality_age_11_17')
df_pr2b <- clean_df('pregrel_mortality_age_18_19')
df_pr_covid <- clean_df('pregrel_mortality_age_20_22') 

concat_df <- function(df2a, df2b) {
  df <- rbind(df2a, df2b) %>%
    group_by(Five.Year.Age.Groups, Five.Year.Age.Groups.Code) %>%
    summarise(Deaths=sum(Deaths))
}

df_mm2 <- concat_df(df_mm2a, df_mm2b)
df_mmno2 <- concat_df(df_mmno2a, df_mmno2b)
df_mmot2 <- concat_df(df_mmot2a, df_mmot2b)
df_pr2 <- concat_df(df_pr2a, df_pr2b)

clean_nat_df <- function(fname, age_var, age_var_code) {
  df_nat <- read.csv(str_interp('data/${fname}.txt'), sep='\t') %>%
    filter(Notes != 'Total') %>%
    subset(select = -c(Notes)) %>%
    na.omit() %>% 
    rename(
      Five.Year.Age.Groups = {{age_var}},
      Five.Year.Age.Groups.Code = {{age_var_code}}
    ) 
}

df_nat1 <- clean_nat_df('natality_age_year_census_01_02', 
                        Age.of.Mother, Age.of.Mother.Code) 
df_nat2 <- clean_nat_df('natality_age_year_census_03_06', 
                        Age.of.Mother.9, Age.of.Mother.9.Code) 
df_nat3 <- clean_nat_df('natality_age_year_census_07_22', 
                        Age.of.Mother.9, Age.of.Mother.9.Code) %>%
  subset(select = -c(Female.Population, Fertility.Rate))

df_nat <- rbind(df_nat1, df_nat2, df_nat3)
df_nat_00_10 <- df_nat %>% 
  filter(Year < 2011) %>%
  group_by(Five.Year.Age.Groups, Five.Year.Age.Groups.Code) %>%
  summarise(Births=sum(Births))
df_nat_11_19 <- df_nat %>% 
  filter((Year > 2010) & (Year < 2020)) %>%
  group_by(Five.Year.Age.Groups, Five.Year.Age.Groups.Code) %>%
  summarise(Births=sum(Births))
df_nat_covid <- df_nat %>% 
  filter(Year > 2019) %>%
  group_by(Five.Year.Age.Groups, Five.Year.Age.Groups.Code) %>%
  summarise(Births=sum(Births))

mrg_births <- function(df_mort, df_nat) {
  df_mort <- merge(df_mort, df_nat, 
                  by=c('Five.Year.Age.Groups','Five.Year.Age.Groups.Code'))
}

df_mm1 <- mrg_births(df_mm1, df_nat_00_10)
df_mm2 <- mrg_births(df_mm2, df_nat_11_19)
df_mm_covid <- mrg_births(df_mm_covid, df_nat_covid)

df_mmno1 <- mrg_births(df_mmno1, df_nat_00_10)
df_mmno2 <- mrg_births(df_mmno2, df_nat_11_19)
df_mmno_covid <- mrg_births(df_mmno_covid, df_nat_covid)

df_mmot1 <- mrg_births(df_mmot1, df_nat_00_10)
df_mmot2 <- mrg_births(df_mmot2, df_nat_11_19)
df_mmot_covid <- mrg_births(df_mmot_covid, df_nat_covid)

df_pr1 <- mrg_births(df_pr1, df_nat_00_10)
df_pr2 <- mrg_births(df_pr2, df_nat_11_19)
df_pr_covid <- mrg_births(df_pr_covid, df_nat_covid)

create_crude <- function(df, rate, label) {
  df_new <- df %>% group_by(Five.Year.Age.Groups) %>% 
    summarise(Deaths=sum(Deaths), Births=sum(Births)) %>%
    mutate({{rate}} := Deaths/Births*100000) %>%
    mutate(Type := {{label}}) # %>%
    # subset(select = -c(Births, Deaths))
}

df_crude_mm1 <- create_crude(df_mm1, Rate.01.10, 'Maternal')
df_crude_mm2 <- create_crude(df_mm2, Rate.11.19, 'Maternal')
df_crude_mm <- merge(df_crude_mm1, df_crude_mm2, by=c('Five.Year.Age.Groups', 'Type'))

df_crude_mm_covid <- create_crude(df_mm_covid, Rate.Covid, 'Maternal')
df_crude_mm_covid_chg <- merge(df_crude_mm2, df_crude_mm_covid, by=c('Five.Year.Age.Groups', 'Type'))

df_crude_mmno1 <- create_crude(df_mmno1, Rate.01.10, 'Cause-Specific Maternal')
df_crude_mmno2 <- create_crude(df_mmno2, Rate.11.19, 'Cause-Specific Maternal')
df_crude_mmno <- merge(df_crude_mmno1, df_crude_mmno2, by=c('Five.Year.Age.Groups', 'Type'))

df_crude_mmno_covid <- create_crude(df_mmno_covid, Rate.Covid, 'Cause-Specific Maternal')
df_crude_mmno_covid_chg <- merge(df_crude_mmno2, df_crude_mmno_covid, by=c('Five.Year.Age.Groups', 'Type'))

df_crude_mmot1 <- create_crude(df_mmot1, Rate.01.10, 'Unspecified Maternal')
df_crude_mmot2 <- create_crude(df_mmot2, Rate.11.19, 'Unspecified Maternal')
df_crude_mmot <- merge(df_crude_mmot1, df_crude_mmot2, by=c('Five.Year.Age.Groups', 'Type'))

df_crude_mmot_covid <- create_crude(df_mmot_covid, Rate.Covid, 'Unspecified Maternal')
df_crude_mmot_covid_chg <- merge(df_crude_mmot2, df_crude_mmot_covid, by=c('Five.Year.Age.Groups', 'Type'))

df_crude_pr1 <- create_crude(df_pr1, Rate.01.10, 'Late Maternal')
df_crude_pr2 <- create_crude(df_pr2, Rate.11.19, 'Late Maternal')
df_crude_pr <- merge(df_crude_pr1, df_crude_pr2, by=c('Five.Year.Age.Groups', 'Type'))

df_crude_pr_covid <- create_crude(df_pr_covid, Rate.Covid, 'Late Maternal')
df_crude_pr_covid_chg <- merge(df_crude_pr2, df_crude_pr_covid, by=c('Five.Year.Age.Groups', 'Type'))

df_crude <- rbind(df_crude_mm, df_crude_mmno, df_crude_mmot, df_crude_pr)
df_crude$Pct.Change = (df_crude$Rate.11.19-df_crude$Rate.01.10)/df_crude$Rate.01.10*100

df_crude_covid <- rbind(df_crude_mm_covid_chg, df_crude_mmno_covid_chg, df_crude_mmot_covid_chg, df_crude_pr_covid_chg)
df_crude_covid$Pct.Change = (df_crude_covid$Rate.Covid-df_crude_covid$Rate.11.19)/df_crude_covid$Rate.11.19*100

# compute CIs ---------------------------------------------------------

df_crude_long <- pivot_longer(
  df_crude, cols = starts_with("Rate"), 
  names_to = "time_period", 
  values_to = "value") 

df_crude_long <- df_crude_long %>% filter(Type=='Cause-Specific Maternal')

df_crude_covid_long <- pivot_longer(
  df_crude_covid, 
  cols = starts_with("Rate"), 
  names_to = "time_period", 
  values_to = "value")

df_crude_covid_long <- df_crude_covid_long %>% filter(Type=='Cause-Specific Maternal')

byars_conf_interval <- function(age, x, n, mult=100000, alpha=0.05) {
  O <- x
  z <- qnorm(1 - alpha/2)
  lower <- (O*(1 - (1/(9*O)) - (z/(3*sqrt(O))))**3)/n
  upper <- ((O+1)*(1 - (1/(9*(O+1))) + (z/(3*sqrt(O+1))))**3)/n
  return(data.frame(Five.Year.Age.Groups = age, lower = as.numeric(lower*mult), upper = as.numeric(upper*mult)))
}

df_ci_00_10 <- distinct(byars_conf_interval(df_crude_long$Five.Year.Age.Groups, df_crude_long$Deaths.x, df_crude_long$Births.x, 100000))
df_ci_00_10$time_period <- 'Rate.01.10'

df_ci_11_19 <- distinct(byars_conf_interval(df_crude_long$Five.Year.Age.Groups, df_crude_long$Deaths.y, df_crude_long$Births.y, 100000))
df_ci_11_19$time_period <- 'Rate.11.19'

df_crude_long_mrg1 <- merge(df_crude_long, df_ci_00_10, by = c("time_period",'Five.Year.Age.Groups'))
df_crude_long_mrg2 <- merge(df_crude_long, df_ci_11_19, by = c("time_period",'Five.Year.Age.Groups'))
df_crude_long <- rbind(df_crude_long_mrg1,df_crude_long_mrg2)

df_ci_covid <- distinct(byars_conf_interval(df_crude_covid_long$Five.Year.Age.Groups, df_crude_covid_long$Deaths.y, df_crude_covid_long$Births.y, 100000))
df_ci_covid$time_period <- 'Rate.Covid'
df_crude_long_mrg3 <- merge(df_crude_covid_long, df_ci_covid, by = c("time_period",'Five.Year.Age.Groups'))
df_crude_covid_long <- rbind(df_crude_long_mrg2,df_crude_long_mrg3)

df_crude_all <- distinct(rbind(df_crude_long, df_crude_covid_long))
df_crude_all <- df_crude_all %>%
  mutate(Period = case_when(
    time_period == 'Rate.01.10' ~ "2001-2010",
    time_period == 'Rate.11.19' ~ "2011-2019",
    time_period == 'Rate.Covid' ~ "2020-2022",
  ))

cbPalette <- c("#CC79A7", "#0072B2", "#009E73", "#E69F00", "#D55E00", "#56B4E9", "#F0E442", "#999999")

ggplot(df_crude_all, aes(x = Five.Year.Age.Groups, y = value, fill = Period)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  geom_errorbar(aes(ymin = lower, ymax = upper), 
                position = position_dodge(width = 0.9), 
                width = 0.25) +
  labs(x = "Five Year Age Groups",
       y = "Rate per 100,000 Live Births") +
  scale_color_manual(values = cbPalette) + scale_fill_manual(values = cbPalette) + 
  theme_minimal() 
ggsave("figs/plt_pct_chg_cause_spec_mat_age_crude.png")
ggsave("figs/plt_pct_chg_cause_spec_mat_age_crude.svg")



# 
# df_crude %>%
#   ggplot(aes(x=Five.Year.Age.Groups, y=Pct.Change, fill=Type)) +
#   geom_bar(stat="identity", position=position_dodge()) +
#   theme_minimal() + 
#   labs(y = "% Change in Rates per 100,000 Live Births", 
#        x = "Mother's Age Groups",
#        title = "Percent Change in Rates of Maternal Deaths",
#        subtitle = "2000-2010 vs. 2011-2019") + 
#   theme(plot.caption=element_text(hjust = 0)) + guides(fill=guide_legend(title="")) +
#   theme(axis.text.x = element_text(angle = 80, vjust=.6))
# ggsave("figs/plt_pct_chg_mat_age_groups.png")
# 
# df_crude_covid %>%
#   ggplot(aes(x=Five.Year.Age.Groups, y=Pct.Change, fill=Type)) +
#   geom_bar(stat="identity", position=position_dodge()) +
#   theme_minimal() + 
#   labs(y = "% Change in Rates per 100,000 Live Births", 
#        x = "Mother's Age Groups",
#        title = "Percent Change in Rates of Maternal Deaths",
#        subtitle = "2011-2019 vs. 2020-2022") + 
#   theme(plot.caption=element_text(hjust = 0)) + guides(fill=guide_legend(title="")) +
#   theme(axis.text.x = element_text(angle = 80, vjust=.6))
# ggsave("figs/plt_pct_chg_mat_age_groups_covid.png")

