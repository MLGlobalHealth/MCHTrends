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
  df <- read.csv(str_interp('data/compare_periods/${fname}.txt'), sep='\t') %>%
    filter((Notes != 'Total') & (Five.Year.Age.Groups.Code != '15-19')) %>%
    subset(select = -c(Notes, Population, Crude.Rate)) %>%
    na.omit() 
}

df_mm1 <- clean_df('maternal_mortality_age_census_00_10')
df_mm2a <- clean_df('maternal_mortality_age_census_11_17')
df_mm2b <- clean_df('maternal_mortality_age_census_18_21')

df_mmno1 <- clean_df('maternal_mortality_spec_age_census_00_10')
df_mmno2a <- clean_df('maternal_mortality_spec_age_census_11_17')
df_mmno2b <- clean_df('maternal_mortality_spec_age_census_18_21')

df_mmot1 <- clean_df('other_mat_mortality_age_census_00_10')
df_mmot2a <- clean_df('other_mat_mortality_age_census_11_17')
df_mmot2b <- clean_df('other_mat_mortality_age_census_18_21')

df_pr1 <- clean_df('pregrel_mortality_age_census_00_10')
df_pr2a <- clean_df('pregrel_mortality_age_census_11_17')
df_pr2b <- clean_df('pregrel_mortality_age_census_18_21')

concat_df <- function(df2a, df2b) {
  df <- rbind(df2a, df2b) %>%
    group_by(Census.Region, Census.Region.Code, 
             Five.Year.Age.Groups, Five.Year.Age.Groups.Code) %>%
    summarise(Deaths=sum(Deaths))
}

df_mm2 <- concat_df(df_mm2a, df_mm2b)
df_mmno2 <- concat_df(df_mmno2a, df_mmno2b)
df_mmot2 <- concat_df(df_mmot2a, df_mmot2b)
df_pr2 <- concat_df(df_pr2a, df_pr2b)

clean_nat_df <- function(fname, age_var, age_var_code) {
  df_nat <- read.csv(str_interp('data/compare_periods/${fname}.txt'), sep='\t') %>%
    filter((Notes != 'Total') & ({{age_var_code}} != '15-19')) %>%
    subset(select = -c(Notes)) %>%
    na.omit() %>% 
    rename(
      Five.Year.Age.Groups = {{age_var}},
      Five.Year.Age.Groups.Code = {{age_var_code}}
    )
}

df_nat1 <- clean_nat_df('natality_age_year_census_00_02', 
                        Age.of.Mother, Age.of.Mother.Code) 
df_nat2 <- clean_nat_df('natality_age_year_census_03_06', 
                        Age.of.Mother.9, Age.of.Mother.9.Code) 
df_nat3 <- clean_nat_df('natality_age_year_census_07_21', 
                        Age.of.Mother.9, Age.of.Mother.9.Code) %>%
  subset(select = -c(Female.Population, Fertility.Rate))

df_nat <- rbind(df_nat1, df_nat2, df_nat3)
df_nat_00_10 <- df_nat %>% 
  filter(Year < 2011) %>%
  group_by(Census.Region, Census.Region.Code, 
           Five.Year.Age.Groups, Five.Year.Age.Groups.Code) %>%
  summarise(Births=sum(Births))
df_nat_11_21 <- df_nat %>% 
  filter(Year > 2010) %>%
  group_by(Census.Region, Census.Region.Code, 
           Five.Year.Age.Groups, Five.Year.Age.Groups.Code) %>%
  summarise(Births=sum(Births))


mrg_births <- function(df_mort, df_nat) {
  df_mort <- merge(df_mort, df_nat, 
                  by=c('Census.Region','Five.Year.Age.Groups',
                       'Census.Region.Code','Five.Year.Age.Groups.Code'))
}

df_mm1 <- mrg_births(df_mm1, df_nat_00_10)
df_mm2 <- mrg_births(df_mm2, df_nat_11_21)

df_mmno1 <- mrg_births(df_mmno1, df_nat_00_10)
df_mmno2 <- mrg_births(df_mmno2, df_nat_11_21)

df_mmot1 <- mrg_births(df_mmot1, df_nat_00_10)
df_mmot2 <- mrg_births(df_mmot2, df_nat_11_21)

df_pr1 <- mrg_births(df_pr1, df_nat_00_10)
df_pr2 <- mrg_births(df_pr2, df_nat_11_21)

create_crude <- function(df, rate, label) {
  df_new <- df %>% group_by(Census.Region) %>% 
    summarise(Deaths=sum(Deaths), Births=sum(Births)) %>%
    mutate({{rate}} := Deaths/Births*100000) %>%
    mutate(Type := {{label}}) %>%
    subset(select = -c(Births, Deaths))
}

df_crude_mm1 <- create_crude(df_mm1, Rate.00.10, 'Maternal Mortality')
df_crude_mm2 <- create_crude(df_mm2, Rate.11.21, 'Maternal Mortality')
df_crude_mm <- merge(df_crude_mm1, df_crude_mm2, by=c('Census.Region', 'Type'))

df_crude_mmno1 <- create_crude(df_mmno1, Rate.00.10, 'Maternal Mortality (Excl. Other)')
df_crude_mmno2 <- create_crude(df_mmno2, Rate.11.21, 'Maternal Mortality (Excl. Other)')
df_crude_mmno <- merge(df_crude_mmno1, df_crude_mmno2, by=c('Census.Region', 'Type'))

df_crude_mmot1 <- create_crude(df_mmot1, Rate.00.10, 'Other Maternal Mortality')
df_crude_mmot2 <- create_crude(df_mmot2, Rate.11.21, 'Other Maternal Mortality')
df_crude_mmot <- merge(df_crude_mmot1, df_crude_mmot2, by=c('Census.Region', 'Type'))

df_crude_pr1 <- create_crude(df_pr1, Rate.00.10, 'Pregnancy-Related Mortality')
df_crude_pr2 <- create_crude(df_pr2, Rate.11.21, 'Pregnancy-Related Mortality')
df_crude_pr <- merge(df_crude_pr1, df_crude_pr2, by=c('Census.Region', 'Type'))

df_crude <- rbind(df_crude_mm, df_crude_mmno, df_crude_mmot, df_crude_pr)
df_crude$Pct.Change = (df_crude$Rate.11.21-df_crude$Rate.00.10)/df_crude$Rate.00.10*100

# Age standardise

df_ref1 <- df_nat_00_10 %>% group_by(Five.Year.Age.Groups) %>%
  summarise(Ref.Births=sum(Births))
df_ref_base1 = sum(df_ref1$Ref.Births) 

df_ref2 <- df_nat_11_21 %>% group_by(Five.Year.Age.Groups) %>%
  summarise(Ref.Births=sum(Births))
df_ref_base2 = sum(df_ref2$Ref.Births) 

create_age_adj <- function(df, df_ref, ref_base, age_rate, label) {
  df1 <- merge(df, df_ref, by=c('Five.Year.Age.Groups')) %>%
    mutate(Crude.Rate = Deaths/Births*Ref.Births) %>%
    group_by(Census.Region) %>%
    summarise(Crude.Rate=sum(Crude.Rate)) %>%
    mutate({{age_rate}} := Crude.Rate/ref_base*100000) %>%
    subset(select = -c(Crude.Rate)) %>%
    mutate(Type := {{label}})
}

df_age_adj_mm1 <- create_age_adj(df_mm1, df_ref1, df_ref_base1, 
                                 Age.Adj.Rate.00.10, 'Maternal Mortality') 
df_age_adj_mm2 <- create_age_adj(df_mm2, df_ref2, df_ref_base2, 
                                 Age.Adj.Rate.11.21, 'Maternal Mortality') 
df_age_adj_mm <- merge(df_age_adj_mm1, df_age_adj_mm2, by=c('Census.Region', 'Type'))

df_age_adj_mmno1 <- create_age_adj(df_mmno1, df_ref1, df_ref_base1, 
                                   Age.Adj.Rate.00.10, 'Maternal Mortality (Excl. Other)') 
df_age_adj_mmno2 <- create_age_adj(df_mmno2, df_ref2, df_ref_base2, 
                                   Age.Adj.Rate.11.21, 'Maternal Mortality (Excl. Other)') 
df_age_adj_mmno <- merge(df_age_adj_mmno1, df_age_adj_mmno2, by=c('Census.Region', 'Type'))

df_age_adj_mmot1 <- create_age_adj(df_mmot1, df_ref1, df_ref_base1, 
                                   Age.Adj.Rate.00.10, 'Other Maternal Mortality') 
df_age_adj_mmot2 <- create_age_adj(df_mmot2, df_ref2, df_ref_base2, 
                                   Age.Adj.Rate.11.21, 'Other Maternal Mortality') 
df_age_adj_mmot <- merge(df_age_adj_mmot1, df_age_adj_mmot2, by=c('Census.Region', 'Type'))

df_age_adj_pr1 <- create_age_adj(df_pr1, df_ref1, df_ref_base1, 
                                 Age.Adj.Rate.00.10, 'Pregnancy-Related Mortality') 
df_age_adj_pr2 <- create_age_adj(df_pr2, df_ref2, df_ref_base2, 
                                 Age.Adj.Rate.11.21, 'Pregnancy-Related Mortality')
df_age_adj_pr <- merge(df_age_adj_pr1, df_age_adj_pr2, by=c('Census.Region', 'Type'))

df_age_adj <- rbind(df_age_adj_mm, df_age_adj_mmno, df_age_adj_mmot, df_age_adj_pr)
df_age_adj$Pct.Change = (df_age_adj$Age.Adj.Rate.11.21-df_age_adj$Age.Adj.Rate.00.10)/df_age_adj$Age.Adj.Rate.00.10*100

library(stringr)

df_crude %>%
  mutate(Region = word(Census.Region,-1)) %>%
  ggplot(aes(x=Region, y=Pct.Change, fill=Type)) +
  geom_bar(stat="identity", position=position_dodge()) +
  theme_minimal() + 
  labs(y = "% Change in Rates per 100,000 Live Births", 
       x = "Census Region",
       title = "Percent Change in Rates of Maternal and Pregnancy-Related Deaths (2000-2021)") + 
  theme(plot.caption=element_text(hjust = 0)) + guides(fill=guide_legend(title="")) +
  theme(axis.text.x = element_text(angle = 80))
ggsave("figs/plt_census_pct_change_crude.png")

df_age_adj %>%
  mutate(Region = word(Census.Region,-1)) %>%
  ggplot(aes(x=Region, y=Pct.Change, fill=Type)) +
  geom_bar(stat="identity", position=position_dodge()) +
  theme_minimal() + 
  labs(y = "% Change in Rates per 100,000 Live Births", 
       x = "Census Region",
       title = "Percent Change in Age-Adjusted Rates of Maternal and Pregnancy-Related Deaths (2000-2021)") + 
  theme(plot.caption=element_text(hjust = 0)) + guides(fill=guide_legend(title="")) +
  theme(axis.text.x = element_text(angle = 80))
ggsave("figs/plt_census_pct_change_age_adj.png")
