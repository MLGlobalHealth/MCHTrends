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

df_crude <- read.csv('data/maternal_deaths_race_comparison.txt', sep='\t') 
 
df_crude %>%
  ggplot(aes(x=Race, y=Pct.Chg.1, fill=Type)) +
  geom_bar(stat="identity", position=position_dodge()) +
  theme_minimal() + 
  labs(y = "% Change in Rates per 100,000 Live Births", 
       x = "Racial/Ethnic Group",
       title = "Percent Change in Rates of Maternal Deaths",
       subtitle = "2000-2010 vs. 2011-2019") + 
  theme(plot.caption=element_text(hjust = 0)) + guides(fill=guide_legend(title="")) 
  #theme(axis.text.x = element_text(angle = 60, vjust=0.6)) 
ggsave("figs/plt_census_pct_change_race_crude.png")

df_crude %>%
  ggplot(aes(x=Race, y=Pct.Chg.2, fill=Type)) +
  geom_bar(stat="identity", position=position_dodge()) +
  theme_minimal() + 
  labs(y = "% Change in Rates per 100,000 Live Births", 
       x = "Racial/Ethnic Group",
       title = "Percent Change in Rates of Maternal Deaths",
       subtitle = "2011-2019 vs. 2020-2021") + 
  theme(plot.caption=element_text(hjust = 0)) + guides(fill=guide_legend(title="")) 
  #theme(axis.text.x = element_text(angle = 80, vjust=.75))
ggsave("figs/plt_census_pct_change_race_crude_covid.png")
