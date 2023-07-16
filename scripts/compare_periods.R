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
    filter(Notes != 'Total') %>%
    subset(select = -c(Notes, Crude.Rate, Population)) %>%
    na.omit() 
}

df_pre <- clean_df('maternal_deaths_age_2014_2017')
df_post <- clean_df('maternal_deaths_age')
load('data/natality_age_year_clean.Rda')

df_nat_age_pre <- df_nat_age_year %>% 
  filter((Year < 2018) & (Year > 2013)) %>% 
  group_by(Age.of.Mother.9.Code) %>%
  summarise(Births=sum(Births))

df_nat_age_post <- df_nat_age_year %>% 
  filter((Year > 2017)) %>% 
  group_by(Age.of.Mother.9.Code) %>%
  summarise(Births=sum(Births))

age_adj <- function(df_period, df_nat) {
  mrg <- merge(df_period, df_nat, all.x=TRUE,
               by.x = 'Five.Year.Age.Groups.Code',
               by.y = 'Age.of.Mother.9.Code')
}

mrg_pre <- age_adj(df_pre, df_nat_age_pre)
mrg_post <- age_adj(df_post, df_nat_age_post)

mrg_pre$Deaths.by.Births = (mrg_pre$Deaths*100000)/mrg_pre$Births
mrg_post$Deaths.by.Births = (mrg_post$Deaths*100000)/mrg_post$Births

subset_df <- function(mrg) {
  mrg <- mrg %>% subset(select = -c(Births, Five.Year.Age.Groups))
}

mrg_pre <- subset_df(mrg_pre)
mrg_post <- subset_df(mrg_post)

mrg_pre$Years = '2014-2017'
mrg_post$Years = '2018-2021'

mrg_plot <- rbind(mrg_pre, mrg_post)

mrg_plot %>%
  ggplot(aes(x=Five.Year.Age.Groups.Code, y=Deaths.by.Births, fill=Years)) +
  geom_bar(stat="identity", position=position_dodge()) +
  geom_text(aes(label=Deaths, group=Years), 
            position = position_dodge(width = 0.9), vjust = -0.3) +
  theme_minimal() + 
  labs(y = "Rate per 100,000 Live Births", 
       x = "Five Year Age Groups",
       title = "Rates of Maternal Deaths (2014-2021)",
       subtitle = "Count of Deaths Above Each Bar",
       caption = "Note: Analysis includes deaths during pregnancy and up to 42 days after birth.") + 
  theme(plot.caption=element_text(hjust = 0), 
        axis.text.x = element_text(angle = 60, hjust=1)) + guides(fill=guide_legend(title="")) 
ggsave("figs/seasonal_trends/plt_compare_age.png")
