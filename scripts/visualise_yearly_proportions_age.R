# load packages -----------------------------------------------------------

## Package names
packages <- c("tidyverse","ggplot2","dplyr","shiny", "zoo",
              "data.table","ggrepel","directlabels")

## Install packages not yet installed
installed_packages <- packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(packages[!installed_packages])
}

## Packages loading
invisible(lapply(packages, library, character.only = TRUE))

setwd('/Users/rpark/Desktop/Research/2. Fetal Maternal Mortality/FetalMaternalMortality')

# cleaning ---------------------------------------------------------

# Import and omit NA
clean_df <- function(fname) {
  df <- read.csv(str_interp('data/${fname}.txt'), sep = "\t")
  subset(df, select = -c(Notes, Population, Crude.Rate,
                         Year.Code, Five.Year.Age.Groups.Code)) %>%
    na.omit() %>%
    filter((Five.Year.Age.Groups != '') & (Deaths != 'Suppressed')) %>%
    mutate_at('Deaths', as.numeric)
}

df_mm1 <- clean_df('maternal_mortality_spec_year_age_00_17')
df_mm2 <- clean_df('maternal_mortality_spec_year_age_18_21')
df_mm <- rbind(df_mm1, df_mm2)

df_tot1 <- clean_df('all_fem_deaths_year_age_00_17')
df_tot2 <- clean_df('all_fem_deaths_year_age_18_21')
df_tot <- rbind(df_tot1, df_tot2)

df_mm_mrg <- merge(df_mm, df_tot, by=c('Year','Five.Year.Age.Groups'))
df_mm_mrg$Pct <- df_mm_mrg$Deaths.x/df_mm_mrg$Deaths.y*100

df_mm_mrg %>%
  ggplot(aes(x=Year, y=Pct, group=Five.Year.Age.Groups, colour=Five.Year.Age.Groups)) +
  geom_line() +
  theme_minimal() + 
  labs(y = "Percent", 
       title = "Percentage of Cause-Specific Maternal Deaths of Total Deaths (2000-2021)") +
  theme(axis.text.x = element_text(angle = 60, hjust=1)) + guides(fill=guide_legend(title="")) +
  scale_color_discrete(name="")
ggsave('figs/plt_mat_spec_tot_deaths_year_age.png') 
