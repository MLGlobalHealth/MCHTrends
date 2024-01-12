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

setwd('/Users/rpark/Desktop/Research/Rotations/2. Fetal Maternal Mortality/FetalMaternalMortality')

# Maternal ---------------------------------------------------------

# Import and omit NA
clean_df <- function(fname) {
  df <- read.csv(str_interp('data/${fname}.txt'), sep = "\t")
    subset(df, select = -c(Notes, Population, Crude.Rate, Year.Code)) %>%
    na.omit() %>%
    filter((Year != ''))
}

df_mm1 <- clean_df('maternal_mortality_yearly_00_02')
df_mm2 <- clean_df('maternal_mortality_yearly_03_17')
df_mm3 <- clean_df('maternal_mortality_yearly_18_22')
df_mm4 <- clean_df('maternal_mortality_yearly_first_half_23') %>% 
  mutate(Deaths = Deaths * 2)

df_mm <- rbind(df_mm1, df_mm2, df_mm3, df_mm4)
df_mm$Type = 'Maternal'

df_mmno1 <- clean_df('maternal_mortality_spec_yearly_00_02')
df_mmno2 <- clean_df('maternal_mortality_spec_yearly_03_17')
df_mmno3 <- clean_df('maternal_mortality_spec_yearly_18_22')
df_mmno4 <- clean_df('maternal_mortality_spec_yearly_first_half_23') %>% 
  mutate(Deaths = Deaths * 2)

df_mmno <- rbind(df_mmno1, df_mmno2, df_mmno3, df_mmno4)
df_mmno$Type = 'Cause-Specific Maternal'

df_mmot1 <- clean_df('other_mat_mortality_yearly_00_02')
df_mmot2 <- clean_df('other_mat_mortality_yearly_03_17')
df_mmot3 <- clean_df('other_maternal_mortality_yearly_18_22')
df_mmot4 <- clean_df('other_maternal_mortality_yearly_first_half_23') %>% 
  mutate(Deaths = Deaths * 2)

df_mmot <- rbind(df_mmot1, df_mmot2, df_mmot3, df_mmot4)
df_mmot$Type = 'Unspecified Maternal'

df_pr1 <- clean_df('pregrel_mortality_yearly_00_02')
df_pr2 <- clean_df('pregrel_mortality_yearly_03_17')
df_pr3 <- clean_df('pregrel_mortality_yearly_18_22')
df_pr4 <- clean_df('pregrel_mortality_yearly_first_half_23') %>% 
  mutate(Deaths = Deaths * 2)

df_pr <- rbind(df_pr1, df_pr2, df_pr3, df_pr4)
df_pr$Type = 'Late Maternal'

df_mat_all_year <- rbind(df_mm, df_mmno, df_mmot, df_pr) %>%
  mutate(Year = ifelse(Year == "2022 (provisional)", 2022, Year)) %>%
  mutate(Year = ifelse(Year == "2023 (provisional and partial)", 2023, Year)) 
load("data/natality_yearly_clean.Rda")
df_mat_all_year <- merge(df_mat_all_year, df_nat_year, on=Year)
df_mat_all_year$Deaths.by.Births = df_mat_all_year$Deaths/df_mat_all_year$Births*100000
save(df_mat_all_year, file="data/mat_all_yearly_clean.Rda")

# Infant ---------------------------------------------------------

df_inf1 <- clean_df('infant_mortality_yearly_00_17')
df_inf2 <- clean_df('infant_mortality_yearly_18_22')
df_inf3 <- clean_df('infant_mortality_first_half_23') %>% 
  mutate(Deaths = Deaths * 2)

df_inf_all_year <- rbind(df_inf1, df_inf2, df_inf3) %>%
  mutate(Year = ifelse(Year == "2022 (provisional)", 2022, Year)) %>%
  mutate(Year = ifelse(Year == "2023 (provisional and partial)", 2023, Year)) 

df_inf_all_year <- merge(df_inf_all_year, df_nat_year, on=Year)
df_inf_all_year$Deaths.by.Births = df_inf_all_year$Deaths/df_inf_all_year$Births*1000
save(df_inf_all_year, file="data/inf_all_yearly_clean.Rda")

# Fetal ---------------------------------------------------------

df_fet_all_year <- read.csv('data/fetal_mortality_yearly_05_21.txt', sep = "\t")
df_fet_all_year <- subset(df_fet_all_year, select = -c(Notes)) %>%
  na.omit()
df_fet_all_year <- merge(df_fet_all_year, df_nat_year, on=Year)
df_fet_all_year$Deaths.by.Births = df_fet_all_year$Fetal.Deaths/df_fet_all_year$Births*1000
df_fet_all_year$Year <- as.character(df_fet_all_year$Year)
save(df_fet_all_year, file="data/fet_all_yearly_clean.Rda")

# Plot ---------------------------------------------------------

df_mat_all_year %>%
  ggplot(aes(x=Year, y=Deaths.by.Births, group=Type, colour=Type)) +
  geom_line() +
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(y = "Rate per 100,000 Live Births", 
       title = "Rates of Maternal and Pregnancy-Related Deaths by Year (2001-2023)",
       caption = "Note: The 2022 count is provisional. The 2023 count is provisional and partial. We take deaths in the first half of 2023 and double the count to approximate the yearly mortality.") +
  theme(plot.caption=element_text(hjust = 0)) + guides(fill=guide_legend(title="")) 
ggsave('figs/plt_mat_year_line.png')

df_inf_all_year %>%
  ggplot(aes(x=Year, y=Deaths.by.Births, group=1)) +
  geom_line(colour="red") +
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(y = "Rate per 1,000 Live Births", 
       title = "Rates of Infant Deaths by Year (2001-2023)",
       caption = "Note: The 2022 count is provisional. The 2023 count is provisional and partial. We take deaths in the first half of 2023 and double the count to approximate the yearly mortality.") +
  theme(plot.caption=element_text(hjust = 0)) + guides(fill=guide_legend(title="")) 
ggsave('figs/plt_inf_year_line.png')

df_fet_all_year %>%
  ggplot(aes(x=Year, y=Deaths.by.Births, group=1)) +
  geom_line(colour="steelblue") +
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(y = "Rate per 1,000 Live Births", 
       title = "Rates of Fetal Deaths by Year (2005-2021)")
ggsave('figs/plt_fet_year_line.png')

