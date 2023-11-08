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

# YEARLY ---------------------------------------------------------

# Import and omit NA
clean_df <- function(fname) {
  df <- read.csv(str_interp('data/${fname}.txt'), sep = "\t")
    subset(df, select = -c(Notes, Population, Crude.Rate, Year.Code)) %>%
    na.omit() %>%
    filter((Year != '') & (Year != '2022 (provisional)'))
}

df_mm1 <- clean_df('maternal_mortality_yearly_03_17')
df_mm2 <- clean_df('maternal_deaths_yearly')
df_mm <- rbind(df_mm1, df_mm2)
df_mm$Type = 'Maternal'

df_mmno1 <- clean_df('new_other_category/maternal_mortality_spec_yearly_03_17')
df_mmno2 <- clean_df('new_other_category/maternal_mortality_spec_yearly')
df_mmno <- rbind(df_mmno1, df_mmno2)
df_mmno$Type = 'Cause-Specific Maternal'

df_mmot1 <- clean_df('new_other_category/other_mat_mortality_yearly_03_17')
df_mmot2 <- clean_df('new_other_category/other_mat_mortality_yearly')
df_mmot <- rbind(df_mmot1, df_mmot2)
df_mmot$Type = 'Unspecified Maternal'

df_pr1 <- clean_df('pregrel_mortality_yearly_03_17')
df_pr2 <- clean_df('pregrel_deaths_yearly')
df_pr <- rbind(df_pr1, df_pr2)
df_pr$Type = 'Late Maternal'

df_all_year <- rbind(df_mm, df_mmno, df_mmot, df_pr)
load("data/natality_yearly_clean.Rda")
df_all_year <- merge(df_all_year, df_nat_year, on=Year)
df_all_year$Deaths.by.Births = df_all_year$Deaths/df_all_year$Births*100000

df_all_year %>%
  ggplot(aes(x=Year, y=Deaths.by.Births, group=Type, colour=Type)) +
  geom_line() +
  theme_minimal() + 
  labs(y = "Rate per 100,000 Live Births", 
       title = "Rates of Maternal and Pregnancy-Related Deaths by Year (2005-2021)")
ggsave('figs/plt_mat_new_other_year_line.png')

# MONTHLY ---------------------------------------------------------

# Import and omit NA
clean_df2 <- function(fname) {
  df <- read.csv(str_interp('data/compare_periods/Monthly/${fname}.txt'), sep = "\t")
    subset(df, select = -c(Notes, Population, Crude.Rate, Year.Code)) %>%
    na.omit() %>%
    filter(Month != '')
    
  df$Month.Code = month.name[as.numeric(str_sub(df$Month.Code, start = -2))]

  df <- df %>% group_by(Year, Month.Code) %>%
    summarise(Deaths := sum(Deaths)) %>%
    na.omit()
}

df_mm_month1 <- clean_df2('maternal_mortality_year_month')
df_mm_month2 <- clean_df2('maternal_mortality_year_month_21')
df_mm_month <- rbind(df_mm_month1, df_mm_month2)

df_mmno_month1 <- clean_df2('new_other_category/maternal_mortality_spec_year_month')
df_mmno_month2 <- clean_df2('new_other_category/maternal_mortality_spec_year_month_21')
df_mmno_month <- rbind(df_mmno_month1, df_mmno_month2)

df_nat_month1 <- read.csv('data/compare_periods/Monthly/natality_year_month_03_06.txt', sep = "\t") %>%
  subset(select = -c(Notes, Month.Code)) %>%
  filter(Month != "") %>%
  rename(Month.Code = Month) %>%
  na.omit() 

df_nat_month2 <- read.csv('data/compare_periods/Monthly/natality_year_month_07_21.txt', sep = "\t") %>%
  subset(select = -c(Notes, Month.Code)) %>%
  filter(Month != "") %>%
  rename(Month.Code = Month) %>%
  na.omit() 

df_nat_month <- rbind(df_nat_month1, df_nat_month2)

df_mm_month <- merge(df_mm_month, df_nat_month, on=c('Year', 'Month'))
df_mmno_month <- merge(df_mmno_month, df_nat_month, on=c('Year', 'Month'))

df_mm_month$Deaths.by.Births = df_mm_month$Deaths/df_mm_month$Births*100000
df_mmno_month$Deaths.by.Births = df_mmno_month$Deaths/df_mmno_month$Births*100000

df_mm_month$Type = 'Maternal'
df_mmno_month$Type = 'Cause-Specific Maternal'
df_mm_month_all <- rbind(df_mm_month, df_mmno_month)

df_mm_month_all$Year.Month <- as.yearmon(paste(df_mm_month_all$Month, df_mm_month_all$Year))

df_mm_month %>%
  mutate(Month.Code = factor(Month.Code, levels = month.name)) %>%
  ggplot(aes(x=Month.Code, y=Deaths.by.Births, group=Year, colour=Year)) +
  geom_line() +
  theme_minimal() + 
  labs(y = "Rate per 100,000 Live Births", 
       x = "Month",
       title = "Rates of Maternal Deaths by Month (2003-2021)") +
  theme(axis.text.x = element_text(angle = 60, hjust=1))
ggsave('figs/plt_mat_month_line.png')

df_mmno_month %>%
  mutate(Month.Code = factor(Month.Code, levels = month.name)) %>%
  ggplot(aes(x=Month.Code, y=Deaths.by.Births, group=Year, colour=Year)) +
  geom_line() +
  theme_minimal() + 
  labs(y = "Rate per 100,000 Live Births", 
       x = "Month",
       title = "Rates of Maternal Deaths by Month (2003-2021)") +
  theme(axis.text.x = element_text(angle = 60, hjust=1))
ggsave('figs/plt_mat_spec_new_other_month_line.png')

df_mm_month_all %>%
  ggplot(aes(x=Year.Month, y=Deaths.by.Births, group=Type, colour=Type)) +
  geom_line() +
  theme_minimal() + 
  labs(y = "Rate per 100,000 Live Births", 
       title = "Rates of Maternal Deaths by Month (2003-2021)") +
  theme(axis.text.x = element_text(angle = 60, hjust=1))
ggsave('figs/plt_mat_new_other_month_cont_line.png') 

# WEEKLY ---------------------------------------------------------
# 
# # Import and omit NA
# clean_df2 <- function(fname) {
#   df <- read.csv(str_interp('data/${fname}.txt'), sep = "\t")
#   subset(df, select = -c(Notes, Population, Crude.Rate)) %>%
#     na.omit() %>%
#     filter((MMWR.Week != '') & (Deaths != 'Suppressed')) %>%
#     separate(MMWR.Week.Code, c('Year','Week')) %>%
#     mutate(Week = as.numeric(Week)) %>%
#     filter(Year != 2023) %>% 
#     group_by(Year) %>% 
#     complete(Week = full_seq(1:52, 1)) %>%
#     subset(select = -c(MMWR.Week)) %>%
#     mutate(Year.Week = str_c(Year, '-', Week)) %>%
#     mutate(Deaths = as.numeric(Deaths)) 
# }
# 
# df_mm_weekly <- clean_df2('maternal_mortality_weekly')
# df_mmno_weekly <- clean_df2('maternal_mortality_spec_weekly')
# df_mmot_weekly <- clean_df2('other_mat_mortality_weekly')
# df_pr_weekly <- clean_df2('pregrel_mortality_weekly')
# 
# xscale = case_when(df_mm_weekly$Week %in% c(1, 30) ~ df_mm_weekly$Year.Week,
#                    df_mm_weekly$Week != 1 ~ "")
# 
# df_mm_weekly %>%
#   arrange(Year,Week) %>%
#   ggplot(aes(x=Year.Week, y=Deaths, group = 1)) +
#   geom_line(color="steelblue") +
#   theme_minimal() + 
#   labs(y = "Count of Deaths", 
#        x = "Week",
#        title = "Weekly Maternal Deaths (2018-2022)") +
#   theme(axis.text.x = element_text(angle = 60, hjust=1)) +
#   scale_x_discrete(breaks = xscale)
# ggsave('figs/plt_mat_weekly.png')

