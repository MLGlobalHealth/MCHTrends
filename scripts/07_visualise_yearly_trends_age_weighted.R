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
clean_df <- function(fname,cat) {
  df <- read.csv(str_interp('data/${fname}.txt'), sep = "\t")
    subset(df, select = -c(Notes, Population, Crude.Rate, Year.Code)) %>%
    na.omit() %>%
    filter((Year != '')) %>%
    mutate(age_group=cat)    
}

df_mmno1a <- clean_df('maternal_mortality_spec_yearly_00_02_below_35', '<35')
df_mmno1b <- clean_df('maternal_mortality_spec_yearly_00_02_above_35', '>35')

df_mmno2a <- clean_df('maternal_mortality_spec_yearly_03_17_below_35', '<35')
df_mmno2b <- clean_df('maternal_mortality_spec_yearly_03_17_above_35', '>35')

df_mmno3a <- clean_df('maternal_mortality_spec_yearly_18_22_below_35', '<35')
df_mmno3b <- clean_df('maternal_mortality_spec_yearly_18_22_above_35', '>35')

df_mmno4a <- clean_df('maternal_mortality_spec_yearly_first_half_23_below_35', '<35') %>% 
  mutate(Deaths = Deaths * 2)
df_mmno4b <- clean_df('maternal_mortality_spec_yearly_first_half_23_above_35', '>35') %>% 
  mutate(Deaths = Deaths * 2)

df_mmno <- rbind(df_mmno1a, df_mmno1b, df_mmno2a, df_mmno2b, 
                 df_mmno3a, df_mmno3b, df_mmno4a, df_mmno4b) %>%
  mutate(Year = ifelse(Year == "2022 (provisional)", 2022, Year)) %>%
  mutate(Year = ifelse(Year == "2023 (provisional)", 2023, Year)) 

df_mmno$Type = 'Cause-Specific Maternal'

clean_nat <- function(fname, cat) {
  df <- read.csv(str_interp('data/${fname}.txt'), sep = "\t")
  subset(df, select = -c(Notes, Fertility.Rate, Female.Population)) %>%
  na.omit() %>%
  mutate(age_group=cat) 
}

clean_nat23 <- function(fname, cat) {
  df <- read.csv(str_interp('data/${fname}.txt'), sep = "\t")
  subset(df, select = -c(Notes)) %>%
  na.omit() %>%
  mutate(Births = Births * 2) %>%
  mutate(age_group=cat) 
}

df_nat_year1a <- read.csv('data/natality_yearly_00_02_below_35.txt', sep = "\t")
df_nat_year1a <- subset(df_nat_year1a, select = -c(Notes)) %>%
  na.omit() %>% mutate(age_group='<35')
df_nat_year1b <- read.csv('data/natality_yearly_00_02_above_35.txt', sep = "\t")
df_nat_year1b <- subset(df_nat_year1b, select = -c(Notes)) %>%
  na.omit() %>% mutate(age_group='>35')

df_nat_year2a <- clean_nat('natality_yearly_03_04_below_35', '<35')
df_nat_year2b <- clean_nat('natality_yearly_03_04_above_35', '>35')

df_nat_year3a <- clean_nat('natality_yearly_05_06_below_35', '<35')
df_nat_year3b <- clean_nat('natality_yearly_05_06_above_35', '>35')

df_nat_year4a <- clean_nat('natality_yearly_07_22_below_35', '<35')
df_nat_year4b <- clean_nat('natality_yearly_07_22_above_35', '>35')

df_nat_year5a <- clean_nat23('natality_yearly_first_half_23_below_35', '<35')
df_nat_year5b <- clean_nat23('natality_yearly_first_half_23_above_35', '>35')

df_nat_year_age <- rbind(df_nat_year1a, df_nat_year1b, 
                         df_nat_year2a, df_nat_year2b, 
                         df_nat_year3a, df_nat_year3b,
                         df_nat_year4a, df_nat_year4b,
                         df_nat_year5a, df_nat_year5b) 

df_nat_year_age_tot <- df_nat_year_age %>%
  group_by(Year) %>%
  summarise(Total = sum(Births))

df_nat_year_age_rate <- merge(df_nat_year_age, df_nat_year_age_tot, by='Year') %>%
  mutate(rate = Births/Total)

df_mmno_age <- merge(df_mmno, df_nat_year_age, on=c(Year,age_group))
df_mmno_age$Deaths.by.Births = df_mmno_age$Deaths/df_mmno_age$Births*100000

# CI -----------------------------------------------------------

byars_conf_interval <- function(x, n, mult=100000, alpha=0.05) {
  O <- x
  z <- qnorm(1 - alpha/2)
  lower <- (O*(1 - (1/(9*O)) - (z/(3*sqrt(O))))**3)/n
  upper <- ((O+1)*(1 - (1/(9*(O+1))) + (z/(3*sqrt(O+1))))**3)/n
  return(data.frame(lower = as.numeric(lower*mult), upper = as.numeric(upper*mult)))
}

df_mat_ci <- byars_conf_interval(df_mmno_age$Deaths, df_mmno_age$Births)
df_mmno_age <- cbind(df_mmno_age, df_mat_ci)

# Weighting ------------------------------------------------------

mort_lt_35_2000 = 3503621/4045691
mort_gt_35_2000 = 1-mort_lt_35_2000

mort_lt_35_2022 = 7.479385
mort_gt_35_2022 = 21.060789

lower_lt_35_2022 = 6.519374
lower_gt_35_2022 = 17.885344

upper_lt_35_2022 = 8.54094
upper_gt_35_2022 = 24.63738

df_nat_year_age_rate <- df_nat_year_age_rate %>%
  mutate(Wgt.Deaths.by.Births = case_when(
    age_group == '<35' ~ rate*mort_lt_35_2022,
    age_group == '>35' ~ rate*mort_gt_35_2022)) %>%
  mutate(Wgt.Lower = case_when(
    age_group == '<35' ~ rate*lower_lt_35_2022,
    age_group == '>35' ~ rate*lower_gt_35_2022)) %>%
  mutate(Wgt.Upper = case_when(
    age_group == '<35' ~ rate*upper_lt_35_2022,
    age_group == '>35' ~ rate*upper_gt_35_2022)) %>%
  group_by(Year) %>%
  summarise(
    Wgt.Deaths.by.Births=sum(Wgt.Deaths.by.Births),
    Wgt.Lower=sum(Wgt.Lower),
    Wgt.Upper=sum(Wgt.Upper)
  ) 

df_mmno_age <- df_mmno_age %>%
  mutate(Deaths.by.Births = case_when(
  age_group == '<35' ~ Deaths.by.Births*mort_lt_35_2000,
  age_group == '>35' ~ Deaths.by.Births*mort_gt_35_2000)) %>%
  mutate(lower = case_when(
    age_group == '<35' ~ lower*mort_lt_35_2000,
    age_group == '>35' ~ lower*mort_gt_35_2000)) %>%
  mutate(upper = case_when(
    age_group == '<35' ~ upper*mort_lt_35_2000,
    age_group == '>35' ~ upper*mort_gt_35_2000)) %>%
  group_by(Year, Type) %>%
  summarise(
    Births=sum(Births),
    Deaths=sum(Deaths),
    Deaths.by.Births=sum(Deaths.by.Births),
    lower=sum(lower),
    upper=sum(upper)
    ) 

# Merge with the yearly plots

load("data/mat_all_yearly_clean.Rda")

df_mat_all_year <- df_mat_all_year %>%
  filter(Type == 'Cause-Specific Maternal')

df_mat_all_year$Type = 'Raw'

df_mat_all_year_ci <- byars_conf_interval(df_mat_all_year$Deaths, df_mat_all_year$Births)
df_mat_all_year <- cbind(df_mat_all_year, df_mat_all_year_ci)
df_mat_all_year$Type = 'Age-Adjusted'

df_all <- rbind(df_mmno_age, df_mat_all_year)

# Plot ---------------------------------------------------------

# colour blind friendly palette from here: https://jfly.uni-koeln.de/color/
cbPalette <- c("#CC79A7","#009E73", "#E69F00", "#D55E00", "#56B4E9", "#F0E442", "#999999")

df_all %>%
  ggplot(aes(x=Year, y=Deaths.by.Births, group=Type, colour=Type, linetype=Type)) +
  geom_line() +
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(y = "Rate per 100,000 Live Births") +
  theme(plot.caption=element_text(hjust = 0), axis.title.x=element_blank()) + guides(fill=guide_legend(title="")) +
  geom_ribbon(aes(ymin=lower, ymax=upper, group=Type, fill=Type), alpha=0.2, color = NA, show.legend = FALSE) +
  scale_color_manual(values = cbPalette) + scale_fill_manual(values = cbPalette) 
ggsave('figs/plt_mat_year_age_wgt_line.png')

# df_mmno_mrg <- merge(df_mmno, df_nat_year_age, by=c('Year','age_group'))
# df_mmno_mrg$rate = df_mmno_mrg$Deaths / df_mmno_mrg$Births * 100000
# 
# df_mmno_mrg %>%
#   ggplot(aes(x=Year, y=rate, group=age_group, colour=age_group, linetype=age_group)) +
#   geom_line() +
#   theme_minimal() +
#   theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
#   labs(y="Death Rate per 100,000") +
#   theme(plot.caption=element_text(hjust = 0), axis.title.x=element_blank()) + guides(fill=guide_legend(title="")) +
#   #geom_ribbon(aes(ymin=lower, ymax=upper, group=Type, fill=Type), alpha=0.2, color = NA, show.legend = FALSE) +
#   scale_color_manual(values = cbPalette) + scale_fill_manual(values = cbPalette)

