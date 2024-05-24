# load packages -----------------------------------------------------------

## Package names
packages <- c("tidyverse","ggplot2","dplyr","shiny",
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

## Import datafiles 
df_mm1 <- read.csv('data/maternal_mortality_yearly_18_22.txt', sep = "\t")
df_mm2 <- read.csv('data/maternal_mortality_yearly_first_half_23.txt', sep = "\t") %>%
  mutate(Deaths = Deaths * 2)
df_mm <- rbind(df_mm1, df_mm2)

df_mmno1 <- read.csv('data/maternal_mortality_spec_yearly_18_22.txt', sep = "\t")
df_mmno2 <- read.csv('data/maternal_mortality_spec_yearly_first_half_23.txt', sep = "\t") %>%
  mutate(Deaths = Deaths * 2)
df_mmno <- rbind(df_mmno1, df_mmno2)

df_mmot1 <- read.csv('data/other_maternal_mortality_yearly_18_22.txt', sep = "\t")
df_mmot2 <- read.csv('data/other_maternal_mortality_yearly_first_half_23.txt', sep = "\t") %>%
  mutate(Deaths = Deaths * 2)
df_mmot <- rbind(df_mmot1, df_mmot2)

df_pr1 <- read.csv('data/pregrel_mortality_yearly_18_22.txt', sep = "\t")
df_pr2 <- read.csv('data/pregrel_mortality_yearly_first_half_23.txt', sep = "\t") %>%
  mutate(Deaths = Deaths * 2)
df_pr <- rbind(df_pr1, df_pr2)

load(file="data/mat_all_yearly_clean.Rda")
load(file="data/natality_yearly_clean.Rda")

## Data cleaning - Year
clean_year <- function(df, varname) {
  df2 = subset(df, select = -c(Notes, Year.Code, 
                               Population, Crude.Rate)) %>% 
        filter(df$Year != "") %>%
        na.omit() %>%
        rename({{varname}} := Deaths)
}
df_mm2 <- clean_year(df_mm, MMR.Deaths)
df_pr2 <- clean_year(df_pr, PRMR.Deaths)
df_mmot2 <- clean_year(df_mmot, OMMR.Deaths)
df_mmno2 <- clean_year(df_mmno, SMMR.Deaths)
  
df_mm_pr <- merge(df_mm2, df_pr2, by='Year')
df_mm_pr_ot <- merge(df_mm_pr, df_mmot2, by='Year')
df_mat_year3 <- merge(df_mm_pr_ot, df_mmno2, by='Year')

df_mat_year3[(df_mat_year3$Year == '2022 (provisional)'), 
             'Year'] = 2022
df_mat_year3[(df_mat_year3$Year == '2023 (provisional and partial)'), 
             'Year'] = 2023
df_mat_year3 <- merge(df_mat_year3, df_nat_year, by='Year', all.x = TRUE)

df_mat_year3$MMR.Deaths.by.Births = (df_mat_year3$MMR.Deaths*100000)/df_mat_year3$Births
df_mat_year3$PRMR.Deaths.by.Births = (df_mat_year3$PRMR.Deaths*100000)/df_mat_year3$Births
df_mat_year3$OMMR.Deaths.by.Births = (df_mat_year3$OMMR.Deaths*100000)/df_mat_year3$Births
df_mat_year3$SMMR.Deaths.by.Births = (df_mat_year3$SMMR.Deaths*100000)/df_mat_year3$Births

# Infant ---------------------------------------------------------

df_inf1 <- read.csv('data/infant_mortality_yearly_18_22.txt', sep = "\t")
df_inf2 <- read.csv('data/infant_mortality_first_half_23.txt', sep = "\t") %>%
  mutate(Deaths = Deaths * 2)
df_inf <- rbind(df_inf1, df_inf2)
df_inf2 <- clean_year(df_inf, Deaths)

df_inf2[(df_inf2$Year == '2022 (provisional)'), 'Year'] = 2022
df_inf2[(df_inf2$Year == '2023 (provisional and partial)'), 'Year'] = 2023
df_inf3 <- merge(df_inf2, df_nat_year, by='Year', all.x = TRUE)
df_inf3$Deaths.by.Births = (df_inf3$Deaths*1000)/df_inf3$Births

# Fetal ---------------------------------------------------------

df_fet <- read.csv('data/fetal_mortality_yearly_05_21.txt', sep = "\t") %>%
  filter(Year >= 2017)
df_fet2 <- subset(df_fet, select = -c(Notes, Year.Code)) %>% 
  filter(df_fet$Year != "") %>%
  na.omit() %>%
  rename(Deaths = Fetal.Deaths)

df_fet3 <- merge(df_fet2, df_nat_year, by='Year', all.x = TRUE)
df_fet3$Deaths.by.Births = (df_fet3$Deaths*1000)/df_fet3$Births

# Reshape for plots ------------------------------------------------------

reshape_long <- function(df, drop_list) {
  df_long <- reshape(
    df, direction='long', 
    varying=c('MMR.Deaths', 'MMR.Deaths.by.Births', 
              'PRMR.Deaths', 'PRMR.Deaths.by.Births',
              'OMMR.Deaths', 'OMMR.Deaths.by.Births',
              'SMMR.Deaths', 'SMMR.Deaths.by.Births'), 
    timevar='Type',
    times=c('Maternal', 'Late Maternal', 'Unspecified Maternal', 'Cause-Specific Maternal'),
    v.names=c('Deaths', 'Deaths.by.Births'),
    idvar='sbj') %>%
    subset(select = -c(Births, sbj)) 
}

df_long_mat_year <- reshape_long(df_mat_year3)

# Visualisations ---------------------------------------------------------

df_long_mat_year %>%
  ggplot(aes(x=Year, y=Deaths.by.Births, fill=Type)) +
  geom_bar(stat="identity", position=position_dodge()) +
  geom_text(aes(label=Deaths, group=Type), 
            position = position_dodge(width = 0.9), vjust = -0.3) +
  theme_minimal() + 
  labs(y = "Rate per 100,000 Live Births", 
       title = "Rates of Maternal Deaths by Year (2018-2023)",
       subtitle = "Count of Deaths Above Each Bar",
       caption = "Note: The 2022 count is provisional. The 2023 count is provisional and partial. We take deaths in the first half of 2023 and double the count to approximate the yearly mortality.") + 
  theme(plot.caption=element_text(hjust = 0), axis.title.x=element_blank()) + guides(fill=guide_legend(title="")) 
ggsave("figs/plt_mat_five_year_bar.png")

df_inf3 %>%
  ggplot(aes(x=Year, y=Deaths.by.Births, group=1)) +
  geom_bar(stat="identity", position=position_dodge(), fill="red") +
  geom_text(aes(label=Deaths), 
            position = position_dodge(width = 0.9), vjust = -0.3) +
  theme_minimal() + 
  labs(y = "Rate per 100,000 Live Births", 
       title = "Rates of Infant Deaths by Year (2018-2023)",
       subtitle = "Count of Deaths Above Each Bar",
       caption = "Note: The 2022 count is provisional. The 2023 count is provisional and partial. We take deaths in the first half of 2023 and double the count to approximate the yearly mortality.") + 
  theme(plot.caption=element_text(hjust = 0), axis.title.x=element_blank()) + guides(fill=guide_legend(title="")) 
ggsave("figs/plt_inf_five_year_bar.png")

df_fet3 %>%
  ggplot(aes(x=Year, y=Deaths.by.Births, group=1)) +
  geom_bar(stat="identity", position=position_dodge(), fill="steelblue") +
  geom_text(aes(label=Deaths), 
            position = position_dodge(width = 0.9), vjust = -0.3) +
  theme_minimal() + 
  labs(y = "Rate per 100,000 Live Births", 
       title = "Rates of Fetal Deaths by Year (2017-2021)",
       subtitle = "Count of Deaths Above Each Bar") +
  theme(axis.title.x=element_blank())
ggsave("figs/plt_fet_five_year_bar.png")

