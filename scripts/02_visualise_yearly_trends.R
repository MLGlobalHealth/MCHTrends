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

df_fet_all_year <- read.csv('data/fetal_mortality_yearly_05_22.txt', sep = "\t")
df_fet_all_year <- subset(df_fet_all_year, select = -c(Notes)) %>%
  na.omit()
df_fet_all_year <- merge(df_fet_all_year, df_nat_year, on=Year)
df_fet_all_year$Deaths.by.Births = df_fet_all_year$Fetal.Deaths/df_fet_all_year$Births*1000
df_fet_all_year$Year <- as.character(df_fet_all_year$Year)
save(df_fet_all_year, file="data/fet_all_yearly_clean.Rda")

# CI -----------------------------------------------------------

byars_conf_interval <- function(x, n, mult=100000, alpha=0.05) {
  O <- x
  z <- qnorm(1 - alpha/2)
  lower <- (O*(1 - (1/(9*O)) - (z/(3*sqrt(O))))**3)/n
  upper <- ((O+1)*(1 - (1/(9*(O+1))) + (z/(3*sqrt(O+1))))**3)/n
  return(data.frame(lower = as.numeric(lower*mult), upper = as.numeric(upper*mult)))
}

df_mat_ci <- byars_conf_interval(df_mat_all_year$Deaths, df_mat_all_year$Births, 100000)
df_mat_all_year <- cbind(df_mat_all_year, df_mat_ci)

df_inf_ci <- byars_conf_interval(df_inf_all_year$Deaths, df_inf_all_year$Births, 1000)
df_inf_all_year <- cbind(df_inf_all_year, df_inf_ci)
df_inf_all_year$Type <- "Infant"

df_fet_ci <- byars_conf_interval(df_fet_all_year$Fetal.Deaths, df_fet_all_year$Births, 1000)
df_fet_all_year <- cbind(df_fet_all_year, df_fet_ci)
df_fet_all_year$Type <- "Fetal"
df_fet_all_year <- rename(df_fet_all_year, Deaths = Fetal.Deaths)

df_inf_fet_all_year <- rbind(df_inf_all_year, df_fet_all_year)

# Plot ---------------------------------------------------------

# colour blind friendly palette from here: https://jfly.uni-koeln.de/color/
cbPalette <- c("#CC79A7", "#0072B2", "#009E73", "#E69F00", "#D55E00", "#56B4E9", "#F0E442", "#999999")

df_mat_all_year %>%
  ggplot(aes(x=Year, y=Deaths.by.Births, group=Type, colour=Type, linetype=Type)) +
  geom_line() +
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(y = "Rate per 100,000 Live Births") +
  theme(plot.caption=element_text(hjust = 0), axis.title.x=element_blank()) + guides(fill=guide_legend(title="")) +
  geom_ribbon(aes(ymin=lower, ymax=upper, group=Type, fill=Type), alpha=0.2, color = NA, show.legend = FALSE) +
  scale_color_manual(values = cbPalette) + scale_fill_manual(values = cbPalette) 
ggsave('figs/plt_mat_year_line.png')

cbPalette2 <- c("#D55E00", "#56B4E9")

df_inf_fet_all_year %>%
  ggplot(aes(x=Year, y=Deaths.by.Births, group=Type, colour=Type, linetype=Type)) +
  geom_line() +
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(y = "Rate per 1,000 Live Births") +
  theme(plot.caption=element_text(hjust = 0), axis.title.x=element_blank()) + guides(fill=guide_legend(title="")) +
geom_ribbon(aes(ymin=lower, ymax=upper, fill=Type), alpha=0.2, color = NA, show.legend = FALSE) +
  scale_color_manual(values = cbPalette2) + scale_fill_manual(values = cbPalette2) 
ggsave('figs/plt_inf_fet_year_line.png')
