# load packages -----------------------------------------------------------

## Package names
packages <- c("tidyverse","ggplot2","dplyr","shiny",
              "data.table","ggrepel","stringr")

## Install packages not yet installed
installed_packages <- packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(packages[!installed_packages])
}

## Packages loading
invisible(lapply(packages, library, character.only = TRUE))

setwd('/Users/rpark/Desktop/Research/2. Fetal Maternal Mortality/FetalMaternalMortality')

# clean datafiles ---------------------------------------------------------

## Import datafiles 
clean_mat_df <- function(fname, varname) {
  df <- read.csv(
    str_interp('data/${fname}.txt'), sep = "\t") %>%
    subset(select = -c(Notes, Population, Crude.Rate)) %>%
    na.omit() %>%
    filter(Month.Code != "") 
  
  df$Month.Code = month.name[as.numeric(str_sub(df$Month.Code, start= -2))]
  
  df <- df %>% group_by(Month.Code) %>% summarise({{varname}} := sum(Deaths))
}

df_mat_month1a <- clean_mat_df('maternal_deaths_monthly_18_19', MMR.Deaths.Pre)
df_mat_month1b <- clean_mat_df('maternal_deaths_monthly_18_19', MMR.Deaths.Pre)
df_mat_month1 <- rbind(df_mat_month1a, df_mat_month1b) %>% 
  group_by(Month.Code) %>% summarise(MMR.Deaths.Pre = sum(MMR.Deaths.Pre))

df_mat_month2a <- clean_mat_df('maternal_deaths_monthly_20_21', MMR.Deaths.Post)
df_mat_month2b <- clean_mat_df('maternal_deaths_monthly_20_21', MMR.Deaths.Post)
df_mat_month2 <- rbind(df_mat_month2a, df_mat_month2b) %>% 
  group_by(Month.Code) %>% summarise(MMR.Deaths.Post = sum(MMR.Deaths.Post))

df_pregrel_month1a <- clean_mat_df('pregrel_deaths_monthly_18_19', PRMR.Deaths.Pre)
df_pregrel_month1b <- clean_mat_df('pregrel_deaths_monthly_18_19', PRMR.Deaths.Pre)
df_pregrel_month1 <- rbind(df_pregrel_month1a, df_pregrel_month1b) %>% 
  group_by(Month.Code) %>% summarise(PRMR.Deaths.Pre = sum(PRMR.Deaths.Pre))

df_pregrel_month2a <- clean_mat_df('pregrel_deaths_monthly_20_21', PRMR.Deaths.Post)
df_pregrel_month2b <- clean_mat_df('pregrel_deaths_monthly_20_21', PRMR.Deaths.Post)
df_pregrel_month2 <- rbind(df_pregrel_month2a, df_pregrel_month2b) %>% 
  group_by(Month.Code) %>% summarise(PRMR.Deaths.Post = sum(PRMR.Deaths.Post))

clean_fet_df <- function(fname, varname) {
  df <- read.csv(
    str_interp('data/${fname}.txt'), sep = "\t") %>%
    subset(select = -c(Notes, Month.Code)) %>%
    na.omit() %>%
    filter(Month != "") %>%
    rename({{varname}} := 'Fetal.Deaths')
}

df_fet_month1 <- clean_fet_df('fetal_deaths_monthly_18_19', Fetal.Deaths.Pre)
df_fet_month2 <- clean_fet_df('fetal_deaths_monthly_20_21', Fetal.Deaths.Post)

load(file="data/natality_month_clean.Rda")

# Merge births data
mrg_mat_month <- merge(df_mat_month1, df_mat_month2, by='Month.Code')
df_mat_month <- merge(mrg_mat_month, df_nat_month, 
                      by.x='Month.Code', by.y='Month')
df_mat_month$MMR.Deaths.Pre.Rate <- 
  (df_mat_month$MMR.Deaths.Pre*100000)/df_mat_month$Births.Pre
df_mat_month$MMR.Deaths.Post.Rate <- 
  (df_mat_month$MMR.Deaths.Post*100000)/df_mat_month$Births.Post

mrg_pregrel_month <- merge(df_pregrel_month1, df_pregrel_month2, by='Month.Code')
df_pregrel_month <-  merge(mrg_pregrel_month, df_nat_month, 
                           by.x='Month.Code', by.y='Month')
df_pregrel_month$PRMR.Deaths.Pre.Rate <- 
  (df_pregrel_month$PRMR.Deaths.Pre*100000)/df_pregrel_month$Births.Pre
df_pregrel_month$PRMR.Deaths.Post.Rate <- 
  (df_pregrel_month$PRMR.Deaths.Post*100000)/df_pregrel_month$Births.Post

mrg_fet_month <- merge(df_fet_month1, df_fet_month2, by='Month')
df_fet_month <- merge(mrg_fet_month, df_nat_month, by='Month')
df_fet_month$Fetal.Deaths.Pre.Rate <- 
  (df_fet_month$Fetal.Deaths.Pre*1000)/df_fet_month$Births.Pre
df_fet_month$Fetal.Deaths.Post.Rate <- 
  (df_fet_month$Fetal.Deaths.Post*1000)/df_fet_month$Births.Post

# reshape for plots ------------------------------------------------------

reshape_long <- function(df, pre, pre_rate, post, post_rate, month) {
  df_long <- reshape(
    df, direction='long', 
    varying=c({{pre}}, {{pre_rate}}, {{post}}, {{post_rate}}), 
    timevar='Type',
    times=c('2018-2019', '2020-2021'),
    v.names=c('Deaths', 'Deaths.by.Births'),
    idvar='sbj') %>%
    subset(select = -c(sbj, Births.Pre, Births.Post))
}

df_long_mat_month <- reshape_long(
  df_mat_month, 'MMR.Deaths.Pre', 'MMR.Deaths.Pre.Rate', 
  'MMR.Deaths.Post', 'MMR.Deaths.Post.Rate') 
df_long_mat_month$Month.Code = factor(df_long_mat_month$Month.Code, levels=month.name)

df_long_pregrel_month <- reshape_long(
  df_pregrel_month, 'PRMR.Deaths.Pre', 'PRMR.Deaths.Pre.Rate', 
  'PRMR.Deaths.Post', 'PRMR.Deaths.Post.Rate')
df_long_pregrel_month$Month.Code = factor(df_long_pregrel_month$Month.Code, levels=month.name)

df_long_fet_month <- reshape_long(
  df_fet_month, 'Fetal.Deaths.Pre', 'Fetal.Deaths.Pre.Rate', 
  'Fetal.Deaths.Post', 'Fetal.Deaths.Post.Rate')
df_long_fet_month$Month = factor(df_long_fet_month$Month, levels=month.name)

# visualisations ---------------------------------------------------------

df_long_mat_month %>%
  ggplot(aes(x=Month.Code, y=Deaths.by.Births, fill=Type)) +
  geom_bar(stat="identity", position=position_dodge()) +
  geom_text(aes(label=Deaths, group=Type), 
            position = position_dodge(width = 0.9), vjust = -0.3) +
  theme_minimal() + 
  labs(y = "Rate per 100,000 Live Births", 
       title = "Rates of Maternal Deaths by Year (2018-2020)",
       subtitle = "Count of Deaths Above Each Bar",
       caption = "Note: Analysis includes deaths during pregnancy and up to 42 days after birth.") + 
  theme(plot.caption=element_text(hjust = 0), 
        axis.text.x = element_text(angle = 60, hjust=1)) + guides(fill=guide_legend(title="")) 
ggsave("figs/plt_mat_month.png")

df_long_pregrel_month %>%
  ggplot(aes(x=Month.Code, y=Deaths.by.Births, fill=Type)) +
  geom_bar(stat="identity", position=position_dodge()) +
  geom_text(aes(label=Deaths, group=Type), 
            position = position_dodge(width = 0.9), vjust = -0.3) +
  theme_minimal() + 
  labs(y = "Rate per 100,000 Live Births", 
       title = "Rates of Pregnancy-Related Deaths by Year (2018-2020)",
       subtitle = "Count of Deaths Above Each Bar",
       caption = "Note: Analysis includes deaths 43-365 days after birth.") + 
  theme(plot.caption=element_text(hjust = 0), 
        axis.text.x = element_text(angle = 60, hjust=1)) + guides(fill=guide_legend(title="")) 
ggsave("figs/plt_pregrel_month.png")

df_long_fet_month %>%
  ggplot(aes(x=Month, y=Deaths.by.Births, fill=Type)) +
  geom_bar(stat="identity", position=position_dodge()) +
#  geom_text(aes(label=Deaths, group=Type), 
#            position = position_dodge(width = 0.9), vjust = -0.3) +
  theme_minimal() + 
  labs(y = "Rate per 1,000 Live Births", 
       title = "Rates of Fetal Deaths by Year (2018-2020)",
       subtitle = "Count of Deaths Above Each Bar") +
  theme(plot.caption=element_text(hjust = 0), 
        axis.text.x = element_text(angle = 60, hjust=1)) + guides(fill=guide_legend(title="")) 
ggsave("figs/plt_fet_month.png")


