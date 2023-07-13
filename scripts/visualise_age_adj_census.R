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
    subset(select = -c(Notes)) %>%
    na.omit() 
}

df_fet <- clean_df('fetal_deaths_age_census')
df_mat <- clean_df('maternal_deaths_age_census')
df_pregrel <- clean_df('pregrel_deaths_age_census')

load("data/natality_age_census_year_clean.Rda")

df_nat_census_07_21 <- df_nat_age_census_year %>%
  group_by(Age.of.Mother.9.Code, Census.Region) %>%
  summarise(Births=sum(Births))

df_nat_census_18_21 <- df_nat_age_census_year %>%
  filter(Year > 2017) %>%
  group_by(Age.of.Mother.9.Code, Census.Region) %>%
  summarise(Births=sum(Births))

df_nat_07_21 <- df_nat_age_census_year %>%
  filter(Year > 2006) %>%
  group_by(Age.of.Mother.9.Code) %>%
  summarise(National.Births=sum(Births))

# df_nat_07_21$Ratio = df_nat_07_21$National.Births/df_nat_07_21[which(df_nat_07_21$Age.of.Mother.9.Code == "30-34"),]$National.Births

mrg_fet <- merge(df_fet, df_nat_census_07_21,
                 by.x=c('Age.of.Mother.9.Code', 'Standard.Residence.Census.Region'),
                 by.y=c('Age.of.Mother.9.Code', 'Census.Region'),
                 all.x=TRUE)

mrg_fet$Deaths.by.Births = mrg_fet$Fetal.Deaths/mrg_fet$Births

mrg_mat <- merge(df_mat, df_nat_census_18_21,
                 by.x=c('Five.Year.Age.Groups.Code', 'Residence.Census.Region'),
                 by.y=c('Age.of.Mother.9.Code', 'Census.Region')) %>%
  subset(select = -c(Population, Crude.Rate))

mrg_pregrel <- merge(df_pregrel, df_nat_census_18_21,
                 by.x=c('Five.Year.Age.Groups.Code', 'Residence.Census.Region'),
                 by.y=c('Age.of.Mother.9.Code', 'Census.Region')) %>%
  subset(select = -c(Population, Crude.Rate))

mrg_fet2 <- merge(mrg_fet, df_nat_07_21, by=('Age.of.Mother.9.Code'))
mrg_fet2$Age.Adj.Births = mrg_fet2$Deaths.by.Births*mrg_fet2$National.Births

sum_fet <- mrg_fet2 %>% group_by(Standard.Residence.Census.Region) %>%
  summarise(Age.Adj.Births=sum(Age.Adj.Births))

sum_fet$Age.Adj.Rates = (sum_fet$Age.Adj.Births*1000)/sum(df_nat_07_21$National.Births)

# plot 30-34 groups ---------------------------------------------------------

subset_30_34 <- function(df, age_var) {
  df <- df %>% filter(age_var == '30-34 years')
}

mrg_fet_30_34 <- subset_30_34(mrg_fet, mrg_fet$Age.of.Mother.9)
mrg_mat_30_34 <- subset_30_34(mrg_mat, mrg_mat$Five.Year.Age.Groups)
pregrel_mat_30_34 <- subset_30_34(mrg_pregrel, mrg_pregrel$Five.Year.Age.Groups)

# maternal and pregrel deaths do not have enough obs

# all female mortality -------------------------------------------------------

df_all <- read.csv('data/all_deaths_year_race.txt', sep = "\t") %>%
  subset(select = -c(Notes)) %>%
  na.omit() %>%
  filter(Hispanic.Origin != '')

df_all[(df_all$Hispanic.Origin == 'Not Hispanic or Latino') &
         (df_all$Race == 'White'), 
       'Race'] = 'Non-Hispanic White'

df_all[(df_all$Hispanic.Origin == 'Not Hispanic or Latino') &
         (df_all$Race == 'Black or African American'), 
       'Race'] = 'Non-Hispanic Black'

df_all[(df_all$Hispanic.Origin == 'Hispanic or Latino'),
       'Race'] = 'Hispanic'

df_all[df_all$Race == 'American Indian or Alaska Native',
       'Race'] = "American Indian/Alaska Native"

df_all2 <- df_all %>% 
  filter(Race %in% c('Non-Hispanic Black', 'Non-Hispanic White'))

# plot all mortality ---------------------------------------------------------

df_all2 %>%
  mutate_at('Age.Adjusted.Rate', as.numeric) %>%
  ggplot(aes(x=Year, y=Age.Adjusted.Rate, group=Race, colour=Race)) +
  geom_line() + 
  theme_minimal() + 
  labs(y = "Rate per 100,000 Population", 
       x = "Year",
       title = "Age-Adjusted Rates of All-Cause Female Mortality (1999-2020)") + 
  theme(plot.caption=element_text(hjust = 0), axis.text.x = element_text(angle = 80, hjust=1)) +
  guides(colour=guide_legend(title="")) + scale_fill_brewer(palette = "Dark2") +
  ylim(0,180)
ggsave("figs/bw_disparities/plt_all_fem_age_adj.png")

# clean geofiles ---------------------------------------------------------

spdf <- geojson_read("data/us_states_hexgrid.geojson",  what = "sp")
spdf@data = spdf@data %>%
  mutate(google_name = gsub(" \\(United States\\)", "", google_name))

spdf@data = spdf@data %>% mutate(google_name = gsub(" \\(United States\\)", "", google_name))
spdf_fortified <- tidy(spdf, region = "google_name")

spdf_fortified <- spdf_fortified %>%
  left_join(., sum_fet, by=c("id"="Standard.Residence.States")) 

spdf_fortified$bin <- cut(spdf_fortified$Age.Adj.Rates, 
                          breaks=c(seq(3,10), Inf), 
                          labels=c("3-4", "4-5", "5-6", "6-7", "7-8", 
                                   "8-9", "9-10", "10+"), 
                          include.lowest = TRUE)
my_palette <- rev(magma(10))[c(-1,-12)]

centers <- cbind.data.frame(data.frame(gCentroid(spdf, byid=TRUE), id=spdf@data$iso3166_2))

# plot hexmap -----------------------------------------------------------------

ggplot() +
  geom_polygon(data = spdf_fortified, aes(fill = bin, x = long, y = lat, group = group) , size=0, alpha=0.9) +
  geom_text(data=centers, aes(x=x, y=y, label=id), color="white", size=3, alpha=0.6) +
  theme_void() +
  scale_fill_manual( 
    values=my_palette, 
    name="Deaths per 1000 live births (2007-2021)", 
    guide = guide_legend( keyheight = unit(3, units = "mm"), keywidth=unit(12, units = "mm"), label.position = "bottom", title.position = 'top', nrow=1) 
  ) +
  ggtitle( "Fetal Deaths by State" ) +
  theme(
    legend.position = c(0.5, 0.9),
    text = element_text(color = "#22211d"),
    plot.background = element_rect(fill = "#f5f5f2", color = NA), 
    panel.background = element_rect(fill = "#f5f5f2", color = NA), 
    legend.background = element_rect(fill = "#f5f5f2", color = NA),
    plot.title = element_text(size= 22, hjust=0.5, color = "#4e4d47", margin = margin(b = -0.1, t = 0.4, l = 2, unit = "cm")),
  )
ggsave("figs/hexmap_fet_state.png")

# Non-age-standardised rank for 2007-2021
mrg_fet_nas <- mrg_fet %>%
  group_by(Standard.Residence.States) %>%
  summarise(Fetal.Deaths=sum(Fetal.Deaths), Births=sum(Births)) 

mrg_fet_nas$Deaths.by.Births = (mrg_fet_nas$Fetal.Deaths*1000)/mrg_fet_nas$Births

mrg_fet_nas %>% 
  mutate(Standard.Residence.States = fct_reorder(Standard.Residence.States, 
                                                 desc(Deaths.by.Births))) %>%
  ggplot(aes(x=Standard.Residence.States, y=Deaths.by.Births)) +
  geom_bar(stat="identity", fill="steelblue") +
  #geom_hline(yintercept = national_avg, color = "red") + 
  coord_flip() +
  theme_minimal() + 
  labs(y = "Rate per 1,000 Live Births", 
       x = "State",
       title = "Age-Adjusted Rates of Fetal Deaths by State (2007-2021)",
       #subtitle = "National Average Rate in Red (6.0)"
  )
ggsave("figs/plt_fet_state_nas_07_21.png")

# Age-standardised rank

sum_fet %>%
  mutate(Standard.Residence.States = fct_reorder(Standard.Residence.States, 
                                                 desc(Age.Adj.Rates))) %>%
  ggplot(aes(x=Standard.Residence.States, y=Age.Adj.Rates)) +
  geom_bar(stat="identity", fill="steelblue") +
  #geom_hline(yintercept = national_avg, color = "red") + 
  coord_flip() +
  theme_minimal() + 
  labs(y = "Rate per 1,000 Live Births", 
       x = "State",
       title = "Age-Adjusted Rates of Fetal Deaths by State (2007-2021)",
       #subtitle = "National Average Rate in Red (6.0)"
       )
ggsave("figs/plt_fet_state_age_adj_07_21.png")

