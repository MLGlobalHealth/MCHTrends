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

df_fet <- clean_df('fetal_deaths_age_state')
# df_mat <- clean_df('maternal_deaths_age_state')
# df_pregrel <- clean_df('pregrel_deaths_age_state')

load("data/natality_age_state_year_clean.Rda")

df_nat_state_07_21 <- df_nat_age_state_year %>%
  group_by(Age.of.Mother.9.Code, State) %>%
  summarise(Births=sum(Births))
# 
# df_nat_state_18_21 <- df_nat_age_state_year %>%
#   filter(Year > 2017) %>%
#   group_by(Age.of.Mother.9.Code, State) %>%
#   summarise(Births=sum(Births))

df_nat_07_21 <- df_nat_age_state_year %>%
  filter(Year > 2006) %>%
  group_by(Age.of.Mother.9.Code) %>%
  summarise(National.Births=sum(Births))

# df_nat_07_21$Ratio = df_nat_07_21$National.Births/df_nat_07_21[which(df_nat_07_21$Age.of.Mother.9.Code == "30-34"),]$National.Births

mrg_fet <- merge(df_fet, df_nat_state_07_21,
                 by.x=c('Age.of.Mother.9.Code', 'Standard.Residence.States'),
                 by.y=c('Age.of.Mother.9.Code', 'State'),
                 all.x=TRUE)

mrg_fet$Deaths.by.Births = mrg_fet$Fetal.Deaths/mrg_fet$Births

# mrg_mat <- merge(df_mat, df_nat_state_18_21,
#                  by.x=c('Five.Year.Age.Groups.Code', 'Residence.State'),
#                  by.y=c('Age.of.Mother.9.Code', 'State')) %>%
#   subset(select = -c(Population, Crude.Rate))
# 
# mrg_pregrel <- merge(df_pregrel, df_nat_state_18_21,
#                  by.x=c('Five.Year.Age.Groups.Code', 'Residence.State'),
#                  by.y=c('Age.of.Mother.9.Code', 'State')) %>%
#   subset(select = -c(Population, Crude.Rate))

mrg_fet2 <- merge(mrg_fet, df_nat_07_21, by=('Age.of.Mother.9.Code'))
mrg_fet2$Age.Adj.Births = mrg_fet2$Deaths.by.Births*mrg_fet2$National.Births

sum_fet <- mrg_fet2 %>% group_by(Standard.Residence.States) %>%
  summarise(Age.Adj.Births=sum(Age.Adj.Births))

sum_fet$Age.Adj.Rates = (sum_fet$Age.Adj.Births*1000)/sum(df_nat_07_21$National.Births)

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
