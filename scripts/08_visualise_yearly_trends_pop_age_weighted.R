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

# setwd('/Users/rpark/Desktop/Research/Rotations/2. Fetal Maternal Mortality/FetalMaternalMortality')

# Import datafiles
clean_df <- function(fname) {
  df <- read.csv(str_interp('data/${fname}.txt'), sep = "\t")
  subset(df, select = -c(Notes, Crude.Rate, Year.Code, X..of.Total.Deaths)) %>%
    na.omit() %>%
    filter((Year != '')) 
}

df_mmno1 <- clean_df('maternal_mortality_spec_yearly_age_adj_00_02')
df_mmno2 <- clean_df('maternal_mortality_spec_yearly_age_adj_03_17')
df_mmno3 <- clean_df('maternal_mortality_spec_yearly_age_adj_18_22')

df_mmno <- rbind(df_mmno1, df_mmno2, df_mmno3)
df_mmno$Type = 'Maternal Excl. Cause Unspecified'
  
byars_conf_interval <- function(x, n, mult=100000, alpha=0.05) {
  O <- x
  z <- qnorm(1 - alpha/2)
  lower <- (O*(1 - (1/(9*O)) - (z/(3*sqrt(O))))**3)/n
  upper <- ((O+1)*(1 - (1/(9*(O+1))) + (z/(3*sqrt(O+1))))**3)/n
  return(data.frame(lower = as.numeric(lower*mult), upper = as.numeric(upper*mult)))
}

df_mat_ci <- byars_conf_interval(df_mmno$Deaths, df_mmno$Population)
df_mmno <- cbind(df_mmno, df_mat_ci)

# colour blind friendly palette from here: https://jfly.uni-koeln.de/color/
cbPalette <- c("#009E73", "#CC79A7", "#E69F00", "#D55E00", "#56B4E9", "#F0E442", "#999999")

df_mmno %>%
  ggplot(aes(x=Year, y=Age.Adjusted.Rate, group=Type, colour=Type, linetype=Type)) +
  geom_line(linetype = "longdash") + geom_point() +
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(y = "Rate per 100,000 Population") +
  theme(plot.caption=element_text(hjust = 0), axis.title.x=element_blank(),legend.position = "none") + guides(fill=guide_legend(title="")) +
  geom_ribbon(aes(ymin=lower, ymax=upper, group=Type, fill=Type), alpha=0.2, color = NA, show.legend = FALSE) +
  scale_color_manual(values = cbPalette) + scale_fill_manual(values = cbPalette) 
ggsave('figs/plt_mat_year_pop_age_wgt_line.png')