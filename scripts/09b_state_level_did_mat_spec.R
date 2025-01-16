# load packages -----------------------------------------------------------

## Package names
packages <- c("tidyverse","ggplot2","dplyr","shiny", "zoo",
              "data.table","ggrepel","directlabels","did")

## Install packages not yet installed
installed_packages <- packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(packages[!installed_packages])
}

## Packages loading
invisible(lapply(packages, library, character.only = TRUE))

setwd('/Users/rpark/Desktop/Research/Rotations/2. Fetal Maternal Mortality/FetalMaternalMortality')

# Import datafiles
clean_df <- function(fname) {
  df <- read.csv(str_interp('data/did_data/${fname}.txt'), sep = "\t")
  subset(df, select = -c(Notes, Crude.Rate, Year.Code)) %>%
    na.omit() %>%
    filter((Year != '')) %>%
    filter((Year != '2024 (provisional and partial)'))
}

df_national1 <- clean_df('maternal_mortality_spec_19992020')
df_national2 <- clean_df('maternal_mortality_spec_20202024')
df_national <- rbind(df_national1, df_national2) %>%
  distinct(.keep_all = TRUE)

clean_nat <- function(fname) {
  df <- read.csv(str_interp('data/did_data/${fname}.txt'), sep = "\t")
  subset(df, select = -c(Notes, Year.Code)) %>%
    na.omit() %>%
    filter((Year != '')) %>%
    filter((State != '')) %>%
    filter((Year != '2024 (provisional and partial)'))
}

# mrg_att4 <- mrg_att2[c('Year','prop_treated')]

df_natality1 <- clean_nat('natality_yearly_19992002')
df_natality2 <- clean_nat('natality_yearly_20032006')
df_natality3 <- clean_nat('natality_yearly_20072023')
df_natality <- rbind(df_natality1, df_natality2, df_natality3) %>%
  distinct(.keep_all = TRUE)

# Import datafiles
clean_state_df <- function(state, years) {
  df1 <- read.csv(str_interp('data/did_data/maternal_mortality_spec_${state}_${years}.txt'), sep = "\t")
  subset(df1, select = -c(Notes, Crude.Rate, Year.Code)) %>%
    na.omit() %>%
    filter((Year != ''))
}

cln_bind <- function(state) {
  df <- rbind(clean_state_df(state, '19992020'), clean_state_df(state, '20202024')) %>% 
    distinct(.keep_all = TRUE) %>%
    filter((Year != '2024 (provisional and partial)'))
}

states <- gsub(" ", "", tolower(state.name))
# Use lapply to apply the function and store the results in a list
results <- lapply(states, cln_bind)

# Set the list names to the states so it’s easy to identify each entry
names(results) <- states

get_diff <- function(state) {
  # Merge the dataframes on the ID column (or any common key)
  merged_df <- merge(df_national, results[[{state}]], by = "Year", suffixes = c("_1", "_2"))
  
  # Calculate the difference for each pair of numeric columns
  # Create a copy of the merged dataframe with only the ID column
  result_df <- data.frame(Year = merged_df$Year)
  
  # Get the names of numeric columns from the original dataframes
  numeric_cols <- setdiff(names(df_national), "Year")
  
  # Calculate the difference and assign it back to result_df with original names
  for (col in numeric_cols) {
    result_df[[col]] <- merged_df[[paste0(col, "_1")]] - merged_df[[paste0(col, "_2")]]
    result_df$state <- state
  }
  return(result_df)
}

diff_results <- lapply(states, get_diff)
names(diff_results) <- states

diffed_state <- do.call(rbind, diff_results) 
  
diffed_state$Year[diffed_state$Year == '2023 (provisional)'] <- 2023

# Put in treatment years
diffed_state$checkbox[diffed_state$state == 'alaska'] <- 2014
diffed_state$checkbox[diffed_state$state == 'alabama'] <- 2016
diffed_state$checkbox[diffed_state$state == 'arkansas'] <- 2008
diffed_state$checkbox[diffed_state$state == 'arizona'] <- 2010
diffed_state$checkbox[diffed_state$state == 'california'] <- 2003 
diffed_state$checkbox[diffed_state$state == 'colorado'] <- 2015
diffed_state$checkbox[diffed_state$state == 'connecticut'] <- 2005
diffed_state$checkbox[diffed_state$state == 'delaware'] <- 2007
diffed_state$checkbox[diffed_state$state == 'florida'] <- 2005
diffed_state$checkbox[diffed_state$state == 'georgia'] <- 2008
diffed_state$checkbox[diffed_state$state == 'hawaii'] <- 2014
diffed_state$checkbox[diffed_state$state == 'idaho'] <- 2003
diffed_state$checkbox[diffed_state$state == 'illinois'] <- 2008
diffed_state$checkbox[diffed_state$state == 'indiana'] <- 2008
diffed_state$checkbox[diffed_state$state == 'iowa'] <- 2011
diffed_state$checkbox[diffed_state$state == 'kansas'] <- 2005
diffed_state$checkbox[diffed_state$state == 'kentucky'] <- 2010
diffed_state$checkbox[diffed_state$state == 'louisiana'] <- 2012
diffed_state$checkbox[diffed_state$state == 'maine'] <- 2010 
diffed_state$checkbox[diffed_state$state == 'massachusetts'] <- 2014
diffed_state$checkbox[diffed_state$state == 'maryland'] <- 2001
diffed_state$checkbox[diffed_state$state == 'michigan'] <- 2004
diffed_state$checkbox[diffed_state$state == 'minnesota'] <- 2011
diffed_state$checkbox[diffed_state$state == 'mississippi'] <- 2012
diffed_state$checkbox[diffed_state$state == 'missouri'] <- 2010
diffed_state$checkbox[diffed_state$state == 'montana'] <- 2003
diffed_state$checkbox[diffed_state$state == 'nevada'] <- 2008
diffed_state$checkbox[diffed_state$state == 'newjersey'] <- 2004
diffed_state$checkbox[diffed_state$state == 'newmexico'] <- 2006
diffed_state$checkbox[diffed_state$state == 'newyork'] <- 2003
diffed_state$checkbox[diffed_state$state == 'northcarolina'] <- 2014
diffed_state$checkbox[diffed_state$state == 'northdakota'] <- 2008
diffed_state$checkbox[diffed_state$state == 'nebraska'] <- 2005
diffed_state$checkbox[diffed_state$state == 'newhampshire'] <- 2004
diffed_state$checkbox[diffed_state$state == 'ohio'] <- 2007
diffed_state$checkbox[diffed_state$state == 'oklahoma'] <- 2004
diffed_state$checkbox[diffed_state$state == 'oregon'] <- 2006
diffed_state$checkbox[diffed_state$state == 'pennsylvania'] <- 2012
diffed_state$checkbox[diffed_state$state == 'rhodeisland'] <- 2006
diffed_state$checkbox[diffed_state$state == 'southcarolina'] <- 2005
diffed_state$checkbox[diffed_state$state == 'southdakota'] <- 2004
diffed_state$checkbox[diffed_state$state == 'tennessee'] <- 2012
diffed_state$checkbox[diffed_state$state == 'texas'] <- 2006
diffed_state$checkbox[diffed_state$state == 'utah'] <- 2005
diffed_state$checkbox[diffed_state$state == 'vermont'] <- 2008
diffed_state$checkbox[diffed_state$state == 'virginia'] <- 2014
diffed_state$checkbox[diffed_state$state == 'washington'] <- 2004
diffed_state$checkbox[diffed_state$state == 'wisconsin'] <- 2013
diffed_state$checkbox[diffed_state$state == 'westvirginia'] <- 2017
diffed_state$checkbox[diffed_state$state == 'wyoming'] <- 2004

# Merge with natality
df_natality$state <- gsub(" ", "", tolower(df_natality$State))
diffed_state_rate <- merge(diffed_state, df_natality, by = c("Year","state"))
diffed_state_rate$rate <- (diffed_state_rate$Deaths/diffed_state_rate$Births)*100000
diffed_state_rate$Year <- as.numeric(diffed_state_rate$Year) 

################################## DID #######################################
out <- att_gt(yname = "rate",
                   gname = "checkbox",
                   idname = "State.Code",
                   tname = "Year",
                   xformla = ~1,
                   data = diffed_state_rate,
                   control_group="notyettreated",
                   weights = "Births"
)

summary(out)
es <- aggte(out, type = "dynamic")
print(es)

pred <- aggte(out, type = "calendar")
pred_simple <- aggte(out, type = "simple")

cbPalette <- c("#CC79A7", "#0072B2", "#009E73", "#E69F00", "#D55E00", "#56B4E9", "#F0E442", "#999999")

ggdid(es) + scale_x_continuous(limits=c(-10,10)) + 
  scale_fill_manual(values = cbPalette) +
  scale_color_manual(values = cbPalette, 
                     labels = c('Pre', 'Post')) +
  labs(title='',
    x="Time to/from Exposure")
ggsave("figs/plt_did_dynamic_mm_spec.png")
ggsave("figs/plt_did_dynamic_mm_spec.svg")

diffed_state_rate <- diffed_state_rate %>%
  mutate(
    treated = ifelse(Year >= checkbox & !is.na(checkbox), 1, 0),
    period = ifelse(Year < checkbox & !is.na(checkbox), "Before", "After")
  )

df_summary <- diffed_state_rate %>%
  group_by(Year, treated = ifelse(Year >= checkbox & !is.na(checkbox), 1, 0)) %>%
  summarise(avg_rate = (sum(Deaths)/sum(Births))*100000)

df_state_sum <- diffed_state_rate %>%
  group_by(Year) %>%
  mutate(nat_births = sum(Births))

df_state_csum <-df_state_sum %>%
  group_by(Year) %>%
  summarise(
    sum_births = sum(Births[checkbox <= Year])
  )

df_state_csum2 <- merge(df_state_sum, df_state_csum, on='Year') %>%
  mutate(prop_treated = sum_births/nat_births) 

df_state_csum2 <- df_state_csum2 %>%
  select(columns=c('Year', 'prop_treated')) %>%
  distinct() 

colnames(df_state_csum2) <- c("Year", "prop_treated")

# ggplot(df_summary, aes(x = Year, y = avg_rate, color = as.factor(treated))) +
#   geom_line(size = 1.2) +
#   labs(
#     title = "Average Rate Trends for Treated vs Untreated States",
#     x = "Year",
#     y = "Average Rate",
#     color = "Treatment Status"
#   ) +
#   theme_minimal()

diffed_national_rate <- diffed_state_rate %>%
  group_by(Year) %>%
  summarise(Deaths = sum(Deaths), 
            Births = sum(Births),
            Rate = (sum(Deaths)/sum(Births))*100000)

df_att <- data.frame(unlist(pred$egt), unlist(pred$att.egt), unlist(pred$se.egt))
names(df_att) = c("Year","Pred","SE")

mrg_att <- merge(diffed_national_rate, df_att, by = "Year", all.x = TRUE) 
mrg_att2 <- merge(mrg_att, df_state_csum2, by = "Year", all.x = TRUE)

mrg_att2$Pred[is.na(mrg_att2$Pred)] <- pred_simple$overall.att
mrg_att2$SE[is.na(mrg_att2$SE)] <- pred_simple$overall.se

mrg_att2$SE[mrg_att2$Pred < 0] <- pred_simple$overall.se
mrg_att2$Pred[mrg_att2$Pred < 0] <- pred_simple$overall.att

mrg_att2$counterfactual <- mrg_att2$Rate-(mrg_att2$Pred*mrg_att2$prop_treated)

mrg_att3 <- mrg_att2 %>%
  pivot_longer(cols = c(Rate, counterfactual), 
               names_to = "Variable", 
               values_to = "Value") %>%
  mutate(SE = ifelse(Variable == "counterfactual", SE, NA))

mrg_att3$Variable[mrg_att3$Variable == 'counterfactual'] <- "Counterfactual"
mrg_att3$Variable[mrg_att3$Variable == 'Rate'] <- "Actual Rate"

# Plot the counterfactuals over time
ggplot(mrg_att3 %>% arrange(desc(Variable)), aes(x = Year, y = Value, color=Variable)) +
  geom_point(size = 1.2) +
  ylim(0,20) +
  geom_pointrange(aes(ymin=Value-SE, ymax=Value+SE)) +
  scale_color_manual(values = cbPalette) + scale_fill_manual(values = cbPalette) + 
  labs(
    x = "Year",
    y = "Deaths per 100,000") +
  theme_minimal()
ggsave("figs/plt_did_counterfactual_mm_spec.png")
ggsave("figs/plt_did_counterfactual_mm_spec.svg")

# Plot the ATTs
# ggplot(df_att, aes(x = Year, y = Pred)) +
#   geom_point(size = 1.2) +
#   geom_pointrange(aes(ymin=Pred-SE, ymax=Pred+SE)) +
#   labs(
#     title = "ATT by Year",
#     x = "Year",
#     y = "Deaths per 100,000",
#     color = "blue") +
#   theme_minimal()
# 
# state_box_unique <- diffed_state[!duplicated(diffed_state[c('state','checkbox')]),]
# plot(ecdf(state_box_unique$checkbox),
#      xlab="Year",
#      ylab="Cumulative Proportion of States",
#      main="Checkbox Implementation (2000-2016)") 
