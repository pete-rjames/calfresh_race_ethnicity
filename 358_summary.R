# 358 summary

# libraries

library(readxl)
library(stringr)
library(tidyverse)
library(janitor)

# bulk download annual reports

dir.create("../../data/358_data")

url_fed_p1 <- "http://www.cdss.ca.gov/research/res/pdf/DFA358F/20"
url_fed_p2 <- "/DFA358FJul"
url_state_p1 <- "http://www.cdss.ca.gov/research/res/pdf/DFA358S/20"
url_state_p2 <- "/DFA358SJul"

yrs <- c("07", "08", "09", "10", "11", "12", "13", "14", "15")

download_358 <- function(x) { 
  Sys.sleep(5)
  url_fed <- str_c(url_fed_p1, (x), url_fed_p2, (x), ".xls")
  rpt_fed <- str_c("../../data/358_data/358F_", (x), ".xls")
  download.file(url_fed, rpt_fed, mode = "wb")
  Sys.sleep(5)
  url_state <- str_c(url_state_p1, (x), url_state_p2, (x), ".xls")
  rpt_state <- str_c("../../data/358_data/358S_", (x), ".xls")
  download.file(url_state, rpt_state, mode = "wb")
} 

lapply(yrs, download_358)

download_2016_F <- "http://www.cdss.ca.gov/Portals/9/DSSDB/DataTables/DFA358FJul16.xls?ver=2017-03-28-091246-690"
download_2016_S <- "http://www.cdss.ca.gov/Portals/9/DSSDB/DataTables/DFA358SJul16.xls?ver=2017-03-28-092631-293"

download.file(download_2016_F, "../../data/358_data/358F_16.xls", mode = "wb")
download.file(download_2016_S, "../../data/358_data/358S_16.xls", mode = "wb")

# import file, validate, aggregate

# 2016 format used as validation template

yrs <- c("07", "08", "09", "10", "11", "12", "13", "14", "15", "16")

df <- read_excel("../../data/358_data/358F_16.xls", sheet = 3, col_names = FALSE) 
data_cell_range <- df[6,2:163] 
rm(df)

# import function - fed

import_358F <- function(x) {
  rpt <- str_c("../../data/358_data/358F_", (x), ".xls")
  df <- read_excel(rpt, sheet = ifelse((x) < 16, 2, 3), col_names = FALSE)
  stopifnot(all.equal(data_cell_range, df[6, 2:163]))  
  df <- df[7:65, 1:163]
  df <- df %>%
    mutate_each(funs(as.integer), -X1) %>%
    mutate(year = 2000 + as.numeric((x))) %>%
    mutate(report = "federal")
  return(df)
}

import_list_fed <- lapply(yrs, import_358F)

df_358F <- do.call(rbind.data.frame, import_list_fed)

# import function - state 
# NA rule introduced as BLANK in 2016 report

import_358S <- function(x) {
  rpt <- str_c("../../data/358_data/358S_", (x), ".xls")
  df <- read_excel(rpt, sheet = ifelse((x) < 16, 2, 3), col_names = FALSE, na = "BLANK")
  stopifnot(all.equal(data_cell_range, df[6, 2:163]))  
  df <- df[7:65, 1:163]
  df <- df %>%
    mutate_each(funs(as.integer), -X1) %>%
    mutate(year = 2000 + as.numeric((x))) %>%
    mutate(report = "state")
  return(df)
}

import_list_state <- lapply(yrs, import_358S)
df_358S <- do.call(rbind.data.frame, import_list_state)

# combine fed and state

df_358 <- rbind.data.frame(df_358F, df_358S)

df_358 <- df_358 %>%
  rename(county = X1) %>%
  select(county, year, report, everything()) %>%
  gather(key = variable, value = households, X2:X163)

# add flag_yr variable
# flag if either federal and/or state report has warning flag

df_358 <- df_358 %>%
  mutate(flag = grepl('a/', county)) %>%
  mutate(county = str_trim(sub('a/', "", county))) %>%
  group_by(county, year) %>%
  mutate(flag_yr = ifelse(sum(flag) >= 1, TRUE, FALSE)) 
  
# add varnames

variable_names_358 <- read_csv("variable_names_358.csv", col_names = TRUE)

df_358 <- left_join(df_358, variable_names_358, by = "variable") 
  
# prelim_validation 
# 59 counties (inc statewide) * 10 years * 2 reports (fed + state) * 162 data cells

nrow(df_358) == 59*(2017-2007)*2*162

# remove na and pa, use total only
# 54 total values in 162 data cells

df_358 <- df_358 %>%
  filter(na_pa_total == "total") %>%
  select(- na_pa_total)

nrow(df_358) == 59*(2017-2007)*2*54

# combine federal and state totals

df_358 <- df_358 %>%
  group_by(year, county, flag_yr, data_cell, race_ethnicity, hispanic, var_pairs) %>%
  summarise(households = sum(households, na.rm = TRUE))  
nrow(df_358) == 59*(2017-2007)*54

# remove subcategories 

df_358 <- df_358 %>%
  filter(race_ethnicity != "asian_subcategory") %>%
  filter(race_ethnicity != "hawaiian_pacific_islander_subcategory") %>%
  filter(race_ethnicity != "hispanic_other_subcategory") 
  
# create non_hispanic values

df_358 <- df_358 %>%
  ungroup() %>%
  arrange(year, county, flag_yr, var_pairs) %>%
  mutate(households = ifelse(hispanic == 0, (households - lead(households)), households)) 

df_358_error <- df_358 %>%
  filter(households < 0)

# reporting errors occuring from 2007 - 2011

# export counts and percents

race_ethnicity_levels <- c("aian",
                           "asian",
                           "black_african_american",
                           "hawaiian_pacific_islander",
                           "white",
                           "two_or_more_races",
                           "other",
                           "hispanic_white",
                           "hispanic_two_or_more_races",
                           "hispanic_other")

df_358_export_long <- df_358 %>%
  filter(race_ethnicity != "total" & county != "Statewide" & year > 2011) %>%
  mutate(race_ethnicity = factor(race_ethnicity, levels = race_ethnicity_levels, ordered = TRUE)) %>%
  group_by(year, county, flag_yr, race_ethnicity) %>%
  summarise(households = sum(households)) %>%
  group_by(year, county) %>%
  mutate(percent = ifelse(households == 0, 0, 100*households/sum(households))) %>%
  data.frame()

# 58 county * 5 yrs * 10 race_ethnicity categories  
nrow(df_358_export_long) == 58*5*10      

df_358_export_wide_count <- df_358_export_long %>%
  select(- percent) %>%
  spread(key = race_ethnicity, value = households) 

df_358_export_wide_percent <- df_358_export_long %>%
  select(- households) %>%
  spread(key = race_ethnicity, value = percent) 

write_csv(df_358_export_long,"358_2012_2016_long.csv")
write_csv(df_358_export_wide_count,"358_2012_2016_wide_count.csv")
write_csv(df_358_export_wide_percent,"358_2012_2016_wide_percent.csv")

# plots

# counts by race and ethnicity 

plot_358 <- df_358 %>%
  filter(year > 2011 & race_ethnicity != "total") %>%
  mutate(race_ethnicity = factor(race_ethnicity, levels = race_ethnicity_levels, ordered = TRUE)) %>%
  group_by(year, county, flag_yr, race_ethnicity) %>%
  summarise(households = sum(households)) %>%
  group_by(year, county) %>%
  mutate(percent = ifelse(households == 0, 0, round1(100*households/sum(households)))) %>%
  data.frame()

ggplot(plot_358[plot_358$county == "Statewide" & plot_358$percent > 5,],
       aes(x = year, y = (households/1000), group = race_ethnicity)) +
  geom_line(aes(color = race_ethnicity), size = 1) +
  ylab("households (thousands)") +
  ggtitle("CalFresh households by race & ethnicity, 2012 - 2016",
          subtitle = "For racial & ethnic groups representing > 5% of total") +
  theme(plot.title = element_text(hjust = 0.5), 
        plot.subtitle = element_text(hjust = 0.5))

ggsave("calfresh_ts.PNG")

# hispanic proportion by county, 2016

library(choroplethrMaps)
data(county.regions)
calfips <- county.regions %>%
  filter(state.fips.character == "06") %>%
  select(county.name, region)

colnames(calfips)[1] <- "county"
str(calfips)

choro_plot <- plot_358 %>%
  filter(year == 2016 & county != "Statewide") %>%
  filter(race_ethnicity %in% levels(race_ethnicity)[8:10]) %>%
  mutate(county = tolower(county)) %>%
  group_by(county) %>%
  summarise(value = sum(percent)) %>%
  left_join(calfips, by = "county") %>%
  select(region, value)

library(choroplethr)

county_choropleth(choro_plot,
                  state_zoom = "california",
                  title = "Percentage CalFresh households that are hispanic/latino, 2016")

ggsave("choro_hispanic.PNG")

detach("package:choroplethr", unload=TRUE)
detach("package:choroplethrMaps", unload=TRUE)
detach("package:acs", unload=TRUE)
