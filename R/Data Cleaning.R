## ----------------------------------------------------------------
## Clean and prepare the NNDSS
##
##      Authors: Shelby Golden, MS from Yale's YSPH DSDE group
## Date Created: June 23rd, 2025
## 
## Description: 
## 

## ----------------------------------------------------------------
## SET UP THE ENVIRONMENT

# Initiate the package environment.
# renv::init()
renv::restore()

suppressPackageStartupMessages({
  library("readr")
  library("tidyr")
  library("dplyr")
  library("stringr")
})

"%!in%" <- function(x,y)!("%in%"(x,y))




## ----------------------------------------------------------------
## LOAD IN THE DATA

nndss_raw <- read_csv("Data/NNDSS.2016.2021.csv")




## ----------------------------------------------------------------
## IDENTIFY MESSY ELEMENTS IN THE DATA

glimpse(nndss_raw)

# The critical metadata variables are "ReportingArea", "MMWRYear", and "MMWRWeek".
# We want to inspect these further to confirm they are consistent and reflect
# complete data.
# 
# Another major thing to notice is the table reflects two sources of data that
# have been joined together. The first six columns are from NNDSS, with
# outcomes denoted "IPD_*". The last eight columns are from Google Trends.
# Unfortunately, it is not remembered which dataset the "School" variable is from.
# 
# data.gov: https://data.cdc.gov/browse?category=NNDSS&sortBy=last_modified&pageSize=20&tags=invasive+pneumococcal+disease&page=1
# CDC site: https://www.cdc.gov/pneumococcal/php/surveillance/index.html
# 
# Other than these, the first things we notice are:
#   - There are a lot of NA's following a "sub_region_1" variable. It is not
#     yet clear if "sub_region_1" differs from "ReportingArea".
#   - There is this unknown variable called "School".
#   - "MMWRWeek" is represented twice.
#   - There are six 7-day average variables for specific locations. 
#     Most entries are NA, possibly they are only related to "sub_region_1"?


## --------------------
## Examine the primary metadata variables

nndss_raw$ReportingArea %>% unique()

# Quite a lot of messiness here. We see that there are states, US territories,
# and census regions and divisions:
# https://www2.census.gov/geo/pdfs/maps-data/maps/reference/us_regdiv.pdf
#
# A number of these entries are also duplicated as a consequence of variations
# in nomenclature. If these were condensed by unique "MMWRYear" and "MMWRWeek",
# then the only concern would be with inconsistencies from "sub_region_1" or
# "mmwr_week". As far as can be discerned, the nine columns reflecting
# occurrences are counts and not rates or cumulative, implying a simple sum 
# is acceptable.
#
# After some nomenclature cleaning is done we can confirm that all states,
# territories, and census regions/divisions are represented.

# The data spans from 2016 to 2021, but we see that there is missing data. The
# missing data is identified in the summary tables where there are different
# combinations of outcomes. They are not all the same length as would otherwise 
# be expected.
nndss_raw$MMWRYear %>% unique()

# Different number of entries for each year.
nndss_raw %>% count(MMWRYear) %>% (\(x) {unique(x$n)} ) %>% sort()
# Different number of entries for each year and MMWRWeek combination.
nndss_raw %>% count(MMWRYear, MMWRWeek) %>% (\(x) {unique(x$n)} ) %>% sort()

# Different number of entries for each year and geolocation.
nndss_raw %>% count(MMWRYear, ReportingArea) %>% (\(x) {unique(x$n)} ) %>% sort()
# Different number of entries for each MMWRWeek and geolocation.
nndss_raw %>% count(MMWRWeek, ReportingArea) %>% (\(x) {unique(x$n)} ) %>% sort()


## --------------------
## Examine the nine occurrences columns

# Earlier we assumed that the nine occurrences columns are not cumulative and
# are integers (i.e. not rates) thus representing counts. We will quickly confirm
# these assumptions are correct.
# 
# In absence of other information about the data, it would be difficult to 
# know for sure. This step is only used as a heuristic to confirm that there are
# no obvious indications that would contradict our assumption.
#
# Columns "IPD_*" are from the NNDSS surveillance program, which are sometimes
# reported as cumulative values, but we would expect that to be indicated in
# the variable name itself. The "7dayavg_*" columns are Google Trends data,
# which are not cumulative.

# Create summary Boolean test if each slice of data is cumulative.
cum_test <- nndss_raw[, -c(7:9)] %>%
  group_by(ReportingArea, MMWRYear) %>%
  arrange(MMWRWeek) %>%
  summarise(across(IPD_all:`7dayavg_res`, ~ all(diff(.) >= 0)), .groups = 'drop') %>%
  as.data.frame()

# Function to count TRUE, FALSE, and NA over the columns.
count_values <- function(x) {
  tibble(
    TRUE_count = sum(x == TRUE, na.rm = TRUE),
    FALSE_count = sum(x == FALSE, na.rm = TRUE),
    NA_count = sum(is.na(x))
  )
}

# Execute the row-wise Boolean test and unnest the results.
cum_result <- cum_test[, -c(1:2)] %>%
  summarise(across(everything(), ~ count_values(.))) %>%
  pivot_longer(cols = everything(), names_to = "column", values_to = "counts") %>%
  unnest(cols = counts) %>%
  pivot_longer(cols = -column, names_to = "count_type", values_to = "count") %>%
  pivot_wider(names_from = column, values_from = count) %>%
  as.data.frame()

# The vast majority are FALSE, implying that it is safe to assume these are NOT
# cumulative counts. We see that some outcomes get identified as cumulative,
# but that could be viewed as incidental and not indicative of any real trend.

# Test each double numeric to see if it has any decimal points.
are_integers <- nndss_raw[, -c(7:9)] %>% 
  summarise(across(IPD_all:`7dayavg_res`, ~ all(is.na(.) | . == floor(.)))) %>%
  as.data.frame()

# This test confirms that all entries are integers. Again, this is not exactly
# conclusive, but very likely these are not rates, which would usually include
# a decimal.


## --------------------
## Examine "School"

# Without a data dictionary, it is not clear what these levels of "School"
# could mean. No documentation was found on the CDC's and data.gov page for
# NNDSS weekly data.
unique(nndss_raw$School)

# We're also not sure which dataset it's associated with, either the NNDSS or
# Google Trends side. Since we don't know what the variable means, we want to
# remove it all together. Before doing this, we need to identify if any
# entries are unique as a consequence of this variable. If there are any, then
# we need to collapse those entries so we do not loose any values.

# The number of unique rows for NNDSS metadata stays the same, even when "School"
# is considered, implying that it is not adding new details.
unique(nndss_raw[, c(1:3)]) |> nrow()
unique(nndss_raw[, c(1:3, 7)]) |> nrow()

# This is not the case for Google Trends, implying that there is some
# differentiation occurring that needs to be reconciled.
unique(nndss_raw[, c(8:9)]) |> nrow()
unique(nndss_raw[, c(7:9)]) |> nrow()


## --------------------
## Examine all secondary metadata variables

# "sub_region_1" and "mmwr_week" are included, likely as part of joining the
# NNDSS and Google Trends datasets. We want to ensure that they are consistent
# with the primary metadata we described earlier.

# It appears that the non-NA "sub_region_1" match with "ReportingArea" based on
# this visual inspection.
nndss_raw[, c("ReportingArea", "sub_region_1")] %>%
  filter(!is.na(sub_region_1)) %>% unique() %>% as.data.frame()


# Similarly, "MMWRWeek" and "mmwr_week" appear to be the same. We have some
# NA's from the matches. We can confirm that these are related to
# "mmwr_week" that are NA and not due to any problem from the matching.
(nndss_raw$MMWRWeek == nndss_raw$mmwr_week) %>% 
  (\(x) {table(x, useNA = "ifany")} )

# The number of NA's matches the number of matches that resulted in an NA,
# confirming these variables are displaying the same information.
table(is.na(nndss_raw$mmwr_week), useNA = "ifany")


# All the expected states are represented in the "sub_region_1" variable,
# and the only ones that are not states are census regions and divisions.
# There is not as many duplication as a result of differences with nomenclature.
unique(nndss_raw$sub_region_1)

# Non-states are census regions and divisions (there should be nine)
unique(nndss_raw$sub_region_1) %>% .[. %!in% c("District of Columbia", datasets::state.name)]
# All the states and the District of Columnbia is included.
c("District of Columbia", datasets::state.name) %in% unique(nndss_raw$sub_region_1) %>% unique()


# Based on the organization of the columns, we'd expect that these two metadata
# are clearly associated with one-another. Unfortunately, we see that sometimes
# they cross-over, but every possible combination of the Boolean tests is
# present, showing they are not perfectly aligned.
nndss_raw[, c("mmwr_week", "sub_region_1")] %>% 
  mutate(
    mmwr_week_is_na = is.na(mmwr_week),
    sub_region_1_is_na = is.na(sub_region_1)
  ) %>%
  select(-mmwr_week, -sub_region_1) %>%
  distinct(mmwr_week_is_na, sub_region_1_is_na)


# The "7dayavg_*" columns are expected to be associated with the "sub_region_1"
# and "mmwr_week" metadata. We can take a look at the alignment between these
# metadata columns and the two sets of metrics. We'd hope to see mutually exclusive
# outcomes, where no "IPD_*" are reported with "sub_region_1"/"mmwr_week" and
# only "7dayavg_*" are reported in this case. 
#
# We can test this assumption in the following tables.

# Examine if "sub_region_1" NA's align with the NNDSS, where NNDSS metrics
# are NA if "sub_region_1" is not NA and vice versa.
sub_reg_exclusive <- sapply(nndss_raw[, c(4:9)], function(x) is.na(x))
table("IPD_all" = sub_reg_exclusive[, 1], "sub_region_1" = sub_reg_exclusive[, 5], "mmwr_week" = sub_reg_exclusive[, 6])
table("IPD_u5" = sub_reg_exclusive[, 2], "sub_region_1" = sub_reg_exclusive[, 5], "mmwr_week" = sub_reg_exclusive[, 6])
table("IPD_o5" = sub_reg_exclusive[, 3], "sub_region_1" = sub_reg_exclusive[, 5], "mmwr_week" = sub_reg_exclusive[, 6])
table("School" = sub_reg_exclusive[, 4], "sub_region_1" = sub_reg_exclusive[, 5], "mmwr_week" = sub_reg_exclusive[, 6])

# Examine if "sub_region_1" NA's align with the Google Trends metrics in
# similar fashion.
sub_reg_components <- sapply(nndss_raw[, c(8:15)], function(x) is.na(x))
table("7dayavg_retail_rec" = sub_reg_components[, 3], "sub_region_1" = sub_reg_components[, 1], "mmwr_week" = sub_reg_components[, 2])
table("7dayavg_grocery" = sub_reg_components[, 4], "sub_region_1" = sub_reg_components[, 1], "mmwr_week" = sub_reg_components[, 2])
table("7dayavg_parks" = sub_reg_components[, 5], "sub_region_1" = sub_reg_components[, 1], "mmwr_week" = sub_reg_components[, 2])
table("7dayavg_transit" = sub_reg_components[, 6], "sub_region_1" = sub_reg_components[, 1], "mmwr_week" = sub_reg_components[, 2])
table("7dayavg_work" = sub_reg_components[, 7], "sub_region_1" = sub_reg_components[, 1], "mmwr_week" = sub_reg_components[, 2])
table("7dayavg_res" = sub_reg_components[, 8], "sub_region_1" = sub_reg_components[, 1], "mmwr_week" = sub_reg_components[, 2])


# Unfortunately, the results are not showing up as expected. There are some
# variables that are never NA when "sub_region_1" is also not NA. When "sub_region_1"
# is NA, then sometimes that metric is NA sometimes not NA, thus showing there
# is not a clear association between them. This is not improved when considering
# "mmwr_week", which itself does not perfectly align with "sub_region_1".
# 
# Fortunately, because we showed the "sub_region_1" and "mmwr_week" align with
# the primary metadata provided in the first few columns, then we do not need
# to be concerned with any information loss.
# 
# Including the "School" variable did not clarify it's association with either
# dataset. It appears "School" only creates unique combinations with the
# Google Trends, and will therefore be handled as if that is the case.




## ----------------------------------------------------------------
## CLEAN THE DATA


## --------------------
## ReportingArea nomenclature

# Previously, we noticed that there are a lot of formatting problems and
# unexpected outcomes under the "ReportingArea" variable.

unique(nndss_raw$ReportingArea)

nndss_raw$ReportingArea <- str_to_title(nndss_raw$ReportingArea) %>%
  str_replace_all("Amer. Samoa", "American Samoa") %>%
  str_replace_all("C.n.m.i.", "Northern Mariana Islands") %>%
  str_replace_all("Dist. Of Col.", "District Of Columbia") %>%
  str_replace_all("E.n. Central", "East North Central") %>%
  str_replace_all("E.s. Central", "East South Central") %>%
  str_replace_all("Mid. Atlantic", "Middle Atlantic") %>%
  str_replace_all("S. Atlantic", "South Atlantic") %>%
  str_replace_all("S. Atlantic", "South Atlantic") %>%
  str_replace_all("U.s. Virgin Islands", "US Virgin Islands") %>%
  str_replace_all("Us Virgin Islands", "US Virgin Islands") %>%
  str_replace_all("Virgin Isl\\.", "US Virgin Islands") %>%
  str_replace_all("W.n. Central", "West North Central") %>%
  str_replace_all("W.s. Central", "West South Central") %>%
  str_replace_all("Us Residents", "US Residents") %>%
  str_replace_all("Non-Us Residents", "Non-US Residents") %>%
  str_replace_all("District Of Columbia", "District of Columbia") %>%
  str_replace_all("Us Territories", "US Territories")

# All states are present.
c(datasets::state.name, "District of Columbia") %>% .[. %in% unique(nndss_raw$ReportingArea)] %>% length == 51

# For the other entries, notice a few things:
#   - New York City separately from the total state of New York.
#   - We have categories for US Residents, Non-Residents, and US Territories.
#   - Total seems to denote all of residents, non-residents, and territories.
unique(nndss_raw$ReportingArea) %>% .[. %!in% c(datasets::state.name, "District of Columbia")] %>% sort()

# Unfortunately, we still see that there are missing values, since a different
# number of weeks is counted for each unique combination of ReportingArea and
# MMWRYear.
nndss_raw %>% count(MMWRYear, ReportingArea) %>% (\(x) {unique(x$n)} ) %>% sort()
nndss_raw %>% count(MMWRWeek, ReportingArea) %>% (\(x) {unique(x$n)} ) %>% sort()

# All the non-states have every week noted for each year, as expected.
nndss_raw %>% 
  filter(ReportingArea %!in% c(datasets::state.name, "District of Columbia")) %>%
  count(MMWRYear, ReportingArea) %>% 
  (\(x) {unique(x$n)} ) %>% sort()

# This is not the case for states, unless the year 2016 is excluded. Then we
# see that only 2016 did not have each week recorded for each state.
nndss_raw %>% 
  filter(ReportingArea %in% c(datasets::state.name, "District of Columbia")) %>%
  filter(MMWRYear %!in% 2016) %>%
  count(MMWRYear, ReportingArea) %>%
  (\(x) {unique(x$n)} ) %>% sort()

# Not all of the non-states are represented for each year. One is only represented
# under one year, while most have five years represented.
nndss_raw %>% 
  filter(ReportingArea %!in% c(datasets::state.name, "District of Columbia")) %>%
  count(MMWRWeek, ReportingArea) %>%
  (\(x) {table(x$n)/52} ) %>% sort()


# --------------------
## Split the tables

# Reorder the rows in decreasing area for clarity.
custom_order <- c("Total", "United States", "US Residents", "US Territories", 
                  "Non-US Residents", "East North Central", "East South Central", 
                  "Middle Atlantic",  "Mountain", "New England", "Pacific", 
                  "South Atlantic", "West North Central", "West South Central", 
                  "American Samoa", "Northern Mariana Islands", "Guam", 
                  "Puerto Rico", "US Virgin Islands", datasets::state.name[1:8], 
                  "District of Columbia", datasets::state.name[9:50], "New York City")

nndss_raw <- nndss_raw %>%
  mutate(ReportingArea = factor(ReportingArea, levels = custom_order)) %>%
  arrange(ReportingArea) %>%
  mutate(ReportingArea = as.character(ReportingArea))

# Convert the columns from double to integer.
nndss_raw <- nndss_raw %>%
  mutate(across(where(is.double), ~ as.integer(.)))


# Previously, we found that "sub_region_1" and "mmwr_week" do not contribute
# new information, and so we will remove them.
nndss_raw <- nndss_raw %>%
  select(-sub_region_1, -mmwr_week)

# Separate the tables, each with a copy of the metadata columns.
nndss <- nndss_raw %>%
  select(colnames(nndss_raw)[1:6])

google_trends <- nndss_raw %>%
  select(colnames(nndss_raw)[c(1:3, 7:13)]) %>%
  filter(
    !(is.na(`7dayavg_retail_rec`) &
        is.na(`7dayavg_grocery`) &
        is.na(`7dayavg_parks`) &
        is.na(`7dayavg_transit`) &
        is.na(`7dayavg_work`) &
        is.na(`7dayavg_res`))
  )




## --------------------
## ReportingArea duplications

# Previously we found that New York City was separated from the state itself.
# It's not totally clear if New York the state excludes New York City. If the
# state includes New York City counts, then we expect it to be always at least
# the same value or more than what is reported that week for the city.

# Separate city and state into two tables for comparison.
ny <- nndss %>% filter(ReportingArea %in% c("New York"))
ny_city <- nndss %>% filter(ReportingArea %in% c("New York City"))

# Perform an inner join, keeping only row matches between both.
joined_df <- ny %>%
  inner_join(ny_city, by = c("MMWRYear", "MMWRWeek"), suffix = c("_ny", "_nyc"))

# Calculate the differences for all columns that exist in both dataframes.
difference_columns <- colnames(ny)[!(colnames(ny_city) %in% c("ReportingArea", "MMWRYear", "MMWRWeek"))]
differences_df <- joined_df %>%
  mutate(
    IPD_all_diff = IPD_all_ny - IPD_all_nyc,
    IPD_u5_diff = IPD_u5_ny - IPD_u5_nyc,
    IPD_o5_diff = IPD_o5_ny - IPD_o5_nyc
  ) %>%
  select(ReportingArea_ny, MMWRYear, MMWRWeek, IPD_all_diff, IPD_u5_diff, IPD_o5_diff)

# We see that sometimes the city counts exceed the state counts. Therefore, we
# expect that the city and state a mutually exclusive, and we need to combine them.

nndss <- nndss %>%
  mutate(
    ReportingArea = ifelse(ReportingArea %in% c("New York", "New York City"), "New York", ReportingArea)
  ) %>%
  group_by(ReportingArea, MMWRYear, MMWRWeek) %>%
  summarise(across(starts_with("IPD"), sum, na.rm = TRUE), .groups = 'drop')

# Doing this reorders the rows, so we need to re-do that operation.
nndss <- nndss %>%
  mutate(ReportingArea = factor(ReportingArea, levels = custom_order)) %>%
  arrange(ReportingArea) %>%
  mutate(ReportingArea = as.character(ReportingArea))


# There does not appear to be any duplicate entries that could be differentiated
# by the School variable. Therefore, we can simply remove it, since it does not
# add new information.
google_trends %>% 
  count(ReportingArea) %>% 
  (\(x) {unique(x$n)} ) %>% sort()    # Result <52

google_trends %>% 
  count(ReportingArea, MMWRWeek) %>% 
  (\(x) {unique(x$n)} ) %>% sort()    # Result 1


google_trends <- google_trends %>%
  select(-School)




## ----------------------------------------------------------------
## SAVE RESULTS

write_csv(nndss, "Data/nndss.csv")
write_csv(google_trends, "Data/google_trends.csv")






