### 3 - MONTREAL DATA ANALYSIS #################################################

source("R/01-helper_functions.R")
source("R/02-data_import.R")
load("Data/Montreal_data.Rdata")

### Prepare data ###############################################################

# Set up dates
start_2019 <- "2019-01-01"
end_2019 <- "2019-12-31"
july_2019 <- "2019-07-01"

end_2018 <- "2018-12-31"
end_2017 <- "2017-12-31"
end_2016 <- "2016-12-31"
start_2016 <- "2016-01-01"
end_2015 <- "2015-12-31"

# Exchange rate (average over last twelve months)
exchange_rate <- mean(1.3301, 1.3201, 1.3365,
                      1.3383, 1.3455, 1.3293, 
                      1.3098, 1.3275, 1.3243,
                      1.3187, 1.3236, 1.3168)

# Add revenue column
property <- 
  daily %>% 
  filter(date >= "2019-01-01", status == "R") %>% 
  group_by(property_ID) %>% 
  summarize(revenue = sum(price)) %>% 
  left_join(property, .)

# Create 2019 property file
LTM_property <- property %>% 
  filter(created <= end_2019, scraped >= start_2019, housing == TRUE)

### Active daily listings ######################################################

## Create objects
active_listings <- 
  daily %>% 
  filter(housing == TRUE) %>% 
  count(date)%>% 
  arrange(desc(date)) %>% 
  filter(date >= "2016-01-01")

## Active listings from property file
# All housing listings on Dec 31, 2019
nrow(filter(property, created <= end_2019, scraped >= end_2019, housing == TRUE))

# Peak active listings in 2019
active_listings %>% 
  filter(date >= start_2019) %>% 
  arrange(desc(n))

# Peak in 2018
active_listings %>% 
  filter(date > end_2017, date <= end_2018) %>% 
  arrange(desc(n))

#Peak in 2017
active_listings %>% 
  filter(date > end_2016, date <= end_2017) %>% 
  arrange(desc(n))

#Peak in 2016
active_listings %>% 
  filter(date > end_2015, date <= end_2016) %>% 
  arrange(desc(n))

# Housing listings over the last twelve months
nrow(LTM_property)

# Listing type breakdown on July 1, 2019
nrow(filter(property, created <= july_2019, scraped >= july_2019, listing_type == "Entire home/apt"))/
  nrow(filter(property, created <= july_2019, scraped >= july_2019))

nrow(filter(property, created <= july_2019, scraped >= july_2019, listing_type == "Private room"))/
  nrow(filter(property, created <= july_2019, scraped >= july_2019))

nrow(filter(property, created <= july_2019, scraped >= july_2019, listing_type == "Shared room"))/
  nrow(filter(property, created <= july_2019, scraped >= july_2019))

# Number of hosts over last twelve months
length(unique(LTM_property$host_ID))

# Hosts by listing type
LTM_property %>% 
  filter(listing_type == "Entire home/apt") %>% 
  select(host_ID) %>% 
  unique() %>% 
  nrow()/
  length(unique(LTM_property$host_ID))


### Which STR platforms are used in Montreal? ###################################

# Airbnb and not Homeaway
nrow(filter(LTM_property, !is.na(ab_property), is.na(ha_property)))

# Homeaway and not Airbnb
nrow(filter(LTM_property, !is.na(ha_property), is.na(ab_property)))

# Both Airbnb and Homeaway
nrow(filter(LTM_property, !is.na(ha_property), !is.na(ab_property)))

nrow(LTM_property)


### Which boroughs have the most listings? ####################################

borough_listings_LTM <- 
  LTM_property %>% 
  filter(created <= july_2019, 
         scraped >= july_2019) %>% 
  group_by(neighbourhood) %>% 
  count(neighbourhood) %>% 
  arrange(desc(n))


### How many listings were FREHs? (July 1, 2019) ######################################################

nrow(filter(FREH, date == july_2019, FREH == TRUE))


### Growth over time #########################################################

# 2016-2019 Growth
nrow(filter(property, created <= end_2019, scraped >= end_2019,
            housing == TRUE)) / 
  nrow(filter(property, created <= start_2016, scraped >= start_2016,
              housing == TRUE))

# 2016-2019 EH Growth
nrow(filter(property, created <= end_2019, scraped >= end_2019,
            listing_type == "Entire home/apt",  housing == TRUE)) / 
  nrow(filter(property, created <= start_2016, scraped >= start_2016,
              listing_type == "Entire home/apt", housing == TRUE))

# 2019 YOY Growth
nrow(filter(property, created <= end_2019, scraped >= end_2019,
            housing == TRUE)) / 
  nrow(filter(property, created <= end_2018, scraped >= end_2018,
              housing == TRUE))

# 2018 YOY Growth
nrow(filter(property, created <= end_2018, scraped >= end_2018,
            housing == TRUE)) / 
  nrow(filter(property, created <= end_2017, scraped >= end_2017,
              housing == TRUE))

# 2017 YOY Growth
nrow(filter(property, created <= end_2017, scraped >= end_2017,
            housing == TRUE)) / 
  nrow(filter(property, created <= end_2016, scraped >= end_2016,
              housing == TRUE))

# 2016 YOY Growth
nrow(filter(property, created <= end_2016, scraped >= end_2016,
            housing == TRUE)) / 
  nrow(filter(property, created <= end_2015, scraped >= end_2015,
              housing == TRUE))


### Listing type prevalence ####################################################

property %>% 
  filter(housing == TRUE) %>% 
  rename(`Listing type` = listing_type) %>% 
  filter(created <= end_2019, scraped >= end_2019) %>% 
  group_by(`Listing type`) %>% 
  summarize(`Number of listings` = n(),
            `Annual revenue` = sum(revenue, na.rm = TRUE),
            `Rev. per listing` = `Annual revenue` / n()) %>% 
  mutate(
    `% of all listings` = round(`Number of listings` /
                                  sum(`Number of listings`), 3),
    `% of all listings` = paste0(100 * `% of all listings`, "%"),
    `% of annual revenue` = `Annual revenue` / sum(`Annual revenue`)) %>% 
  mutate(
    `Annual revenue` = round(`Annual revenue`),
    `Annual revenue` = paste0("$", str_sub(`Annual revenue`, 1, -7), ".",
                              str_sub(`Annual revenue`, -6, -6), " million"),
    `% of annual revenue` = round(`% of annual revenue`, 3),
    `% of annual revenue` = paste0(100 * `% of annual revenue`, "%"),
    `Rev. per listing` = round(`Rev. per listing`),
    `Rev. per listing` = paste0("$", str_sub(`Rev. per listing`, 1, -4),
                                ",", str_sub(`Rev. per listing`, -3, -1))
  ) %>% view()

### Bedroom breakdown ##########################################################

property %>% 
  filter(created <= end_2019, scraped >= end_2019, housing == TRUE,
         listing_type == "Entire home/apt") %>% 
  count(bedrooms) %>% 
  mutate(percentage = n / sum(n))

### Revenue distribution and commercial operators ##############################

# LTM revenue
sum(LTM_property$revenue, na.rm = TRUE)

# LTM revenue by property type
filter(LTM_property, listing_type == "Entire home/apt") %>% 
  select(revenue) %>% 
  sum(na.rm = TRUE) / sum(LTM_property$revenue, na.rm = TRUE)


## Host revenue percentiles (2019)
daily %>%
  filter(housing == TRUE, date >= start_2019, status == "R") %>%
  group_by(host_ID) %>%
  summarize(rev = sum(price)*exchange_rate) %>%
  filter(rev > 0) %>%
  summarize(
    `Top 1%`  = sum(rev[rev > quantile(rev, c(0.99))] / sum(rev)),
    `Top 5%`  = sum(rev[rev > quantile(rev, c(0.95))] / sum(rev)),
    `Top 10%` = sum(rev[rev > quantile(rev, c(0.90))] / sum(rev)),
    `Top 20%` = sum(rev[rev > quantile(rev, c(0.80))] / sum(rev)))

## Host revenue percentiles (2016)
daily %>% 
  filter(housing == TRUE, date >= end_2015, date <= end_2016, status == "R") %>% 
  group_by(host_ID) %>% 
  summarise(rev = sum(price)*exchange_rate) %>% 
  filter(rev > 0) %>% 
  summarize(`Top 1%`  = sum(rev[rev > quantile(rev, c(0.99))] / sum(rev)),
            `Top 5%`  = sum(rev[rev > quantile(rev, c(0.95))] / sum(rev)),
            `Top 10%` = sum(rev[rev > quantile(rev, c(0.90))] / sum(rev)),
            `Top 20%` = sum(rev[rev > quantile(rev, c(0.80))] / sum(rev)))


## Median host income (2019)
LTM_property %>% 
  filter(revenue > 0) %>% 
  pull(revenue) %>% 
  quantile() %>% 
  as.list() %>% 
  as_tibble() %>% 
  select(-`0%`) %>% 
  set_names(c("25th percentile", "Median", "75th percentile", 
              "100th percentile")) %>% 
  mutate_all(round, -2)

## Median host income (2016)
property %>% 
  filter(created <= end_2016, scraped >= end_2015, housing == TRUE) %>% 
  filter(revenue > 0) %>% 
  pull(revenue) %>% 
  quantile() %>% 
  as.list() %>% 
  as_tibble() %>% 
  select(-`0%`) %>% 
  set_names(c("25th percentile", "Median", "75th percentile",
              "100th percentile")) %>% 
  mutate_all(round, -2)


## Top earning host(s)
host_revenue <- LTM_property %>% 
  group_by(host_ID) %>% 
  summarise(host_rev = sum(revenue)) %>% 
  arrange(desc(host_rev))

## Revenue of interview sample
interviewed_revenue <- host_revenue %>% 
  filter(host_ID == 43298240 | host_ID == 487327 | host_ID == 85530634 | host_ID == 43531 |
           host_ID == 129749545 | host_ID == 182103998 | host_ID == 84463268 | host_ID == 126452184 |
           host_ID == 683657 | host_ID == 93741077 | host_ID == 134010568 | host_ID == 126274289 | 
           host_ID == 3758382 | host_ID == 1015379 | host_ID == 5180021 | host_ID == 195649860 |
           host_ID == 6512090 | host_ID == 25284295 | host_ID == 3299104 | host_ID == 77816784 |
           host_ID == 193770682 | host_ID == 1132399 | host_ID == 96593768)

interviewed_revenue[is.na(interviewed_revenue)] <- 0

sum(interviewed_revenue$host_rev)

interviewed_revenue %>% 
  pull(host_rev) %>% 
  quantile() %>% 
  as.list() %>% 
  as_tibble() %>% 
  select(-`0%`) %>% 
  set_names(c("25th percentile", "Median", "75th percentile",
              "100th percentile")) %>% 
  mutate_all(round, -2)

## Top 20% earners
top_20pct <- LTM_property %>% 
  group_by(host_ID) %>% 
  summarise(host_rev = sum(revenue)) %>% 
  filter(host_rev > quantile(host_rev, c(0.80), na.rm = TRUE)) %>% 
  arrange(desc(host_rev)) %>% 
  filter(host_ID == 43298240 | host_ID == 487327 | host_ID == 85530634 | host_ID == 43531 |
           host_ID == 129749545 | host_ID == 182103998 | host_ID == 84463268 | host_ID == 126452184 |
           host_ID == 683657 | host_ID == 93741077 | host_ID == 134010568 | host_ID == 126274289 | 
           host_ID == 3758382 | host_ID == 1015379 | host_ID == 5180021 | host_ID == 195649860 |
           host_ID == 6512090 | host_ID == 25284295 | host_ID == 3299104 | host_ID == 77816784 |
           host_ID == 193770682 | host_ID == 1132399 | host_ID == 96593768)

## Multilistings
ML_table <- 
  daily %>% 
  group_by(date) %>% 
  summarize(Listings = mean(multi),
            Revenue = sum(price * (status == "R") * multi * exchange_rate, na.rm = TRUE) / 
              sum(price * (status == "R") * exchange_rate, na.rm = TRUE)) %>% 
  gather(Listings, Revenue, key = `Multilisting percentage`, value = Value)

ML_table %>% 
  filter(date == end_2019)

# Entire home multilistings
daily %>% 
  filter(listing_type == "Entire home/apt") %>% 
  group_by(date) %>% 
  summarize(Listings = sum(multi)) %>% 
  filter(date == end_2019)

### Housing loss ###############################################################

FREH %>% 
  filter(date == end_2019, FREH == T) %>% 
  count()

FREH %>% 
  count(date) %>% 
  ggplot() +
  geom_line(aes(date, n), colour = "black", size = 1) +
  theme_minimal() +
  scale_y_continuous(name = NULL) +
  ggtitle("FREH listings in Montreal")

GH <- read_csv("Data/GH.csv")

GH %>% 
  group_by(date) %>% 
  summarize(GH_units = sum(housing_units)) %>%
  ggplot() +
  geom_line(aes(date, GH_units), colour = "black", size = 1) +
  theme_minimal() +
  scale_y_continuous(name = NULL) +
  ggtitle("Units converted to ghost hostels in Montreal")

GH_total <- 
  GH %>% 
  group_by(date) %>% 
  summarize(GH_units = sum(housing_units)) %>% 
  pull(GH_units) %>% 
  rollmean(365, align = "right")

GH_total <- GH_total[(length(GH_total) + 1 - n_groups(FREH %>% group_by(date))):length(GH_total)]

housing_loss <- 
  FREH %>% 
  group_by(date) %>% 
  summarize(`Entire home/apt` = n()) %>% 
  mutate(`Private room` = as.integer(GH_total)) %>% 
  gather(`Entire home/apt`, `Private room`, key = `Listing type`,
         value = `Housing units`)

# Current housing loss figure
sum(filter(housing_loss, date == end_2019)$`Housing units`)

# YOY increase
sum(filter(housing_loss, date == end_2019)$`Housing units`) /
  sum(filter(housing_loss, date == end_2018)$`Housing units`)

## Relate housing loss to rental vacancy rate

vacancy_rate <- 1.5

housing <- 
  get_census("CA16", regions = list(CMA = 24462), level = "CMA", 
             vectors = c("v_CA16_4897", "v_CA16_405"))

housing %>% 
  select(`v_CA16_405: Private dwellings occupied by usual residents`,
         `v_CA16_4897: Total - Tenant households in non-farm, non-reserve private dwellings - 25% sample data`) %>% 
  set_names(c("Dwellings", "Tenants")) %>% 
  pull(Tenants) %>% 
  {. * vacancy_rate * (vacancy_rate - 1)}

### Number of long reservations

daily <- left_join(daily, property, by = "property_ID")

daily <- daily %>% 
  select(1:13, 18:19)

long_reservations <- 
  daily %>% 
  filter(status == "R") %>% 
  filter(start_2019 >= created, end_2019 <= scraped + 30) %>% 
  group_by(res_ID)

res_length <- 
  long_reservations %>%
  group_by(res_ID) %>% 
  summarize(nights = max(date) - min(date) + 1) %>% 
  filter(nights >= 28)

mean(res_length$nights)

### Listings likely in violation of principal residence requirement ############

## LFRML calculations

# Add ML field to property file
property <- 
  daily %>% 
  filter(date == end_2019) %>% 
  select(property_ID, multi) %>% 
  left_join(property, .) %>% 
  mutate(multi = if_else(is.na(multi), FALSE, multi))

# Add n_reserved and n_available fields
property <- 
  daily %>% 
  filter(status == "R") %>% 
  group_by(property_ID) %>% 
  summarize(n_reserved = n()) %>% 
  left_join(property, .)

property <- 
  daily %>% 
  filter(status == "R" | status == "A") %>% 
  group_by(property_ID) %>% 
  summarize(n_available = n()) %>% 
  left_join(property, .)

# Add LFRML field
property <- 
  property %>%
  group_by(host_ID, listing_type) %>% 
  mutate(LFRML = case_when(
    listing_type != "Entire home/apt" ~ FALSE,
    multi == FALSE                    ~ FALSE,
    n_available == min(n_available)   ~ TRUE,
    TRUE                              ~ FALSE)) %>% 
  ungroup()

# Resolve ties
property <- 
  property %>% 
  group_by(host_ID, listing_type) %>% 
  mutate(prob = sample(0:10000, n(), replace = TRUE),
         LFRML = if_else(
           sum(LFRML) > 1 & prob != max(prob), FALSE, LFRML)) %>% 
  select(-prob)

# Add GH status
GH_list <-
  GH %>% 
  filter(date == end_2019)

property <-
  property %>%
  mutate(GH = if_else(property_ID %in% GH_list, TRUE, FALSE))

# Add FREH status
property <- 
  FREH %>% 
  filter(date == end_2019) %>% 
  mutate(FREH = TRUE) %>% 
  left_join(property, .) %>% 
  mutate(FREH = if_else(is.na(FREH), FALSE, FREH))

# Add Legal field
legal <- 
  property %>%
  filter(housing == TRUE, created <= end_2019, scraped >= end_2019) %>% 
  mutate(legal = case_when(
    GH == TRUE                     ~ FALSE,
    listing_type == "Shared room"  ~ TRUE,
    listing_type == "Private room" ~ TRUE,
    FREH == TRUE                   ~ FALSE,
    LFRML == TRUE                  ~ TRUE,
    multi == TRUE                  ~ FALSE,
    TRUE                           ~ TRUE))

mean(legal$FREH, na.rm = TRUE)
mean(legal$GH, na.rm = TRUE)
mean(legal$LFRML, na.rm = TRUE)
mean(legal$multi, na.rm = TRUE)
mean(legal$legal, na.rm = TRUE)

# Legal by neighbourhood
legal %>% 
  group_by(neighbourhood) %>% 
  summarize(non_PR = length(legal[legal == FALSE]))
