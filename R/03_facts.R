#### FACTS FOR PAPER ###########################################################

source("R/01_startup.R")
qload("output/data.qsm", nthreads = availableCores())


# Montreal's STR market ranking -------------------------------------------

biggest_markets <- 
  daily_remote |> 
  filter(country %in% c("United States", "Canada"), start_date >= "2019-05-01",
         start_date >= "2019-08-31", status %in% c("A", "R")) |>
  group_by(country, region, city) |> 
  summarize(tot = as.numeric(sum(end_date - start_date + 1)), 
            .groups = "drop") |> 
  collect() |> 
  arrange(-tot) |> 
  slice(1:20)


# Montreal rental housing -------------------------------------------------

# Canada
get_census("CA16", region = list(C = "01"), 
           vectors = c("total" = "v_CA16_4836", "renter" = "v_CA16_4838")) |> 
  summarize(rent_pct = renter / total)

# Montreal CMA
get_census("CA16", region = list(CMA = "24462"), 
           vectors = c("total" = "v_CA16_4836", "renter" = "v_CA16_4838")) |> 
  summarize(rent_pct = renter / total)

# Montreal CSD
get_census("CA16", region = list(CSD = "2466023"), 
           vectors = c("total" = "v_CA16_4836", "renter" = "v_CA16_4838")) |> 
  summarize(rent_pct = renter / total)



# Montreal STR market -----------------------------------------------------

# Average 2019 active listings
daily |> 
  filter(date >= "2019-01-01", status %in% c("A", "R")) |> 
  count(date) |> 
  pull(n) |> 
  mean() |> 
  scales::comma(10)

FREH |> 
  filter(FREH, date == "2019-12-31") |> 
  count(date) |> 
  pull(n) |> 
  (\(x) x + GH |> 
     st_drop_geometry() |> 
     filter(date == "2019-12-31") |> 
     count(date) |> 
     pull(n))() |> 
  scales::comma(10)


# Number of FREH listings in June/July 2019 -------------------------------

FREH |> 
  filter(FREH, date >= "2019-06-01", date <= "2019-07-31") |> 
  pull(property_ID) |> 
  unique() |> 
  length() |> 
  scales::comma()
