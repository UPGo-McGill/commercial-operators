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


