#### FIGURES ###################################################################

source("R/01_startup.R")



# Figure 1. Active/FREH listings ------------------------------------------

figure_1 <- 
  daily |> 
  filter(date >= "2017-01-01", status %in% c("A", "R")) |> 
  count(date) |> 
  mutate(type = "Active listings") |> 
  mutate(n = slider::slide_dbl(n, mean, .before = 6)) |> 
  bind_rows(
    FREH |> 
      filter(FREH, date >= "2017-01-01") |> 
      count(date) |> 
      left_join(GH |> 
                  st_drop_geometry() |> 
                  filter(date >= "2017-01-01") |> 
                  count(date), by = "date") |> 
      mutate(n.x = coalesce(n.x, 0),
             n.y = coalesce(n.y, 0),
             n = n.x + n.y) |> 
      select(-n.x, -n.y) |> 
      mutate(type = "Dedicated listings") |> 
      mutate(n = slider::slide_dbl(n, mean, .before = 6))) |> 
  ggplot(aes(date, n, colour = type, label = type)) +
  geom_textline(family = "Futura", straight = TRUE, linewidth = 1.5) +
  scale_x_date(name = NULL) +
  scale_y_continuous(name = NULL, labels = scales::comma) +
  scale_colour_manual(values = c("#225EA8", "#4AA59D")) +
  theme_minimal() +
  theme(legend.position = "none",
        text = element_text(family = "Futura"),
        plot.background = element_rect(fill = "white", colour = "transparent"))

ggsave("output/figure_1.png", figure_1, width = 8, height = 6)  


# Figure 2. Map -----------------------------------------------------------

library(patchwork)

province <- 
  get_census("CA16", regions = list(PR = "24"), geo_format = "sf") |> 
  st_transform(32618) |> 
  select(geometry)

active_map <- 
  daily |> 
  filter(date >= "2019-01-01", status %in% c("A", "R")) |> 
  left_join(select(st_drop_geometry(property), property_ID, GeoUID)) |> 
  count(GeoUID) |> 
  mutate(n = n / 365) 

active_map <- 
  CT |> 
  as_tibble() |> 
  st_as_sf() |> 
  select(GeoUID, dwellings = Dwellings, geometry) |> 
  left_join(active_map, by = "GeoUID") |> 
  mutate(active_pct = n / dwellings)

FREH_map <- 
  FREH |> 
  filter(FREH, date >= "2019-01-01") |> 
  left_join(select(st_drop_geometry(property), property_ID, GeoUID)) |> 
  count(GeoUID) |> 
  mutate(n = n / 365)

GH_map <-
  GH |> 
  filter(date >= "2019-01-01") |> 
  st_drop_geometry() |> 
  unnest(property_IDs) |> 
  select(property_ID = property_IDs, date) |> 
  semi_join(filter(daily, status %in% c("A", "R"))) |> 
  left_join(select(st_drop_geometry(property), property_ID, GeoUID)) |> 
  count(GeoUID) |> 
  mutate(n = n / 365)

commercial_map <- 
  FREH_map |> 
  full_join(GH_map, by = "GeoUID") |> 
  mutate(n.x = coalesce(n.x, 0),
         n.y = coalesce(n.y, 0),
         n = n.x + n.y) |> 
  select(GeoUID, n)

commercial_map <- 
  CT |> 
  as_tibble() |> 
  st_as_sf() |> 
  select(GeoUID, dwellings = Dwellings, geometry) |> 
  left_join(commercial_map, by = "GeoUID") |> 
  mutate(commercial_pct = n / dwellings)

fig_2_left <-
  active_map |> 
  ggplot() +
  geom_sf(data = province, colour = "transparent", fill = "grey93") +
  geom_sf(aes(colour = active_pct, fill = after_scale(alpha(colour, 0.8))), 
          lwd = 0.3) +
  geom_rect(xmin = 607000, ymin = 5038000, xmax = 614000, ymax = 5045000,
            fill = NA, colour = "black", size = 0.3) +
  scale_colour_viridis_b(name = "Listings as %\nof dwellings",
                         labels = scales::percent_format(1),
                         limits = c(0, 0.06), oob = scales::squish) +
  upgo::gg_bbox(active_map) +
  ggtitle("Average active listings") +
  theme_void() +
  theme(legend.position = "bottom", text = element_text(family = "Futura"),
        legend.text = element_text(size = 7))

fig_2_left_inset <- 
  fig_2_left +
  ggtitle(NULL) +
  coord_sf(xlim = c(607000, 614000), ylim = c(5038000, 5045000),
           expand = FALSE) +
  theme(legend.position = "none",
        panel.border = element_rect(fill = NA, colour = "black", size = 0.6))

fig_2_left_map <- 
  fig_2_left + 
  inset_element(fig_2_left_inset, left = 0.0, bottom = 0.47, right = 0.53, 
                top = 0.94, ignore_tag = TRUE)

fig_2_right <-
  commercial_map |> 
  ggplot() +
  geom_sf(data = province, colour = "transparent", fill = "grey93") +
  geom_sf(aes(colour = commercial_pct, fill = after_scale(alpha(colour, 0.8))), 
          lwd = 0.3) +
  geom_rect(xmin = 607000, ymin = 5038000, xmax = 614000, ymax = 5045000,
            fill = NA, colour = "black", size = 0.3) +
  scale_colour_viridis_b(name = "Listings as %\nof dwellings",
                         labels = scales::percent,
                         limits = c(0, 0.06),
                         oob = scales::squish,
                         guide = NULL) +
  ggtitle("Average commercial listings") +
  upgo::gg_bbox(active_map) +
  theme_void() +
  theme(legend.position = "none", text = element_text(family = "Futura"))

fig_2_right_inset <- 
  fig_2_right +
  ggtitle(NULL) +
  coord_sf(xlim = c(607000, 614000), ylim = c(5038000, 5045000),
           expand = FALSE) +
  theme(legend.position = "none",
        panel.border = element_rect(fill = NA, colour = "black", size = 0.6))

fig_2_right_map <- 
  fig_2_right + 
  inset_element(fig_2_right_inset, left = 0.0, bottom = 0.47, right = 0.53, 
                top = 0.96, ignore_tag = TRUE)

fig_2_layout <- "
AABB
AABB
AABB
AABB
AABB
AABB
AABB
AABB
CCCC
"

figure_2 <- fig_2_left_map + fig_2_right_map + guide_area() + 
  plot_layout(design = fig_2_layout, guides = "collect") +
  theme(legend.position = "bottom", text = element_text(family = "Futura"))

ggsave("output/figure_2.png", figure_2, width = 9, height = 5)
