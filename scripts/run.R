library(sf)
library(tidyverse)
library(here)

source(here("scripts/funcs.R"))

# --------------------------- 
# PRE-PROCESS STREETS
# ---------------------------

netascore_file = here("data/salzburgerland-netascore-20240123.gpkg")

streets =  netascore_file |>
  read_sf(layer = "edge") |>
  rename_with(\(x) gsub("_ft", "", x), ends_with("_ft")) |>
  select(
    osm_id,
    access_car,
    access_bicycle,
    access_pedestrian,
    bicycle_infrastructure,
    road_category,
    pavement,
    gradient,
    index_bike
  )

# --------------------------- 
# PRE-PROCESS HOUSEHOLDS
# ---------------------------

address_file = here("data/austria-addresses-20211001.csv")

households = address_file |>
  read_delim(delim = ";", col_types = "cccccccccccccccddicc") |>
  select(ADRCD, GKZ, RW, HW, EPSG) |>
  filter(GKZ == "50101") |>
  group_by(EPSG) |>
  group_split() |>
  lapply(\(x) st_transform(st_as_sf(x, crs = x$EPSG[1], coords = c("RW", "HW")), st_crs(streets))) |>
  bind_rows() |>
  select(-EPSG, -GKZ) |>
  rename(id = ADRCD)

households$pop = 1

# ---------------------------
# DEFINE HUB LOCATION
# ---------------------------

bike_parking_1 = st_sfc(st_point(c(13.046598, 47.811632)), crs = 4326)
bike_parking_2 = st_sfc(st_point(c(13.044484, 47.813867)), crs = 4326)
bike_parking_3 = st_sfc(st_point(c(13.043535, 47.813183)), crs = 4326)

hub = st_sf(geometry = c(bike_parking_1, bike_parking_2, bike_parking_3)) |>
  st_transform(st_crs(streets))

# --------------------------- 
# CREATE DATA
# ---------------------------

data = prepare_data(hub, streets, households)

# ---------------------------
# COMPUTE ACCESSIBILITY
# ---------------------------

data$accessibility = compute_accessibility(
  data,
  index_thresholds = seq(0, 1, by = 0.05),
  detour_thresholds = seq(1, 2, by = 0.1)
)

# ---------------------------
# VISUALIZE AND EXPORT
# ---------------------------

map_data(data$network, data$households)
ggsave(here("plots/data_map.png"), width = 16, height = 16.8, units = "cm", dpi = 300)

map_accessibility(data$accessibility, data$network, data$households)
ggsave(here("plots/accessibility_map.png"), width = 16, height = 16, units = "cm", dpi = 300)

plot_accessibility(data$accessibility)
ggsave(here("plots/accessibility_plot.png"), width = 16, height = 11, units = "cm", dpi = 300)

save(data, file = here("data/data.RData"))
