library(sf)
library(sfnetworks)
library(tidyverse)
library(tidygraph)
library(tidyterra)
library(maptiles)
library(here)
library(units)

prepare_data = function(hub, streets, households, directed = FALSE,
                        proximity_threshold = 3000,
                        index_column = "index_bike", 
                        population_column = "pop") {
  # Define the extent of the network.
  # This extent exceeds the proximity threshold.
  # Such that routes from households inside the proximity threshold ...
  # ... can use streets that fall just outside of the proximity threshold.
  extent = st_buffer(hub, set_units(proximity_threshold * 1.5, "m"))
  # Create the network object.
  network = streets |>
    st_filter(extent) |>
    as_sfnetwork(directed = directed) |>
    activate("edges") |>
    rename(index = all_of(index_column)) |>
    mutate(length = edge_length())
  # Remove footpaths.
  network = network |>
    activate("edges") |>
    filter(access_bicycle | access_car) |>
    activate("nodes") |>
    filter(!node_is_isolated())
  # Assign each street on which cycling is not allows an index of NA
  # Do note that streets for which the index could not be computed are also NA.
  # That means those streets are assumed to be not allowed for cycling.
  network = network |>
    activate("edges") |>
    mutate(index = replace(index, !access_bicycle, NA))
  # Select only the largest connected component of the network.
  # Such that small disconnected components are removed.
  network = network |>
    convert(to_largest_component, .clean = TRUE)
  # Assign an index to each node.
  network = network |>
    activate("nodes") |>
    mutate(id = seq_len(n()))
  # Mark the nodes within the proximity threshold of the hub.
  # This defines the core of our analysis area.
  # Only households located within this part of the network are analyzed.
  for (i in seq_len(nrow(hub))) {
    orig = st_geometry(hub)[i]
    network = network |>
      activate("nodes") |>
      morph(to_spatial_neighborhood, orig, proximity_threshold, weights = "length") |>
      mutate(in_proximity = TRUE) |>
      unmorph() |>
      mutate(in_proximity = replace_na(in_proximity, FALSE))
  }
  # Mark the nodes that represent the hub.
  # These are the nearest node to each of its bicycle parking facilities.
  hub$node = st_nearest_feature(hub, st_geometry(network, "nodes"))
  hub_nodes = hub |>
    st_drop_geometry() |>
    filter(!is.na(node)) |>
    mutate(is_hub = TRUE) |>
    group_by(node) |>
    summarize(is_hub = any(is_hub))
  network = network |>
    activate("nodes") |>
    left_join(hub_nodes, by = join_by(id == node)) |>
    mutate(is_hub = replace_na(is_hub, FALSE))
  # Pre-filter the households by circular buffers around the hub locations.
  # Households outside of the proximity threshold are not analyzed.
  buffer = st_buffer(hub, set_units(proximity_threshold, "m"))
  households = households |>
    st_filter(buffer)
  # Identify and rename the population column
  households = households |>
    rename(pop = all_of(population_column))
  # Find the nearest node to each household.
  nodes = st_as_sf(network, "nodes")
  households$node = st_nearest_feature(households, nodes)
  # Only keep households for which the nearest node is in proximity of the hub.
  # In contradiction to the pre-filter, this uses network distance.
  households = households |>
    filter(nodes$in_proximity[node])
  # Join household information to their nearest nodes in the network.
  household_nodes = households |>
    st_drop_geometry() |>
    filter(!is.na(node)) |>
    group_by(node) |>
    summarize(households = list(id), pop = sum(pop))
  network = network |>
    activate("nodes") |>
    left_join(household_nodes, by = join_by(id == node)) |>
    mutate(pop = replace_na(pop, 0))
  # Return both the constructed street network and the updated household data.
  list(network = network, households = households)
}

compute_accessibility = function(data, index_thresholds = seq(0, 1, by = 0.05),
                                 detour_thresholds = seq(1, 2, by = 0.1)) {
  # Initialize output object.
  out = list()
  # Extract the required components of the data.
  households = data$households
  network = data$network
  nodes = st_as_sf(network, "nodes")
  # Infer the indices of the nodes that:
  # --> Represent the hub (this will be the destination for routing).
  # --> Represent the households (these will be the origins for routing).
  to_idxs = which(nodes$is_hub)
  from_idxs = unique(households$node)
  from_idxs = from_idxs[!is.na(from_idxs)]
  # Infer the total population that lives in the proximity of the hub.
  # This will be used to compute population shares for different thresholds.
  total_pop = sum(households$pop)
  # Compute travel costs on the full network.
  # This is the minimum of shortest path costs to one of the hub entrances.
  shortest_costs = st_network_cost(
    network,
    from = from_idxs,
    to = to_idxs,
    weights = "length"
  )
  shortest_costs = apply(shortest_costs, 1, min)
  # Compute travel costs on the bikeable network, i.e. "bikeable travel costs".
  # What the bikeable network is, is defined by the given index thresholds.
  for (i in seq_along(index_thresholds)) {
    # Create the bikeable network.
    # By removing all streets with bicycle suitability lower than the index threshold.
    it = index_thresholds[i]
    bikeable_network = network |>
      activate("edges") |>
      filter(index >= it)
    # Compute bikeable travel costs.
    # This is the minimum of shortest path costs to one of the hub entrances.
    bikeable_costs = st_network_cost(
      bikeable_network,
      from = from_idxs,
      to = to_idxs,
      weights = "length"
    )
    bikeable_costs = apply(bikeable_costs, 1, min)
    # Compute the ratios between bikeable costs and shortest costs.
    # These we call the detours.
    detours = bikeable_costs / shortest_costs
    detours[which(from_idxs %in% to_idxs)] = 0
    # Subset households are "acceptably" connected to the hub.
    # This depends on the accepted detour compared to the full network routes.
    # Combine them into a single feature.
    subout = list()
    for (j in seq_along(detour_thresholds)) {
      dt = detour_thresholds[j]
      is_acceptable = !is.na(detours) & detours <= dt
      connected_households = filter(households, node %in% from_idxs[is_acceptable]) 
      acceptable_detours = detours[is_acceptable]
      if (nrow(connected_households) == 0) {
        pop = 0
        mindetour = NA
        maxdetour = NA
        avgdetour = NA
        geom = st_sfc(NA, crs = st_crs(households))
      } else {
        pops = nodes$pop[from_idxs[is_acceptable]]
        pop = sum(pops)
        mindetour = min(acceptable_detours)
        maxdetour = max(acceptable_detours)
        avgdetour = weighted.mean(acceptable_detours, pops)
        geom = st_union(st_geometry(connected_households))
      }
      subout[[j]] = st_sf(tibble(
        index_threshold = it,
        detour_threshold = dt,
        pop = pop,
        share = round(pop / total_pop * 100, 2),
        detour_longest = round(maxdetour, 2),
        detour_shortest = round(mindetour, 2),
        detour_mean = round(avgdetour, 2),
        geom = geom
      ))
    }
    out[[i]] = bind_rows(subout)
  }
  bind_rows(out)
}

map_accessibility = function(accessibility, network, households, zoom = 14,
                             indices = c(0.25, 0.5, 0.75), detours = c(1, 1.5, 2)) {
  data = accessibility |>
    filter(detour_threshold %in% detours) |>
    filter(index_threshold %in% indices) |>
    filter(!st_is_empty(geom))
  hub = network |>
    activate("nodes") |>
    filter(is_hub) |>
    st_geometry() |>
    st_combine() |>
    st_centroid()
  basemap = network |>
    activate("nodes") |>
    filter(in_proximity) |>
    st_bbox() |>
    st_as_sfc() |>
    st_transform(3857) |>
    get_tiles(provider = "CartoDB.Positron", zoom = zoom, crop = TRUE)
  ggplot(data) +
    geom_spatraster_rgb(data = basemap, maxcell = 5e9) +
    geom_sf(data = households, color = "darkgrey", size = 0.7) +
    geom_sf(aes(color = detour_threshold), size = 1, alpha = 0.8) +
    geom_sf(data = hub, cex = 5, pch = 8) +
    facet_grid(vars(index_threshold), vars(detour_threshold)) +
    scale_color_viridis_c(
      "Detour threshold",
      limits = c(1, 2),
      breaks = c(1, 1.5, 2)
    ) +
    theme(
      axis.text = element_blank(),
      axis.ticks = element_blank(),
      legend.position = "none"
    )
}

map_data = function(network, households, zoom = 14) {
  edges = network |>
    activate("nodes") |>
    filter(in_proximity) |>
    activate("edges") |>
    st_as_sf()
  hub = network |>
    activate("nodes") |>
    filter(is_hub) |>
    st_as_sf()
  basemap = network |>
    activate("nodes") |>
    filter(in_proximity) |>
    st_bbox() |>
    st_as_sfc() |>
    st_transform(3857) |>
    get_tiles(provider = "CartoDB.Positron", zoom = zoom, crop = TRUE)
  ggplot() +
    geom_spatraster_rgb(data = basemap, maxcell = 5e9) +
    geom_sf(data = edges, lwd = 1, aes(color = index)) +
    geom_sf(data = households, cex = 0.1) +
    geom_sf(data = hub, cex = 2, pch = 15, color = "firebrick") +
    scale_color_viridis_c(
      "Bicycle suitability index",
      limits = c(0, 1),
      breaks = c(0, 0.5, 1)
    ) +
    theme(
      axis.text = element_blank(),
      axis.ticks = element_blank(),
      legend.position = "bottom",
      legend.title = element_text(family = "AppleGothic", size = 8, vjust = 0.75),
      legend.text = element_text(family = "AppleGothic", size = 6)
    )
}

plot_accessibility = function(accessibility) {
  data = accessibility |>
    mutate(detour_threshold = as.factor(detour_threshold))
  ggplot(data, aes(x = index_threshold, y = share, group = detour_threshold, color = detour_threshold)) +
    geom_point() +
    geom_line() +
    xlab(
      "Index threshold"
    ) +
    ylab(
      "Share of households with bikeable access to the hub [%]"
    ) +
    scale_color_viridis_d(
      "Detour threshold"
    ) +
    theme(
      axis.title = element_text(family = "AppleGothic", size = 8),
      axis.text = element_text(family = "AppleGothic", size = 6),
      legend.position = "bottom",
      legend.direction = "horizontal",
      legend.title = element_text(family = "AppleGothic", size = 8),
      legend.text = element_text(family = "AppleGothic", size = 6)
    ) +
    guides(
      color = guide_legend(
        ncol = length(unique(data$detour_threshold)),
        title.position = "bottom",
        title.hjust = 0.5
      )
    )
}
