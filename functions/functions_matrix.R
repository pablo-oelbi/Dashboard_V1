create_connection_matrix <- function(points, polygons, paths_info) {
  method_types <- c("street_path", "point_in_polygon", "buffered_polygon", "attribute_match")
  connection_matrix <- expand.grid(
    origin_index = seq_len(nrow(points)),
    polygon_index = seq_len(nrow(polygons)),
    method = method_types,
    stringsAsFactors = FALSE
  ) |>
    mutate(connected = FALSE, path_length_km = NA, num_nodes = NA)

  # Trage Street Path Verbindungen ein
  for (p in paths_info) {
    idx <- with(connection_matrix, which(
      origin_index == p$origin_index &
        polygon_index == p$polygon_index &
        method == "street_path"
    ))
    connection_matrix$connected[idx] <- TRUE
    connection_matrix$path_length_km[idx] <- round(p$length, 3)
    connection_matrix$num_nodes[idx] <- length(unlist(p$path))
  }

  # Point-in-Polygon
  pip_matches <- st_within(points, polygons, sparse = FALSE)
  for (i in seq_len(nrow(points))) {
    for (j in seq_len(nrow(polygons))) {
      if (pip_matches[i, j]) {
        idx <- which(
          connection_matrix$origin_index == i &
            connection_matrix$polygon_index == j &
            connection_matrix$method == "point_in_polygon"
        )
        connection_matrix$connected[idx] <- TRUE
      }
    }
  }

  # Buffer-Matches
  buffer_matches <- st_intersects(st_buffer(points, 500), polygons, sparse = FALSE)
  inside_polygon <- st_within(points, polygons, sparse = FALSE)

  for (i in seq_len(nrow(points))) {
    for (j in seq_len(nrow(polygons))) {
      if(buffer_matches[i, j]) {
        idx <- which(
          connection_matrix$origin_index == i &
            connection_matrix$polygon_index == j &
            connection_matrix$method == "buffered_polygon"
        )
        connection_matrix$connected[idx] <- TRUE
      }
    }
  }

  return(connection_matrix)
}


summarize_connection_matrix <- function(connection_matrix) {
  summary <- connection_matrix %>%
    group_by(method) %>%
    summarise(total_connections = sum(connected), .groups = "drop")

  pip_total <- summary %>%
    filter(method == "point_in_polygon") %>%
    pull(total_connections)

  summary <- summary %>%
    mutate(percentage_increase = ifelse(
      method != "point_in_polygon" & pip_total > 0,
      ((total_connections - pip_total) / pip_total) * 100,
      0
    ))

  return(summary)
}