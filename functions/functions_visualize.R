create_leaflet_map <- function(points, polygons, paths_info, connection_matrix) {
  points_sf <- st_as_sf(points)
  polygons_sf <- st_as_sf(polygons) |> mutate(polygon_id = row_number())
  points_sf$point_id <- seq_len(nrow(points_sf))

  m <- leaflet() %>%
    addTiles() %>%
    addPolygons(
      data = polygons_sf,
      color = "#999",
      weight = 1,
      fillOpacity = 0.2,
      label = ~paste("Polygon", polygon_id),
      group = "Polygons"
    ) %>%
    addCircleMarkers(
      data = points_sf,
      radius = 5,
      color = "black",
      label = ~paste("Point", point_id),
      group = "Points"
    )

  # Street Path Linien
  path_lines <- do.call(rbind, lapply(paths_info, function(p) {
    geom <- get_path_line(p$path, p$verts_sf)
    if (is.null(geom)) return(NULL)
    st_sf(origin_index = p$origin_index, polygon_index = p$polygon_index, geometry = st_sfc(geom, crs = 4326))
  }))

  if (!is.null(path_lines) && nrow(path_lines) > 0) {
    m <- m %>%
      addPolylines(
        data = path_lines,
        color = "#ff7700",
        weight = 3,
        label = ~paste0("Punkt ", origin_index, " ↔ Polygon ", polygon_index),
        group = "Street Paths"
      )
  }

  # Point-in-Polygon Linien
  pip_matches <- connection_matrix %>% filter(method == "point_in_polygon", connected)
  if (nrow(pip_matches) > 0) {
    pip_lines <- do.call(rbind, lapply(seq_len(nrow(pip_matches)), function(i) {
      pt <- points_sf[pip_matches$origin_index[i], ]
      pg <- polygons_sf[pip_matches$polygon_index[i], ]
      line <- st_union(st_centroid(pg), pt) |> st_cast("LINESTRING")
      st_sf(origin_index = pip_matches$origin_index[i],
            polygon_index = pip_matches$polygon_index[i],
            geometry = st_as_sfc(line, crs = 4326))
    }))
    m <- m %>%
      addPolylines(
        data = pip_lines,
        color = "#2a9d8f",
        weight = 2,
        dashArray = "4,6",
        label = ~paste0("Punkt ", origin_index, " IN Polygon ", polygon_index),
        group = "Point-in-Polygon"
      )
  }

  # Layer-Steuerung
  m <- m %>%
    addLayersControl(
      overlayGroups = c("Polygons", "Points", "Street Paths", "Point-in-Polygon"),
      options = layersControlOptions(collapsed = FALSE)
    )

  return(m)
}
