get_path_length <- function(path_ids, verts_df) {
  ids <- unlist(path_ids)
  path_df <- verts_df[match(ids, verts_df$id), ]
  if (nrow(path_df) < 2) return(0)
  line_geom <- st_linestring(as.matrix(path_df[, c("x", "y")]))
  line_sf <- st_sfc(line_geom, crs = 4326) |> st_transform(32735)
  as.numeric(st_length(line_sf)) / 1000
}

get_path_line <- function(path_ids, verts_df) {
  ids <- unlist(path_ids)
  if (length(ids) < 2) return(NULL)
  path_df <- verts_df[match(ids, verts_df$id), ]
  coords <- st_coordinates(path_df)
  if (nrow(coords) < 2) return(NULL)
  colnames(coords) <- c("x", "y")
  st_linestring(coords)
}

find_paths <- function(points, polygons) {
  lapply(seq_len(nrow(points)), function(origin_i) {
    origin_point <- points[origin_i, ]
    buffer_geom <- st_buffer(origin_point, 10000)
    bbox <- st_bbox(buffer_geom)
    polygons_buffer <- st_filter(polygons, buffer_geom)
    
    streetnet <- dodgr_streetnet(bbox = bbox, expand = 0.05)
    graph <- weight_streetnet(streetnet, wt_profile = "hgv")
    verts <- dodgr_vertices(graph)
    if (!"x" %in% names(verts)) names(verts)[grep("^x", names(verts))] <- "x"
    if (!"y" %in% names(verts)) names(verts)[grep("^y", names(verts))] <- "y"
    
    verts_sf <- st_as_sf(verts, coords = c("x", "y"), crs = 4326, remove = FALSE)
    streets_sf <- st_as_sf(streetnet, crs = 4326)
    
    origin_coords <- matrix(st_coordinates(origin_point), ncol = 2)
    from_index <- match_points_to_graph(graph, origin_coords)
    from_id <- graph$from_id[from_index]
    
    lapply(seq_len(nrow(polygons_buffer)), function(i) {
      polygon <- polygons_buffer[i, ]
      polygon_proj <- st_transform(polygon, 32735)
      streets_proj <- st_transform(streets_sf, 32735)
      crossing_streets <- streets_proj[st_crosses(streets_proj, polygon_proj, sparse = FALSE), ]
      if (nrow(crossing_streets) == 0) return(NULL)
      
      verts_proj <- st_transform(verts_sf, 32735)
      verts_on_streets <- st_filter(verts_proj, crossing_streets)
      street_ids <- verts_on_streets$id
      tried_ids <- c()
      
      for (dest_id in street_ids) {
        if (dest_id %in% tried_ids) next
        tried_ids <- c(tried_ids, dest_id)
        path_try <- dodgr_paths(graph, from = from_id, to = dest_id)[[1]][[1]]
        if (is.null(path_try) || length(path_try) < 2) next
        
        matched_verts <- verts_sf[match(path_try, verts_sf$id), ]
        inside_flags <- st_within(matched_verts, polygon, sparse = FALSE)[, 1]
        inside_indices <- which(inside_flags)
        
        if (length(inside_indices) == 0) next

        valid_path <- path_try[1:inside_indices[1]]
        return(list(
          origin_index = origin_i,
          polygon_index = polygon$polygon_index,
          path = list(valid_path),
          length = get_path_length(list(valid_path), verts),
          verts_sf = verts_sf
        ))
      }
      NULL
    }) |> Filter(Negate(is.null), x = _)
  }) |> unlist(recursive = FALSE)
}
