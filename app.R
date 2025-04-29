library(shiny)
library(shinyjs)
library(sf)
library(dplyr)
library(leaflet)
library(tidyr)
library(ggplot2)
library(plotly)

# Set working directory to the location of this script
# setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

safe_read <- function(path, reader) {
  if (!file.exists(path)) {
    stop(paste("Datei nicht gefunden:", path))
  }
  reader(path)
}

# Eigene Funktionen laden
source("functions/functions_path.R")
source("functions/functions_matrix.R")
source("functions/functions_visualize.R")

`%||%` <- function(a, b) if (!is.null(a)) a else b

# Lade Daten
points <- safe_read("shiny_data/points.geojson", st_read)
polygons <- safe_read("shiny_data/polygons.geojson", st_read)
paths_info <- safe_read("shiny_data/paths_info.rds", readRDS)
connection_matrix <- safe_read("shiny_data/connection_matrix.csv", read.csv)

method_labels <- c(
  "Punkt im Polygon" = "point_in_polygon",
  "Pufferzone" = "buffered_polygon",
  "Strassennetz" = "street_path"
)

ui <- navbarPage("Interaktive Verbindungsanalyse",
  useShinyjs(),
  tabPanel("Übersicht",
    sidebarLayout(
      sidebarPanel(
        checkboxGroupInput("methods", "Verbindungsmethoden auswählen:",
                           choices = method_labels,
                           selected = c("point_in_polygon", "buffered_polygon", "street_path")),
        verbatimTextOutput("point_info"),
        tableOutput("connection_summary"),
        width = 5
      ),
      mainPanel(
        leafletOutput("map", height = "700px"),
        width = 7
      )
    )
  ),
  tabPanel("Portfolio",
    sidebarLayout(
      sidebarPanel(
        h4("Portfolio-Übersicht"),
        textOutput("portfolio_n_mines"),
        textOutput("portfolio_total_area"),
        textOutput("portfolio_msa_avg"),
        textOutput("portfolio_critical"),

        hr(),
        h4("Filteroptionen"),
        selectInput("portfolio_index", "Index wählen:", choices = c("MSA", "Baumbedeckung", "Nicht-Baum Vegetation", "Nicht-bewachsene Fläche")),
        
        conditionalPanel(
          condition = "input.portfolio_index != 'MSA' && input.portfolio_tabs == 'Zeitreihe'",
          sliderInput("portfolio_year", "Zeitraum (Jahr):", min = 2000, max = 2025, value = c(2000, 2025)),
          checkboxInput("show_individual_lines", "Einzellinien pro Punkt anzeigen", value = TRUE),
          checkboxGroupInput(
            "timeseries_elements",
            "Anzuzeigende Elemente:",
            choices = c("Min-Max", "25%-75% Quantil", "Mittelwert"),
            selected = c("Min-Max", "Mittelwert")
          )
        ),
        
        checkboxGroupInput("portfolio_commodity", "Rohstoff:", choices = sort(unique(points$primary_commodity)), selected = unique(points$primary_commodity)),
        checkboxGroupInput("portfolio_region", "Region:", choices = sort(unique(points$country)), selected = unique(points$country)),
        
        conditionalPanel(
          condition = "input.portfolio_index == 'MSA' && input.portfolio_tabs == 'Boxplot'",
          selectInput("boxplot_group", "Vergleich nach:", choices = c("Region", "Rohstoff"))
        )
      ),
      mainPanel(
        h3("Analyse des Portfolios"),
        uiOutput("portfolio_tabs")
      )
    )
  )
)

server <- function(input, output, session) {
  output$portfolio_tabs <- renderUI({
    if (input$portfolio_index == "MSA") {
      tabsetPanel(
        id = "portfolio_tabs",   # <<< WICHTIG: ID vergeben
        tabPanel("Histogramm", plotOutput("plot_histogram")),
        tabPanel("Boxplot", plotOutput("plot_box"))
      )
    } else {
      tabsetPanel(
        id = "portfolio_tabs",   # <<< WICHTIG: ID vergeben
        tabPanel("Zeitreihe", plotlyOutput("plot_timeseries", height = "400px")),
        tabPanel("Dichte-Zeitreihe", plotOutput("plot_timeseries_density", height = "400px"))
      )
    }
  })
  output$plot_histogram <- renderPlot({
    pts_ids <- which(
      (points$country %in% input$portfolio_region) &
      (points$primary_commodity %in% input$portfolio_commodity)
    )

    # Verbindung von Punkten zu ihren verbundenen Polygonen
    connected <- connection_matrix %>%
      filter(origin_index %in% pts_ids, connected == TRUE)

    if (nrow(connected) == 0) return(NULL)

    # Verknüpfe MSA-Werte mit den Verbindungen
    joined <- connected %>%
      left_join(polygons %>% select(polygon_index, msa_mean), by = "polygon_index") %>%
      filter(!is.na(msa_mean)) %>%
      group_by(origin_index) %>%
      summarise(msa_avg = mean(msa_mean), .groups = "drop")

    if (nrow(joined) == 0) return(NULL)

    ggplot(joined, aes(x = msa_avg)) +
      geom_histogram(bins = 20, fill = "steelblue", color = "white") +
      labs(title = "MSA-Durchschnitt pro Punkt (verbundene Polygone)",
           x = "Durchschnittlicher MSA-Wert",
           y = "Anzahl Punkte")
  })

  output$plot_box <- renderPlot({
    pts_ids <- which(
      (points$country %in% input$portfolio_region) &
      (points$primary_commodity %in% input$portfolio_commodity)
    )

    connected <- connection_matrix %>%
      filter(origin_index %in% pts_ids, connected == TRUE)

    joined <- connected %>%
      left_join(polygons %>% select(polygon_index, msa_mean), by = "polygon_index") %>%
      filter(!is.na(msa_mean)) %>%
      left_join(points %>% mutate(origin_index = row_number()), by = "origin_index") %>%
      group_by(origin_index) %>%
      summarise(msa = mean(msa_mean),
                country = first(country),
                commodity = first(primary_commodity),
                .groups = "drop")

    if (nrow(joined) == 0) return(NULL)

    if (input$boxplot_group == "Region") {
      ggplot(joined, aes(x = country, y = msa)) +
        geom_boxplot(fill = "darkorange", alpha = 0.7) +
        labs(title = "MSA-Vergleich nach Region", x = "Region", y = "MSA") +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
    } else {
      ggplot(joined, aes(x = commodity, y = msa)) +
        geom_boxplot(fill = "forestgreen", alpha = 0.7) +
        labs(title = "MSA-Vergleich nach Rohstoff", x = "Rohstoff", y = "MSA") +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
    }
  })

  output$plot_timeseries <- renderPlotly({
    selected_index <- input$portfolio_index
    if (selected_index == "MSA") return(NULL)

    pts_ids <- which(
      (points$country %in% input$portfolio_region) &
      (points$primary_commodity %in% input$portfolio_commodity)
    )

    if (length(pts_ids) == 0) return(NULL)

    df <- read.csv("shiny_data/index_point_timeseries.csv")

    df_filtered <- df %>%
      filter(Index == selected_index, Punkt %in% pts_ids) %>%
      pivot_longer(cols = starts_with("X"), names_to = "Jahr", values_to = "Wert") %>%
      mutate(
        Jahr = as.numeric(gsub("X", "", Jahr)),
        production_start = points$production_start[Punkt],
        name = points$facility_name[Punkt],
        region = points$country[Punkt],
        commodity = points$primary_commodity[Punkt]
      ) %>%
      filter(Jahr >= input$portfolio_year[1], Jahr <= input$portfolio_year[2])

    if (nrow(df_filtered) == 0) return(NULL)

    y_limits <- range(df_filtered$Wert, na.rm = TRUE)

    df_summary <- df_filtered %>%
      group_by(Jahr) %>%
      summarise(
        ymin_minmax = min(Wert, na.rm = TRUE),
        ymax_minmax = max(Wert, na.rm = TRUE),
        ymin_quantile = quantile(Wert, 0.25, na.rm = TRUE),
        ymax_quantile = quantile(Wert, 0.75, na.rm = TRUE),
        Mittelwert = mean(Wert, na.rm = TRUE),
        .groups = "drop"
      )

    p <- ggplot()

    if ("Min-Max" %in% input$timeseries_elements) {
      p <- p + geom_ribbon(data = df_summary, aes(x = Jahr, ymin = ymin_minmax, ymax = ymax_minmax),
                           fill = "lightblue", alpha = 0.3)
    }

    if ("25%-75% Quantil" %in% input$timeseries_elements) {
      p <- p + geom_ribbon(data = df_summary, aes(x = Jahr, ymin = ymin_quantile, ymax = ymax_quantile),
                           fill = "lightgreen", alpha = 0.4)
    }

    if ("Mittelwert" %in% input$timeseries_elements) {
      p <- p + geom_line(
        data = df_summary,
        aes(
          x = Jahr,
          y = Mittelwert,
          group = 1,
          text = paste0("Jahr: ", Jahr, "<br>Mittelwert: ", round(Mittelwert, 2), "%")
        ),
        color = "darkblue", size = 1.5
      )
    }

    if (input$show_individual_lines %||% TRUE) {
      p <- p +
        geom_line(data = df_filtered,
                  aes(x = Jahr, y = Wert, group = Punkt, color = factor(Punkt),
                      text = paste0("Punkt: ", Punkt,
                                    "<br>Name: ", name,
                                    "<br>Region: ", region,
                                    "<br>Rohstoff: ", commodity,
                                    "<br>Jahr: ", Jahr,
                                    "<br>Wert: ", round(Wert, 2), "%")),
                  alpha = 0.6) +
        geom_point(data = df_filtered %>% filter(Jahr == production_start),
                   aes(x = Jahr, y = Wert, group = Punkt, fill = factor(Punkt),
                       text = paste0("Produktionsstart<br>Punkt: ", Punkt,
                                     "<br>Name: ", name,
                                     "<br>Region: ", region,
                                     "<br>Rohstoff: ", commodity,
                                     "<br>Jahr: ", Jahr,
                                     "<br>Wert: ", round(Wert, 2), "%")),
                   shape = 21, color = "black", size = 2) +
        guides(color = "none", fill = "none")
    }

    p <- p +
      labs(
        title = paste("Zeitreihe:", selected_index),
        x = "Jahr", y = "%",
        subtitle = NULL
      ) +
      theme_minimal() +
      coord_cartesian(ylim = y_limits)

    ggplotly(p, tooltip = "text") %>% layout(hovermode = "closest")
  })

  output$plot_timeseries_density <- renderPlot({
    selected_index <- input$portfolio_index
    if (selected_index == "MSA") return(NULL)

    pts_ids <- which(
      (points$country %in% input$portfolio_region) &
      (points$primary_commodity %in% input$portfolio_commodity)
    )

    if (length(pts_ids) == 0) return(NULL)

    df <- read.csv("shiny_data/index_point_timeseries.csv")

    df_filtered <- df %>%
      filter(Index == selected_index, Punkt %in% pts_ids) %>%
      pivot_longer(cols = starts_with("X"), names_to = "Jahr", values_to = "Wert") %>%
      mutate(
        Jahr = as.numeric(gsub("X", "", Jahr)),
        Wert = as.numeric(Wert)
      ) %>%
      filter(Jahr >= input$portfolio_year[1], Jahr <= input$portfolio_year[2])

    if (nrow(df_filtered) == 0) return(NULL)

    ggplot(df_filtered, aes(x = Jahr, y = Wert)) +
      stat_density_2d(aes(fill = after_stat(density)), geom = "raster", contour = FALSE) +
      scale_fill_viridis_c(option = "inferno") +
      labs(
        title = paste("Dichte der Zeitreihen:", selected_index),
        x = "Jahr", y = "%",
        fill = "Dichte"
      ) +
      theme_minimal()
  })
  output$map <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      addLayersControl(
        overlayGroups = c("Polygone", "Punkte", "Strassenpfade"),
        options = layersControlOptions(collapsed = FALSE)
      )
  })

  observe({
    bounds <- st_bbox(st_union(points))
    leafletProxy("map") %>%
      fitBounds(
        lng1 = as.numeric(bounds["xmin"]),
        lat1 = as.numeric(bounds["ymin"]),
        lng2 = as.numeric(bounds["xmax"]),
        lat2 = as.numeric(bounds["ymax"])
      )
  })

  filtered_connections <- reactive({
    connection_matrix %>%
      filter(method %in% input$methods, connected == TRUE)
  })

output$connection_summary <- renderTable({
  # Gesamtfläche aller Polygone
  total_area <- sum(st_area(polygons), na.rm = TRUE)

  # Alle verbundenen Polygone (egal mit welcher Methode)
  alle_verbundenen_polygone <- connection_matrix %>%
    filter(connected == TRUE) %>%
    distinct(polygon_index) %>%
    pull(polygon_index)

  # Fläche je Methode
  area_summary <- lapply(unique(connection_matrix$method), function(meth) {
    relevant_polygons <- connection_matrix %>%
      filter(method == meth, connected == TRUE) %>%
      distinct(polygon_index) %>%
      pull(polygon_index)

    flaeche <- sum(st_area(polygons %>% filter(polygon_index %in% relevant_polygons)), na.rm = TRUE)
    anzahl_polygone <- length(relevant_polygons)

    data.frame(
      Methode = meth,
      Anzahl_Polygone = anzahl_polygone,
      Fläche_km2 = as.numeric(flaeche) / 1e6,  # Umrechnen in km²
      Anteil_Prozent = paste0(round((as.numeric(flaeche)) / as.numeric(total_area) * 100, 1), " %")
    )
  }) %>%
    bind_rows() %>%
    mutate(
      Methode = recode(Methode,
                       "point_in_polygon" = "Punkt im Polygon",
                       "buffered_polygon" = "Pufferzone",
                       "street_path" = "Strassennetz")
    )

  # Fläche der nicht verbundenen Polygone
  unverbundene_polygone <- polygons %>%
    filter(!(polygon_index %in% alle_verbundenen_polygone))

  unverbundene_flaeche_km2 <- as.numeric(sum(st_area(unverbundene_polygone), na.rm = TRUE)) / 1e6
  anzahl_unverbundene <- nrow(unverbundene_polygone)

  # Zeile für unverbundene Polygone
  unverbunden_row <- data.frame(
    Methode = "Nicht verbundene Polygone",
    Anzahl_Polygone = anzahl_unverbundene,
    Fläche_km2 = round(unverbundene_flaeche_km2, 1),
    Anteil_Prozent = paste0(round(unverbundene_flaeche_km2 * 1e6 / as.numeric(total_area) * 100, 1), " %")
  )

  # Zeile für Gesamtfläche
  gesamt_row <- data.frame(
    Methode = "Gesamtfläche (alle Polygone)",
    Anzahl_Polygone = nrow(polygons),
    Fläche_km2 = round(as.numeric(total_area) / 1e6, 1),
    Anteil_Prozent = "100 %"
  )

  # Alles zusammenfügen und Spalten umbenennen
  bind_rows(area_summary, unverbunden_row, gesamt_row) %>%
    setNames(c("Methode", "Anzahl", "Fläche (km²)", "Anteil (%)"))
})

  observe({
    selected <- connection_matrix %>%
      filter(method %in% input$methods, connected == TRUE)

    point_ids <- unique(selected$origin_index)
    polygon_ids <- unique(selected$polygon_index)

    points_sel <- points[point_ids, ]
    points_sel$origin_index <- point_ids

    polygons_sel <- polygons[polygon_ids, ]

    leafletProxy("map") %>%
      clearGroup("Punkte") %>%
      clearGroup("Polygone") %>%
      clearGroup("Strassenpfade")

    polygons_all <- polygons
    polygons_all$polygon_index <- seq_len(nrow(polygons_all))
    polygons_all$connected <- polygons_all$polygon_index %in% polygon_ids

    leafletProxy("map") %>%
      addPolygons(
        data = polygons_all,
        color = ~ifelse(connected, "#2a9d8f", "#999999"),
        fillOpacity = ~ifelse(connected, 0.6, 0.1),
        weight = 1.5,
        label = ~paste("Polygon", polygon_index),
        group = "Polygone"
      ) %>%
      addCircleMarkers(data = points_sel, color = "red", radius = 5,
                       label = ~paste("Punkt", origin_index), group = "Punkte",
                       layerId = ~origin_index)

    if ("street_path" %in% input$methods) {
      path_lines <- lapply(paths_info, function(p) {
        if (!(p$origin_index %in% point_ids && p$polygon_index %in% polygon_ids))
          return(NULL)
        geom <- get_path_line(p$path, p$verts_sf)
        if (is.null(geom)) return(NULL)
        st_sf(
          origin_index = p$origin_index,
          polygon_index = p$polygon_index,
          geometry = st_sfc(geom, crs = 4326)
        )
      })
      path_lines <- do.call(rbind, path_lines)
      if (!is.null(path_lines) && nrow(path_lines) > 0) {
        leafletProxy("map") %>%
          addPolylines(data = path_lines, color = "blue", weight = 3,
                       label = ~paste("Pfad von Punkt",
                                      origin_index,
                                      "zu Polygon",
                                      polygon_index),
                       group = "Strassenpfade")
      }
    }
  })

  observeEvent(input$map_marker_click, {
    leafletProxy("map") %>% clearGroup("highlighted")
    click <- input$map_marker_click
    if (is.null(click$id)) return()

    clicked_point <- st_nearest_feature(
      st_sfc(st_point(c(click$lng, click$lat)), crs = 4326),
      points
    )

    matched <- connection_matrix %>%
      filter(origin_index == clicked_point, connected == TRUE)

    if (nrow(matched) == 0) return()

    polygon_ids <- matched$polygon_index
    point_id <- as.integer(click$id)

    leafletProxy("map") %>%
      addPolygons(
        data = polygons[polygons$polygon_index %in% polygon_ids, ],
        color = "red",
        weight = 3,
        fillOpacity = 0.5,
        label = ~paste("Polygon", polygon_index),
        group = "highlighted"
      ) %>%
      addCircleMarkers(
        data = points[point_id, ],
        color = "red",
        radius = 6,
        weight = 3,
        label = ~facility_name,
        group = "highlighted"
      )

    matched_paths <- lapply(paths_info, function(p) {
      if (p$origin_index == point_id && p$polygon_index %in% polygon_ids) {
        geom <- get_path_line(p$path, p$verts_sf)
        if (!is.null(geom)) {
          st_sf(
            origin_index = p$origin_index,
            polygon_index = p$polygon_index,
            geometry = st_sfc(geom, crs = 4326)
          )
        } else NULL
      } else NULL
    })
    matched_paths <- do.call(rbind, Filter(Negate(is.null), matched_paths))

    if (!is.null(matched_paths) && nrow(matched_paths) > 0) {
      leafletProxy("map") %>%
        addPolylines(
          data = matched_paths,
          color = "red",
          weight = 4,
          label = ~paste("Pfad von Punkt", origin_index, "zu Polygon", polygon_index),
          group = "highlighted"
        )
    }

    pt_data <- points[clicked_point, ]
    pt_name <- pt_data$facility_name
    pt_type <- pt_data$facility_type
    pt_commodity <- pt_data$primary_commodity

    method_table <- matched %>%
      mutate(
        Pip = ifelse(method == "point_in_polygon", "JA", ""),
        Buffer = ifelse(method == "buffered_polygon", "JA", ""),
        Strasse = ifelse(method == "street_path" & !is.na(path_length_km), paste0(round(path_length_km, 1), " km"), "")
      ) %>%
      group_by(polygon_index) %>%
      summarise(
        Pip = ifelse(any(Pip == "JA"), "JA", ""),
        Buffer = ifelse(any(Buffer == "JA"), "JA", ""),
        Strasse = paste(unique(Strasse[Strasse != ""]), collapse = ", "),
        .groups = "drop"
      ) %>%
      arrange(polygon_index)

    msa_data <- polygons %>%
      filter(polygon_index %in% polygon_ids) %>%
      st_drop_geometry()

    if ("msa_mean" %in% names(msa_data)) {
      msa_durchschnitt <- mean(msa_data$msa_mean, na.rm = TRUE)
    } else {
      msa_durchschnitt <- NA
    }

    text_header <- paste0(
      "Name: ", pt_name, "\n",
      "Typ: ", pt_type, "\n",
      "Rohstoff: ", pt_commodity, "\n",
      "Durchschnittlicher MSA-Wert: ", ifelse(is.na(msa_durchschnitt), "Nicht verfügbar", round(msa_durchschnitt, 2)), "\n",
      "Anzahl Verbindungen: ", length(unique(matched$polygon_index)), "\n\n"
    )

    connection_info <- paste0(
      sprintf("%-10s %-5s %-7s %-10s\n", "Polygon", "PiP", "Buffer", "Strasse"),
      paste(
        sprintf("%-10s %-5s %-7s %-10s",
                method_table$polygon_index,
                method_table$Pip,
                method_table$Buffer,
                method_table$Strasse),
        collapse = "\n"
      )
    )

    output$point_info <- renderText({ paste0(text_header, connection_info) })
  })

  output$portfolio_n_mines <- renderText({
    pts <- points
    pts <- pts[pts$country %in% input$portfolio_region, ]
    pts <- pts[pts$primary_commodity %in% input$portfolio_commodity, ]
    paste("Anzahl Minen:", nrow(pts))
  })

  output$portfolio_total_area <- renderText({
    pts_ids <- which(
      (points$country %in% input$portfolio_region) &
      (points$primary_commodity %in% input$portfolio_commodity)
    )

    connected_ids <- connection_matrix %>%
      filter(origin_index %in% pts_ids, connected == TRUE) %>%
      distinct(polygon_index) %>%
      pull(polygon_index)

    # Filtere verknüpfte Polygone und berechne deren Fläche
    area_sum <- polygons %>%
      filter(polygon_index %in% connected_ids) %>%
      st_area() %>%
      sum(na.rm = TRUE)

    paste("Totale Polygonfläche:", round(as.numeric(area_sum) / 1e6, 1), "km²")
  })

  # output$portfolio_msa_avg <- renderText({
  #   pts_ids <- which(
  #     (points$country %in% input$portfolio_region) &
  #     (points$primary_commodity %in% input$portfolio_commodity)
  #   )

  #   connected_ids <- connection_matrix %>%
  #     filter(origin_index %in% pts_ids, connected == TRUE) %>%
  #     distinct(polygon_index) %>%
  #     pull(polygon_index)

  #   msa_avg <- polygons %>%
  #     filter(polygon_index %in% connected_ids) %>%
  #     summarise(avg = mean(msa_mean, na.rm = TRUE)) %>%
  #     pull(avg)

  #   # paste("Durchschnittlicher MSA-Wert:", round(msa_avg, 2))
  # })

  # output$portfolio_critical <- renderText({
  #   pts_ids <- which(
  #     (points$country %in% input$portfolio_region) &
  #     (points$primary_commodity %in% input$portfolio_commodity)
  #   )

  #   connected_ids <- connection_matrix %>%
  #     filter(origin_index %in% pts_ids, connected == TRUE) %>%
  #     distinct(polygon_index) %>%
  #     pull(polygon_index)

  #   n_crit <- polygons %>%
  #     filter(polygon_index %in% connected_ids, msa_mean < 0.3) %>%
  #     nrow()

  #   # paste("Minen unter MSA-Grenzwert (0.3):", n_crit)
  # })

  observe({
    if (input$portfolio_index == "MSA") {
      shinyjs::disable("portfolio_year")
    } else {
      shinyjs::enable("portfolio_year")
    }
  })
}


shinyApp(ui, server)

