# California Carbon Explorer
# Written by Lily Klinek, lpklinek@ucdavis.edu
#
# requires the following files:
# processed_data.Rds (processed using scripts in "R" folder)
# processed rasters tifs
# uncertainty tifs
# var_groups_and_palettes.R,
# info_pages.R
# styles.css
# custom.js

library(shiny)
library(leaflet)
library(leaflet.extras)
library(terra)
library(sf)
library(bslib)
library(shinyWidgets)
library(shinyjs)
library(shinyjqui)
library(plotly)
library(DT)
library(scico)
library(gifski)
library(basemaps)
library(base64enc)
library(later)

# helper function
`%||%` <- function(a, b) if (!is.null(a)) a else b

# ---- Data ---------------------------------------------------------------
data_objects   <- readRDS("data/processed_data.Rds")
var_labels     <- data_objects$var_labels
display_labels <- data_objects$display_labels
var_names      <- data_objects$var_names
var_range_list <- data_objects$var_range_list
unc_levels     <- data_objects$unc_levels
unc_range_list <- data_objects$unc_range_list

has_unc <- length(unc_levels) > 0 && dir.exists("data/unc")
unc_choices <- if (has_unc) {
  setNames(names(unc_levels), vapply(unc_levels, function(x) x$label, character(1)))
}

# median rasters
rast_list <- setNames(
  lapply(var_names, function(v) terra::rast(file.path("data/processed_rasters", paste0(v, ".tif")))),
  var_names
)

source("R/var_groups_and_palettes.R")
source("R/get_palette_for_var.R")
source("R/info_pages.R")  

time_seq    <- seq(as.Date("2014-01-01"), as.Date("2022-12-31"), by = "month")
time_labels <- format(time_seq, "%Y-%m")

addResourcePath("images", "www")

# time between month slider steps (milliseconds)
anim_interval_ms <- 3000

# basemap
# note: CARTO_KEY is set as an environment variable in Connect Cloud 
carto_key <- Sys.getenv("CARTO_KEY", "")
add_maptypes(
  map_service     = "carto_voyager_custom",
  map_type        = "voyager",
  url_endpoint    = "https://basemaps.cartocdn.com/rastertiles/voyager/",
  url_xy          = "xy",
  url_file_format = ".png",
  url_map_token   = "?key="
)
carto_tile_url <- paste0(
  "https://basemaps.cartocdn.com/rastertiles/light_all/{z}/{x}/{y}.png",
  if (nzchar(carto_key)) paste0("?key=", carto_key) else ""
)

# ---- Helper functions  -------------------------
.bound_cache <- new.env()
get_bound <- function(level, vr, side) {
  key <- paste(level, vr, side, sep = "|")
  if (!is.null(.bound_cache[[key]])) return(.bound_cache[[key]])
  p <- file.path("data/unc", level, side, paste0(vr, ".tif"))
  if (!file.exists(p)) return(NULL)
  r <- terra::rast(p)
  .bound_cache[[key]] <- r
  r
}

is_point_geom <- function(g) any(sf::st_geometry_type(g) %in% c("POINT", "MULTIPOINT"))


extract_ts <- function(geom, r) {
  v <- terra::vect(sf::st_sf(geometry = sf::st_geometry(geom)))
  vals <- if (is_point_geom(geom)) {
    terra::extract(r, v)
  } else {
    terra::extract(r, v, fun = mean, na.rm = TRUE, exact = TRUE)  # coverage-weighted mean
  }
  vals <- vals[, names(vals) != "ID", drop = FALSE]
  ts <- as.numeric(vals[1, ])
  if (length(ts) != length(time_seq)) {
    message("extract_ts: ", length(ts), " layers vs ", length(time_seq), " time steps")
    length(ts) <- length(time_seq)
  }
  ts
}

build_ts_df <- function(geom, vr, level = NULL) {
  df <- data.frame(Date = time_seq, Median = extract_ts(geom, rast_list[[vr]]),
                   Lower = NA_real_, Upper = NA_real_)
  if (!is.null(level) && has_unc) {
    lo <- get_bound(level, vr, "lo"); hi <- get_bound(level, vr, "hi")
    if (!is.null(lo) && !is.null(hi)) {
      df$Lower <- extract_ts(geom, lo)
      df$Upper <- extract_ts(geom, hi)
    }
  }
  df
}

geojson_polygon_to_sf <- function(feat) {
  if (!identical(feat$geometry$type, "Polygon")) return(NULL)
  ring <- feat$geometry$coordinates[[1]]
  m <- do.call(rbind, lapply(ring, function(p) as.numeric(unlist(p))[1:2]))
  if (!identical(m[1, ], m[nrow(m), ])) m <- rbind(m, m[1, ])
  sf::st_sf(geometry = sf::st_sfc(sf::st_polygon(list(m)), crs = 4326))
}

# creating palette for uncertainty width layer
width_palette <- function(rng) {
  if (rng[2] <= rng[1]) rng[2] <- rng[1] + 1
  leaflet::colorNumeric(scico::scico(100, palette = "lajolla"), domain = rng, na.color = "transparent")
}

gif_palette_colors <- function(vr) {
  group <- var_groups[[vr]]
  pal_name <- group_palettes[[group]]
  viridis_opts <- c("viridis", "magma", "plasma", "inferno", "cividis", "turbo", "mako", "rocket")
  if (pal_name %in% viridis_opts) {
    viridisLite::viridis(100, option = pal_name)
  } else if (group %in% c("Foliage")) {
    scico::scico(100, direction = -1, palette = pal_name)
  } else {
    scico::scico(100, palette = pal_name)
  }
}

generate_gif <- function(vr, rast_subset, filename, fps = 5, width = 950, height = 750) {
  
  frame_dir <- tempfile("gif_frames_"); dir.create(frame_dir)
  on.exit(unlink(frame_dir, recursive = TRUE), add = TRUE)
  
  n <- terra::nlyr(rast_subset)
  png_paths <- file.path(frame_dir, sprintf("frame_%03d.png", seq_len(n)))
  width <- as.integer(width); height <- as.integer(height)
  
  pal_cols <- gif_palette_colors(vr)
  domain   <- var_range_list[[vr]]        # precomputed with map scale
  
  rast_p <- terra::project(rast_subset, "EPSG:3857")
  projected_ext <- sf::st_bbox(rast_p, crs = sf::st_crs(3857))
  r <- 80000
  bbox_expanded <- projected_ext + c(-r, -r, -60000, r)
  
  basemap <- basemap_terra(ext = sf::st_as_sfc(bbox_expanded), map_service = "carto_voyager_custom",
                           map_type = "voyager", map_token = carto_key)
  
  # progress message for gif rendering
  withProgress(message = "Rendering frames", value = 0, {
    for (i in seq_len(n)) {
      png(png_paths[i], width = width, height = height)
      tryCatch({
        plot(basemap, main = paste("Date:", names(rast_subset)[i]),
             box = FALSE, mar = c(3, 3, 6, 2), axes = FALSE)
        plot(rast_p[[i]], col = pal_cols, range = domain, type = "continuous",
             plg = list(title = display_labels[[vr]]), axes = FALSE, add = TRUE)
      }, finally = dev.off())
      incProgress(1 / (n + 1))
    }
    gifski::gifski(png_paths, gif_file = filename, width = width, height = height, delay = 1 / fps)
  })
  filename
}

# ---- UI -----------------------------------------------------------------
ui <- navbarPage(
  title = div("California Carbon Explorer",
              style = "font-size: 22px; padding-left: 10px; padding-top: 12px; padding-bottom: 0px; color: #2f3714"),
  theme = bslib::bs_theme(heading_font = bslib::font_google("Tenor Sans")),
  id = "main_navbar", position = "fixed-top", inverse = TRUE, collapsible = TRUE,
  header = tagList(
    shinyjs::useShinyjs(),   
    tags$head(
      tags$link(rel = "stylesheet", href = "styles.css"),
      tags$style(HTML("
        .unc-hidden { display: none !important; }
        .selectize-dropdown { z-index: 10000 !important; }
        /* compact shapefile upload + Add ROI button */
        #roi_section .form-group { margin-bottom: 4px; }
        #roi_section label.control-label { margin-bottom: 2px; font-size: 12px; }
        #roi_section .form-control,
        #roi_section .input-group > .btn,
        #roi_section .input-group .btn-file,
        #roi_section .input-group-btn > .btn,
        #roi_section .input-group-prepend > .btn,
        #roi_section > .btn {
          height: 24px; min-height: 0; padding: 0 10px; font-size: 12px; line-height: 22px;
        }
        #roi_section .progress { height: 10px; margin: 2px 0 0 0; }
        #roi_section .progress-bar { font-size: 9px; line-height: 10px; }
      ")),
      tags$script(src = "custom.js"),
      tags$link(rel = "stylesheet", href = "https://cdnjs.cloudflare.com/ajax/libs/leaflet.draw/1.0.4/leaflet.draw.css"),
      tags$script(src = "https://cdnjs.cloudflare.com/ajax/libs/leaflet.draw/1.0.4/leaflet.draw.js"),
      tags$script(HTML("
        Shiny.addCustomMessageHandler('showSpinner', function(show) {
          var s = document.getElementById('map-spinner');
          if (s) s.style.display = show ? 'block' : 'none';
        });
      "))
    )
  ),
  
  ## Map View ----
  tabPanel(
    title = "Map View",
    tags$div(
      id = "map-container", style = "position: relative; height: 100vh; width: 100vw;",
      leafletOutput("map", height = "100%", width = "100%"),
      tags$div(id = "map-spinner",
               style = "position: absolute; top: 50%; left: 50%; transform: translate(-50%, -50%); z-index: 9999; display: none;",
               tags$div(class = "lds-dual-ring-initial"))
    ),
    
    ### left column: controls + time slider ----
    absolutePanel(
      top = 60, left = 15, width = 355,
      style = "background: transparent; border: none; box-shadow: none; padding: 0;
               max-height: calc(100vh - 290px); overflow-y: auto; overflow-x: hidden;",
      tags$div(
        class = "panel-well",
        h4("Controls", style = "text-align: center"),
        selectInput("variable", "Select Variable:", choices = names(var_labels), selected = names(var_labels)[1]),
        if (has_unc) radioButtons("map_layer", "Map layer:",
                                  choices = c("Median" = "median", "Uncertainty width" = "width"),
                                  selected = "median", inline = TRUE),
        radioButtons("selection_mode", "Selection Mode:",
                     choices = c("Point" = "point", "Drawn Area" = "drawn_area", "Uploaded Area" = "uploaded_area"),
                     selected = "point", inline = TRUE),
        tags$p("Click a point or draw a box on the map to view time series."),
        div(id = "roi_section",
            fileInput("roi_upload", "Upload Shapefile (.zip)", accept = ".zip"),
            actionButton("add_roi", "Add ROI to Map", class = "btn-primary"))
      ),
      tags$div(
        class = "panel-well", style = "margin-top: 10px; padding-left: 20px;",
        shinyWidgets::sliderTextInput(
          inputId = "time", label = "Select Month:", choices = time_labels, selected = time_labels[1],
          grid = FALSE, animate = animationOptions(interval = anim_interval_ms, loop = TRUE), width = "100%")
      )
    ),
    
    ### time series window ----
    jqui_resizable(
      absolutePanel(
        top = 60, right = 15, class = "panel-well",
        style = "min-width: 300px; min-height: 280px; width: 420px; height: 400px; position: absolute;",
        tags$div(
          style = "display: flex; flex-direction: column; height: 100%;",
          h4("Time Series of Selected Pixel / Area", style = "text-align: center; margin: 0 0 4px 0; flex: none;"),
          tags$div(style = "flex: 1 1 auto; min-height: 0;",
                   plotlyOutput("ts_plot", height = "100%", width = "100%")),
          if (has_unc) tags$div(
            id = "unc_controls", class = "unc-hidden",
            style = "flex: none; display: flex; gap: 12px; align-items: center; font-size: 12px; margin-top: 4px; margin-bottom: -12px;",
            tags$div(style = "flex: none; white-space: nowrap;",
                     checkboxInput("show_unc", "Show uncertainty", value = FALSE, width = "auto")),
            tags$div(style = "flex: 1 1 auto; min-width: 0;",
                     selectizeInput("unc_level", NULL, choices = unc_choices, width = "100%",
                                    options = list(dropdownParent = "body")))
          ),
          uiOutput("ts_note")
        )
      ),
      options = list(handles = "sw")
    ),
    
    ### legend ----
    absolutePanel(bottom = 10, left = 15, width = 290, class = "panel-well panel-legend",
                  uiOutput("custom_legend"))
  ),
  
  ## Export Data ----
  tabPanel(
    "Export Data",
    div(style = "padding: 70px 30px 30px 30px; background-color: #f8f9fa; min-height: 100vh;",
        div(class = "panel panel-default", style = "padding: 20px; background-color: white; border-radius: 6px;",
            sidebarLayout(
              sidebarPanel(
                width = 3,
                h3("Export Data"),
                tags$p(style = "font-size: 13px; color: #666;",
                       "Exports a CSV using the current selection from the Map View tab."),
                tags$hr(),
                selectInput("csv_variable", "Variable", choices = var_labels),
                if (has_unc) selectInput("csv_level", "Uncertainty bounds",
                                         choices = c(unc_choices, "None (median only)" = "none")),
                actionButton("generate_csv_preview", "Generate Preview", class = "btn-primary"),
                br(), br(),
                uiOutput("csv_download_ui")
              ),
              mainPanel(
                width = 9,
                h4("Preview"),
                DT::dataTableOutput("csv_preview"),
                if (has_unc) tags$p(style = "font-size: 12px; color: #666; margin-top: 10px;",
                                    "Columns are the median and the lower/upper percentile bounds. For point selections these are the ",
                                    "pixel's posterior percentiles. For drawn or uploaded areas they are the area mean of the pixel-level ",
                                    "percentiles.")
              )
            )))
  ),
  
  ## Generate GIF ----
  tabPanel(
    "Generate GIF",
    div(style = "padding: 70px 30px 30px 30px; background-color: #f8f9fa; min-height: 100vh;",
        div(class = "panel panel-default", style = "padding: 20px; background-color: white; border-radius: 6px;",
            sidebarLayout(
              sidebarPanel(
                width = 3,
                h3("Generate GIF"), tags$hr(),
                selectInput("gif_variable", "Variable:", choices = var_labels),
                dateRangeInput("gif_timerange", "Date Range:", start = "2014-01-01", end = "2022-12-01"),
                sliderInput("gif_fps", "Frames per Second:", min = 1, max = 10, value = 3),
                actionButton("generate_gif", "Generate GIF Preview")
              ),
              mainPanel(
                width = 9,
                div(id = "gif_panel", style = "margin-top: 10px; text-align: center; padding: 0; width: 100%;",
                    uiOutput("gif_preview_block"))
              )
            )))
  ),
  
  info_menu
)

# ---- Server -------------------------------------------------------------
server <- function(input, output, session) {
  
  selected_geom <- reactiveVal(NULL)   
  roi_data      <- reactiveVal(NULL)   
  
  varname <- reactive(var_labels[[input$variable]])
  cur_level <- reactive(input$unc_level %||% names(unc_levels)[1])
  
  clear_drawings <- function() session$sendCustomMessage("clearDrawnItems", list())
  
  session$onFlushed(function() {
    session$sendCustomMessage("clearDrawnItems", list())
    session$sendCustomMessage("enableResizable", TRUE)
  }, once = TRUE)
  
  # ---- Map ----
  output$map <- renderLeaflet({
    leaflet(options = leafletOptions(zoomControl = FALSE)) %>%
      addTiles(urlTemplate = carto_tile_url,
               attribution = paste0('&copy; <a href="https://www.openstreetmap.org/copyright">OpenStreetMap</a> contributors, ',
                                    '&copy; <a href="https://carto.com/attributions">CARTO</a>')) %>%
      fitBounds(lng1 = -128.4, lat1 = 35.5, lng2 = -115.1, lat2 = 41.5) %>%
      htmlwidgets::onRender("
        function(el, x) {
          var map = this;
          map._drawnItems = new L.FeatureGroup();
          map.addLayer(map._drawnItems);
          L.control.zoom({position: 'bottomright'}).addTo(map);
        }") %>%
      addScaleBar(position = "bottomright",
                  options = scaleBarOptions(maxWidth = 100, imperial = TRUE, updateWhenIdle = TRUE))
  })
  
  # raster layer: either median, or uncertainty width
  observe({
    vr    <- varname()
    layer <- input$map_layer %||% "median"
    tlab  <- input$time
    lvl   <- if (has_unc && layer == "width") cur_level() else NULL
    
    # loading spinner
    session$sendCustomMessage("showSpinner", TRUE)
    on.exit(later::later(function() session$sendCustomMessage("showSpinner", FALSE), 0.3), add = TRUE)
    
    r_stack <- rast_list[[vr]]
    idx <- match(tlab, names(r_stack))
    if (is.na(idx) && terra::nlyr(r_stack) == length(time_seq)) idx <- match(tlab, time_labels)
    
    proxy <- leafletProxy("map") %>% removeImage("raster")
    if (is.na(idx)) return()
    
    if (!is.null(lvl)) {
      lo <- get_bound(lvl, vr, "lo"); hi <- get_bound(lvl, vr, "hi")
      if (is.null(lo) || is.null(hi)) return()
      rng <- unc_range_list[[lvl]][[vr]]
      r   <- terra::clamp(hi[[idx]] - lo[[idx]], lower = 0, upper = rng[2], values = TRUE)
      pal <- width_palette(rng)
    } else {
      r   <- r_stack[[idx]]
      pal <- get_palette_for_var(vr, rast_list, var_groups, group_palettes, slice_rast = r)
    }
    
    proxy %>% addRasterImage(r, colors = pal, opacity = 0.8, group = "raster", layerId = "raster")
  })
  
  # ---- Legend ----
  output$custom_legend <- renderUI({
    vr    <- varname()
    layer <- input$map_layer %||% "median"
    if (has_unc && layer == "width") {
      lvl <- cur_level()
      var_range <- unc_range_list[[lvl]][[vr]]
      if (var_range[2] <= var_range[1]) var_range[2] <- var_range[1] + 1
      pal <- width_palette(var_range)
      legend_title <- paste0(unc_levels[[lvl]]$short, " width: ", display_labels[[vr]] %||% input$variable)
    } else {
      var_range <- var_range_list[[vr]]
      pal <- get_palette_for_var(vr, rast_list, var_groups, group_palettes)
      legend_title <- display_labels[[vr]] %||% input$variable
    }
    vals <- seq(var_range[1], var_range[2], length.out = 100)
    gradient_css <- paste0("linear-gradient(to top, ", paste0(pal(vals), collapse = ", "), ");")
    digits_to_use <- if (diff(var_range) < 10) 2 else 0
    label_texts <- formatC(pretty(var_range, n = 6), format = "f", digits = digits_to_use, big.mark = ",")
    
    tagList(
      tags$div(class = "legend-title", legend_title),
      tags$div(style = "display: flex; align-items: center;",
               tags$div(style = paste0("height: 180px; width: 35px; background: ", gradient_css,
                                       " border: 1px solid #000; margin-right: 0px; position: relative;")),
               tags$div(style = "height: 180px; display: flex; flex-direction: column; justify-content: space-between;",
                        lapply(rev(label_texts), function(txt) {
                          tags$div(style = "display: flex; align-items: center;",
                                   tags$div(style = "width:5px; height:2px; background:black; margin-right:2px;"),
                                   tags$div(class = "legend-label", txt))
                        })))
    )
  })
  
  # ---- Selection mode (point, area, or uploaded roi) ----
  observeEvent(input$selection_mode, {
    selected_geom(NULL)
    proxy <- leafletProxy("map") %>%
      clearGroup("clicked_point") %>% clearGroup("selected_area") %>%
      clearGroup("roi") %>% clearGroup("_drawnItems") %>%
      removeDrawToolbar(clearFeatures = TRUE) 
    clear_drawings()
    
    # draw toolbar
    if (input$selection_mode == "drawn_area") {
      proxy %>% addDrawToolbar(
        targetGroup = "_drawnItems", position = "bottomleft",
        rectangleOptions = drawRectangleOptions(
          shapeOptions = drawShapeOptions(color = "#788554", weight = 2, fillOpacity = 0.2), showArea = TRUE),
        polygonOptions = drawPolygonOptions(
          shapeOptions = drawShapeOptions(color = "#788554", weight = 2, fillOpacity = 0.2), showArea = TRUE),
        polylineOptions = FALSE, markerOptions = FALSE, circleOptions = FALSE, circleMarkerOptions = FALSE,
        editOptions = editToolbarOptions(edit = TRUE, remove = TRUE))
    }
    if (input$selection_mode == "uploaded_area" && !is.null(roi_data())) activate_roi()
  }, ignoreInit = TRUE)
  
  observe({
    on <- input$selection_mode == "uploaded_area"
    shinyjs::toggleState("roi_upload", condition = on)
    shinyjs::toggleState("add_roi", condition = on)
    shinyjs::toggleClass("roi_section", "disabled-section", condition = !on)
  })
  
  # ---- Point ----
  observeEvent(input$map_click, {
    req(input$selection_mode == "point")
    click <- input$map_click
    selected_geom(sf::st_sf(geometry = sf::st_sfc(sf::st_point(c(click$lng, click$lat)), crs = 4326)))
    leafletProxy("map") %>% clearGroup("clicked_point") %>%
      addCircleMarkers(click$lng, click$lat, radius = 4, color = "#244C20", fillOpacity = 1, group = "clicked_point")
  })
  
  # ---- Drawn area ----
  observeEvent(input$map_draw_new_feature, {
    req(input$selection_mode == "drawn_area")
    g <- geojson_polygon_to_sf(input$map_draw_new_feature)
    if (!is.null(g)) selected_geom(g)
  }, ignoreInit = TRUE)
  
  observeEvent(input$map_draw_edited_features, {
    req(input$selection_mode == "drawn_area")
    feats <- input$map_draw_edited_features$features
    if (length(feats)) {
      g <- geojson_polygon_to_sf(feats[[1]])
      if (!is.null(g)) selected_geom(g)
    }
  }, ignoreInit = TRUE)
  
  observeEvent(input$map_draw_deleted_features, {
    if (input$selection_mode == "drawn_area") selected_geom(NULL)
  }, ignoreInit = TRUE)
  
  # ---- Uploaded area ----
  observeEvent(input$roi_upload, {
    exdir <- tempfile("roi_"); dir.create(exdir)
    unzip(input$roi_upload$datapath, exdir = exdir)
    shp <- list.files(exdir, pattern = "\\.shp$", full.names = TRUE, recursive = TRUE)
    # potential error messages for uploaded zip
    if (!length(shp)) {
      showNotification("No .shp file found in the zip.", type = "error"); return()
    }
    roi <- tryCatch({
      x <- sf::st_read(shp[1], quiet = TRUE)
      if (is.na(sf::st_crs(x))) stop("The shapefile has no coordinate system (.prj file missing).")
      x <- sf::st_transform(sf::st_make_valid(sf::st_zm(x)), 4326)
      x <- x[sf::st_geometry_type(x) %in% c("POLYGON", "MULTIPOLYGON"), ]
      if (!nrow(x)) stop("No polygon features found in the shapefile.")
      sf::st_sf(geometry = sf::st_union(sf::st_geometry(x)))   # dissolve to a single area
    }, error = function(e) { showNotification(conditionMessage(e), type = "error"); NULL })
    roi_data(roi)
  })
  
  activate_roi <- function() {
    roi <- roi_data(); req(roi)
    bb <- sf::st_bbox(roi)
    leafletProxy("map") %>% clearGroup("roi") %>%
      addPolygons(data = roi, group = "roi", color = "red", weight = 2, fillOpacity = 0.2) %>%
      fitBounds(bb[["xmin"]], bb[["ymin"]], bb[["xmax"]], bb[["ymax"]])
    selected_geom(roi)
  }
  observeEvent(input$add_roi, {
    req(input$selection_mode == "uploaded_area")
    activate_roi()
  })
  
  # ---- Time series  ----
  ts_df <- reactive({
    g <- selected_geom(); req(g)
    vr  <- varname()
    lvl <- if (has_unc && isTRUE(input$show_unc)) cur_level() else NULL
    tryCatch(build_ts_df(g, vr, lvl), error = function(e) {
      showNotification(paste("Could not extract time series:", conditionMessage(e)), type = "error")
      NULL
    })
  })
  
  output$ts_plot <- renderPlotly({
    df <- ts_df(); req(df)
    validate(need(any(!is.na(df$Median)), "No model data at this location.")) # if a location with no data is clicked
    vr <- varname()
    band <- !all(is.na(df$Lower))
    fmt <- function(x) formatC(x, digits = 3, format = "g")
    short <- if (band) unc_levels[[cur_level()]]$short else ""
    hover <- if (band) {
      sprintf("%s<br>Median: %s<br>%s: %s to %s", format(df$Date, "%Y-%m"),
              fmt(df$Median), short, fmt(df$Lower), fmt(df$Upper))
    } else {
      sprintf("%s<br>Median: %s", format(df$Date, "%Y-%m"), fmt(df$Median))
    }
    
    p <- plot_ly(df, x = ~Date)
    if (band) {
      p <- add_ribbons(p, ymin = ~Lower, ymax = ~Upper, name = short,
                       fillcolor = "rgba(120,133,84,0.25)", line = list(width = 0), hoverinfo = "skip")
    }
    p %>%
      add_lines(y = ~Median, name = "Median", line = list(color = "#788554", width = 2),
                text = hover, hoverinfo = "text") %>%
      add_markers(y = ~Median, marker = list(color = "#244C20", size = 4), showlegend = FALSE,
                  text = hover, hoverinfo = "text") %>%
      layout(xaxis = list(title = ""),
             yaxis = list(title = list(text = display_labels[[vr]] %||% vr, font = list(size = 11))),
             margin = list(l = 60, r = 10, t = 10, b = 40),
             legend = list(orientation = "h", y = -0.2)) %>%
      config(displaylogo = FALSE)
  })
  
  output$ts_note <- renderUI({
    g <- selected_geom(); req(g)
    if (has_unc && isTRUE(input$show_unc) && !is_point_geom(g)) {
      tags$div(style = "flex: none; font-size: 11px; color: #666; line-height: 1.2;",
               "For area ROIs, the plot ribbon is the mean of pixel-level bounds.")
    }
  })
  
  observeEvent(ts_df(), session$sendCustomMessage("enableResizable", TRUE), ignoreNULL = FALSE)
  
  # show uncertainty controls only after a point/area is selected
  if (has_unc) {
    observe({
      show_ctrl <- !is.null(selected_geom()) || identical(input$map_layer, "width")
      shinyjs::toggleClass("unc_controls", "unc-hidden", condition = !show_ctrl)
    })
  }
  
  # ---- CSV export ----
  csv_preview_data <- eventReactive(input$generate_csv_preview, {
    req(selected_geom(), input$csv_variable)
    vr  <- input$csv_variable
    lvl <- if (has_unc && !identical(input$csv_level, "none")) input$csv_level else NULL
    df  <- build_ts_df(selected_geom(), vr, lvl)
    
    out <- data.frame(Date = format(df$Date, "%Y-%m"), Variable = display_labels[[vr]] %||% vr,
                      Median = df$Median)
    if (!is.null(lvl) && !all(is.na(df$Lower))) {
      qn <- function(q) paste0("P", format(q * 100, trim = TRUE))
      out[[qn(unc_levels[[lvl]]$lo_q)]] <- df$Lower
      out[[qn(unc_levels[[lvl]]$hi_q)]] <- df$Upper
    }
    validate(need(any(!is.na(out$Median)), "No values extracted for the selected geometry."))
    out
  })
  
  output$csv_preview <- renderDT({
    req(csv_preview_data())
    datatable(head(csv_preview_data(), 12), options = list(pageLength = 12, searching = FALSE, dom = "t"))
  })
  
  output$csv_download_ui <- renderUI({
    req(csv_preview_data())
    downloadButton("download_csv", "Download CSV")
  })
  
  output$download_csv <- downloadHandler(
    filename = function() paste0("data_export_", Sys.Date(), ".csv"),
    content  = function(file) write.csv(csv_preview_data(), file, row.names = FALSE)
  )
  
  # ---- GIF ----
  gif_file <- reactiveVal(NULL)
  gif_generating <- reactiveVal(FALSE)
  
  output$gif_preview_block <- renderUI({
    if (gif_generating()) {
      tags$div(style = "color: #244C20; font-weight: bold;", "Generating GIF, please wait...")
    } else if (!is.null(gif_file())) {
      tags$div(
        tags$img(src = base64enc::dataURI(file = gif_file(), mime = "image/gif"),
                 style = "max-width: 100%; height: auto; border: none; display: block; margin: 0;"),
        tags$div(style = "margin-top: 2px;", downloadButton("download_gif", "Download GIF"))
      )
    } else {
      tags$p("GIF preview will generate below...")
    }
  })
  
  observeEvent(input$generate_gif, {
    req(input$gif_variable, input$gif_timerange)
    gif_generating(TRUE); gif_file(NULL)
    
    vr <- input$gif_variable
    r_stack <- rast_list[[vr]]
    rast_dates <- tryCatch(as.Date(paste0(names(r_stack), "-01")), error = function(e) NULL)
    if (is.null(rast_dates) || anyNA(rast_dates)) {
      showNotification("Error parsing raster dates.", type = "error")
      gif_generating(FALSE); return()
    }
    keep_idx <- which(rast_dates >= as.Date(input$gif_timerange[1]) & rast_dates <= as.Date(input$gif_timerange[2]))
    if (!length(keep_idx)) {
      showNotification("No layers found for the selected date range.", type = "error")
      gif_generating(FALSE); return()
    }
    
    tryCatch({
      path <- withCallingHandlers(
        generate_gif(vr, terra::subset(r_stack, keep_idx),
                     filename = tempfile("map_animation_", fileext = ".gif"),
                     fps = input$gif_fps),
        error = function(e) {   # if gif error -- prints the call stack to the R console / Connect log
          calls <- vapply(sys.calls(), function(x) paste(deparse(x, nlines = 1), collapse = ""), "")
          message("GIF error: ", conditionMessage(e), "\n", paste(calls, collapse = "\n"))
        })
      gif_file(path)
    }, error = function(e) {
      showNotification(paste("GIF generation failed:", conditionMessage(e)), type = "error")
      gif_file(NULL)
    }, finally = gif_generating(FALSE))
  })
  
  output$download_gif <- downloadHandler(
    filename = function() paste0("map_animation_", Sys.Date(), ".gif"),
    content  = function(file) { req(gif_file()); file.copy(gif_file(), file, overwrite = TRUE) }
  )
}

shinyApp(ui, server)