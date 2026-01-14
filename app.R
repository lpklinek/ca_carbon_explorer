#' 
#' 
#' #7/16 v2 (updated 7/18) -----
#' 
#' # UI -----
#' ui <- navbarPage(
#'   # style for navbar
#'   title = div("California Carbon Explorer", style = "font-weight: 600; font-size: 22px; padding-left: 10px; color: #FFFFFF"),
#'   id = "main_navbar",
#'   position = "fixed-top",
#'   inverse = TRUE, # makes the background dark and text light
#'   collapsible = TRUE, # collapses into menu icon when window is too small
#'   header = tagList(
#'     tags$head(
#'       tags$style(HTML("
#'         @font-face {
#'           font-family: 'DMSans';
#'           src: url('DMSans-Variable.ttf') format('truetype');
#'           font-weight: 100 900;
#'           font-style: normal;
#'         }
#' 
#'         html, body {
#'           height: 100%;
#'           margin: 0;
#'           padding: 0;
#'           overflow: hidden;
#'         }
#' 
#'         body, input, button, select, textarea {
#'           font-family: 'DMSans', sans-serif;
#'           font-weight: 300;
#'           font-size: 14px;
#'           color: #333;
#'         }
#'         
#'         h1, h2, h3, h4, h5, h6 {
#'           font-family: 'DMSans', sans-serif;
#'           font-weight: 600;
#'         }
#' 
#'         .panel-well {
#'           padding: 10px;
#'           border-radius: 8px;
#'           box-shadow: 0 2px 4px rgba(0,0,0,0.2);
#'           font-weight: 300;
#'           overflow: hidden;
#'           background-color: #FFFFFF;
#'         }
#'         
#'         .panel-legend {
#'           background-color: rgba(255, 255, 255, 0.0);
#'           box-shadow: none;
#'           border: none;
#'         }
#' 
#'         .legend-title {
#'           font-family: 'DMSans', sans-serif;
#'           font-weight: 510;
#'           font-size: 16px;
#'           margin-bottom: 4px;
#'           color: #333;
#'         }
#' 
#'         .legend-label {
#'           font-family: 'DMSans', sans-serif;
#'           font-weight: 300;
#'           font-size: 14px;
#'           color: #333;
#'         }
#'         
#'         .irs-single, .irs-bar, .irs-grid-text {
#'           font-size: 10px !important;
#'           font-family: 'DMSans', sans-serif;
#'         }
#' 
#'         .ui-resizable-handle.ui-resizable-sw {
#'           width: 20px;
#'           height: 20px;
#'           position: absolute;
#'           left: 0;
#'           bottom: 0;
#'           cursor: nesw-resize;
#'           background: url(\"data:image/svg+xml,%3Csvg xmlns='http://www.w3.org/2000/svg' width='20' height='20'%3E%3Cline x1='2' y1='14' x2='6' y2='18' stroke='rgba(0,0,0,0.3)' stroke-width='1'/%3E%3Cline x1='2' y1='8' x2='12' y2='18' stroke='rgba(0,0,0,0.3)' stroke-width='1'/%3E%3C/svg%3E\") no-repeat center center;
#'           background-size: 100% 100%;
#'         }
#' 
#'         .navbar, .container-fluid, .tab-content {
#'           margin: 0 !important;
#'           padding: 0 !important;
#'         }
#'         
#'         /* ========== NAVBAR OVERALL BACKGROUND ========== */
#'         .navbar {
#'           background-color: #1B0D33 !important;   /* Navbar background color */
#'           border-bottom: 3px solid #244C20;       /* Bottom border/accent line */
#'         }
#' 
#'         /* ========== NAVBAR BRAND TITLE & TABS (RESTING STATE) ========== */
#'         .navbar .navbar-brand,
#'         .navbar-default .navbar-nav > li > a {
#'           color: #ECECFC !important;              /* Color of title and tab text when idle */
#'           font-weight: 500;                       /* Slight boldness */
#'         }
#' 
#'         /* ========== NAVBAR TABS ON HOVER ========== */
#'         .navbar-default .navbar-nav > li > a:hover {
#'           color: #C29A6D !important;              /* Text color when hovered */
#'           background-color: #244C20 !important;   /* Background color when hovered */
#'         }
#' 
#'         /* ========== NAVBAR TABS WHEN ACTIVE (CURRENTLY SELECTED) ========== */
#'         /* This may not always be visible depending on whether you're using tabs inside navbarPage or tabsetPanel. */
#'         /* For tabs inside navbarPage, this is typically handled by Bootstrap nav-tabs styles instead. */
#'         .navbar-default .navbar-nav > .active > a,
#'         .navbar-default .navbar-nav > .active > a:focus,
#'         .navbar-default .navbar-nav > .active > a:hover {
#'           background-color: #ECECFC !important;   /* Background of selected tab */
#'           color: #244C20 !important;              /* Text color of selected tab */
#'           border-bottom: 2px solid #9A6E33 !important; /* Optional: bottom border accent for active tab */
#'         }
#' 
#'         #initial-loader {
#'           position: fixed;
#'           top: 0; left: 0;
#'           width: 100vw; height: 100vh;
#'           background-color: white;
#'           z-index: 99999;
#'           display: flex;
#'           align-items: center;
#'           justify-content: center;
#'         }
#' 
#'         .lds-dual-ring-initial:after {
#'           content: ' ';
#'           display: block;
#'           width: 64px;
#'           height: 64px;
#'           border-radius: 50%;
#'           border: 6px solid #244C20;
#'           border-color: #244C20 transparent #244C20 transparent;
#'           animation: lds-dual-ring 1.2s linear infinite;
#'         }
#' 
#'         @keyframes lds-dual-ring {
#'           0% { transform: rotate(0deg); }
#'           100% { transform: rotate(360deg); }
#'         }
#'         
#'         .leaflet-draw {
#'           top: 80px !important;
#'           z-index: 999 !important;
#'         }
#'       ")),
#'       tags$script(HTML("
#'         $(document).on('shiny:connected', function() {
#'           $('#initial-loader').fadeOut();
#'         });
#'       ")),
#'       tags$script(HTML("
#'   Shiny.addCustomMessageHandler('showSpinner', function(show) {
#'     var spinner = document.getElementById('map-spinner');
#'     if (spinner) {
#'       spinner.style.display = show ? 'block' : 'none';
#'     }
#'   });
#' "))
#'     )
#'   ),
#'   
#'   ## --- Map View Tab ----
#'   tabPanel(
#'     title = "Map View",
#'     tags$div(
#'       id = "map-container",
#'       style = "position: relative; height: 100vh; width: 100vw;",
#'       leafletOutput("map", height = "100%", width = "100%"),
#'       # adds spinner while map is loading
#'       tags$div(
#'         id = "map-spinner",
#'         style = "position: absolute; top: 50%; left: 50%; transform: translate(-50%, -50%);
#'                  z-index: 9999; display: none;",
#'         tags$div(class = "lds-dual-ring-initial")
#'       )
#'     ),
#'     
#'     ### Controls panel ----
#'     absolutePanel(
#'       top = 60, left = 20, width = 300,
#'       class = "panel-well",
#'       h4("Controls"),
#'       selectInput("variable", "Select Variable:",
#'                   choices = names(var_labels),
#'                   selected = names(var_labels)[1]),
#'       radioButtons("selection_mode", "Selection Mode:",
#'                    choices = c("Point" = "point", "Area" = "area"),
#'                    selected = "point",
#'                    inline = TRUE),
#'       tags$p("Click a point or draw a box on the map to view time series."),
#'       textOutput("date_text")
#'     ),
#'     
#'     ### Time slider -----
#'     absolutePanel(
#'       top = 330, left = 20, width = 300,
#'       class = "panel-well",
#'       shinyWidgets::sliderTextInput(
#'         inputId = "time",
#'         label = "Select Month:",
#'         choices = time_labels,
#'         selected = time_labels[1],
#'         grid = FALSE,
#'         animate = animationOptions(interval = 1500, loop = TRUE),
#'         width = "100%"
#'       )
#'     ),
#'     
#'     ### Time series panel ----
#'     # Time series panel
#'     jqui_resizable(
#'       absolutePanel(
#'         top = 60, right = 15,
#'         class = "panel-well",
#'         style = "min-width: 300px; min-height: 250px; width: 480px; height: 370px; position: absolute;",
#'         h4("Time Series of Selected Pixel / Area", style = "text-align: center"),
#'         tags$div(
#'           style = "height: calc(100% - 60px); width: 100%; padding-bottom: 0px;",
#'           plotlyOutput("ts_plot", height = "100%", width = "100%"),
#'           div(
#'             style = "text-align: center; padding: 2px 0 2px 0; margin: 0;",
#'             downloadButton("download_csv_timeseries", "Download CSV", class = "btn-primary btn-sm")
#'           )
#'         )
#'       ),
#'       options = list(handles = "sw")
#'     ),
#'     tags$style(HTML("
#'       #download_csv_timeseries {
#'       padding: 2px 2px !important;
#'       font-size: 11px !important;
#'     }
#'     ")),
#'     
#'     
#'     ### Legend panel ----
#'     absolutePanel(
#'       bottom = 20, left = 20, width = 280,
#'       class = "panel-well panel-legend",
#'       uiOutput("custom_legend")
#'     )
#'   ),
#'   
#'   ## Export tab ----
#'   tabPanel("Export GIF",
#'            fluidPage(
#'              titlePanel("Export GIF of Map Time Series"),
#'              sidebarLayout(
#'                sidebarPanel(
#'                  helpText("Select a variable and date range to generate an animated map GIF."),
#'                  selectInput("gif_variable", "Variable to export:", choices = var_labels),
#'                  dateRangeInput(
#'                    "gif_date_range",
#'                    "Date Range:",
#'                    start = min(time_seq),
#'                    end = max(time_seq),
#'                    min = min(time_seq),
#'                    max = max(time_seq)
#'                  ),
#'                  actionButton("generate_gif", "Generate GIF", class = "btn-primary")
#'                ),
#'                mainPanel(
#'                  h4("Preview (available after GIF generation)", style = "text-align: center"),
#'                  imageOutput("gif_preview"),
#'                  br(),
#'                  downloadButton("download_gif", "Download GIF", class = "btn-success")
#'                )
#'              )
#'            )
#'   ),
#'   
#'   ## --- Other Pages ----
#'   navbarMenu("About this tool", 
#'              tabPanel("Overview", h3("Overview"), p("Page under construction...")),
#'              tabPanel("The CARDAMOM Framework", h3("Framework Info"), p("Page under construction...")),
#'              tabPanel("Variable Descriptions", h3("Variable Info"), p("Page under construction...")),
#'              tabPanel("Model Information", h3("Model Info"), p("Page under construction...")),
#'              tabPanel("Instructions", h3("Instructions"), p("Page under construction..."))
#'   )
#'   
#'   
#'   # --- Initial Loading Spinner ---
#'   # ,
#'   # tags$div(id = "initial-loader", tags$div(class = "lds-dual-ring-initial"))
#' )
#' 
#' 
#' 
#' 
#' # SERVER ----------------------
#' server <- function(input, output, session) {
#'   observe({
#'     invalidateLater(500, session)  # wait 500ms
#'     session$sendCustomMessage("enableResizable", TRUE)
#'   })
#'   
#'   varname <- reactive({
#'     var_labels[[input$variable]]
#'   })
#'   
#'   ts_df <- reactiveVal()
#'   
#'   ## pre-caching raster slices -----
#'   r_slices <- reactiveValues()
#'   observe({
#'     for (vr_name in names(rast_list)) {
#'       r_stack <- rast_list[[vr_name]]
#'       r_slices[[vr_name]] <- lapply(1:nlyr(r_stack), function(i) terra::subset(r_stack, i))
#'     }
#'   })
#'   
#'   ## render map -----
#'   output$map <- renderLeaflet({
#'     leaflet() %>%
#'       addProviderTiles(providers$CartoDB.Voyager) %>%
#'       fitBounds(lng1 = -128.4, lat1 = 35.5, lng2 = -115.1, lat2 = 41.5) %>%
#'       htmlwidgets::onRender("
#'         function(el, x) {
#'           var map = this;
#'           map.zoomControl.setPosition('bottomright');
#'         }
#'       ") %>%
#'       addScaleBar(position = "bottomright",   # add scale bar
#'                   options = scaleBarOptions(maxWidth = 100, 
#'                                             imperial = TRUE,
#'                                             updateWhenIdle = TRUE))
#'   })
#'   
#'   ## selection mode ----
#'   observeEvent(input$selection_mode, {
#'     proxy <- leafletProxy("map")
#'     
#'     proxy %>%
#'       clearControls() %>%
#'       clearGroup("clicked_point") %>%
#'       clearGroup("selected_area") %>%
#'       clearGroup("draw")
#'     
#'     if (input$selection_mode == "point") {
#'       # Remove the draw toolbar UI completely
#'       proxy %>% removeDrawToolbar()
#'       
#'       # Clear polygon timeseries and any polygon reactive values
#'       ts_df(NULL)
#'       # If you have polygon reactive values, clear them here, e.g. poly_values(NULL)
#'       
#'     } else if (input$selection_mode == "area") {
#'       # Clear point selection and timeseries
#'       clicked_point(NULL)
#'       ts_df(NULL)
#'       
#'       # Add the draw toolbar back for area selection
#'       proxy %>%
#'         addDrawToolbar(
#'           targetGroup = "draw",
#'           rectangleOptions = drawRectangleOptions(showArea = TRUE),
#'           polylineOptions = FALSE,
#'           polygonOptions = FALSE,
#'           markerOptions = FALSE,
#'           circleOptions = FALSE,
#'           circleMarkerOptions = FALSE,
#'           editOptions = editToolbarOptions()
#'         )
#'     }
#'     
#'     output$ts_plot <- renderPlotly(NULL)
#'   })
#'   
#'   # clear points/polygons when choosing new variable ----
#'   observeEvent(input$variable, {
#'     # Clear any selected point
#'     clicked_point(NULL)
#'     # Clear any selected area polygon group from the map
#'     leafletProxy("map") %>%
#'       clearGroup("selected_area") %>%
#'       clearGroup("clicked_point") %>%
#'       clearGroup("draw") %>%
#'       clearControls() %>%
#'       removeDrawToolbar()
#'     
#'     # Clear the timeseries plot
#'     output$ts_plot <- renderPlotly(NULL)
#'     
#'     # Also clear the stored timeseries data
#'     ts_df(NULL)
#'   })
#'   
#'   ## update raster with spinner -----
#'   observeEvent({ input$variable; input$selection_mode; input$time }, {
#'     session$sendCustomMessage("showSpinner", TRUE)  # Show spinner immediately
#'     
#'     vr <- varname()
#'     selected_idx <- which(time_labels == input$time)
#'     r <- r_slices[[vr]][[selected_idx]]
#'     pal <- get_palette_for_var(vr, rast_list, var_groups, group_palettes, slice_rast = r)
#'     
#'     leafletProxy("map") %>%
#'       clearControls() %>%
#'       removeImage("raster") %>%
#'       addRasterImage(
#'         r,
#'         colors = pal,
#'         opacity = if (input$selection_mode == "area") 0.6 else 0.8,
#'         group = "raster",
#'         layerId = "raster"
#'       )
#'     
#'     # Hide spinner after a short delay to let the image render
#'     # (because leaflet addRasterImage is async and has no direct callback)
#'     invalidateLater(500, session)
#'     # observe({
#'     #   session$sendCustomMessage("showSpinner", FALSE)
#'     #   isolate({
#'     #     # Remove this observer immediately after firing once
#'     #     invalidateLater(0, session)
#'     #   })
#'     # })
#'     # 
#'     later::later(function() {
#'       session$sendCustomMessage("showSpinner", FALSE)
#'     }, 0.5)
#'   })
#'   
#'   
#'   ## legend -----
#'   output$custom_legend <- renderUI({
#'     vr <- varname()
#'     var_range <- var_range_list[[vr]]
#'     pal <- get_palette_for_var(vr, rast_list, var_groups, group_palettes)
#'     n_colors <- 100
#'     vals <- seq(var_range[1], var_range[2], length.out = n_colors)
#'     colors <- pal(vals)
#'     gradient_css <- paste0("linear-gradient(to top, ", paste0(colors, collapse = ", "), ");")
#'     range_diff <- diff(var_range)
#'     digits_to_use <- if (range_diff < 10) 2 else 0
#'     label_vals <- pretty(var_range, n = 6)
#'     label_texts <- formatC(label_vals, format = "f", digits = digits_to_use, big.mark = ",")
#'     legend_title <- display_labels[[vr]] %||% input$variable
#'     
#'     tagList(
#'       tags$div(class = "legend-title", legend_title),
#'       tags$div(style = "display: flex; align-items: center;",
#'                tags$div(style = paste0("height: 180px; width: 35px; background: ", gradient_css, 
#'                                        " border: 1px solid #000; margin-right: 0px; position: relative;")),
#'                tags$div(style = "height: 180px; display: flex; flex-direction: column; justify-content: space-between;",
#'                         lapply(rev(label_texts), function(txt) {
#'                           tags$div(style = "display: flex; align-items: center;",
#'                                    tags$div(style="width:5px; height:2px; background:black; margin-right:2px;"),
#'                                    tags$div(class = "legend-label", txt))
#'                         }))
#'       )
#'     )
#'   })
#'   
#'   ## clicked point -----
#'   clicked_point <- reactiveVal(NULL)
#'   observeEvent(input$map_click, {
#'     if (input$selection_mode != "point") return(NULL)
#'     click <- input$map_click
#'     if (!is.null(click)) {
#'       clicked_point(c(click$lng, click$lat))
#'       leafletProxy("map") %>%
#'         clearGroup("clicked_point") %>%
#'         addCircleMarkers(
#'           lng = click$lng, lat = click$lat,
#'           group = "clicked_point",
#'           radius = 3, fillColor = "black", color = "black",
#'           fillOpacity = 0.9, opacity = 1
#'         )
#'     }
#'   })
#'   
#'   ## time series for point -----
#'   observeEvent(clicked_point(), {
#'     coords <- clicked_point()
#'     vr <- varname()
#'     r_stack <- rast_list[[vr]]
#'     values <- terra::extract(r_stack, matrix(coords, ncol = 2), method = "simple")
#'     ts_values <- unlist(values[1, -1], use.names = FALSE)
#'     ts_values <- if (is.null(ts_values) || length(ts_values)==0) rep(NA, length(time_seq)) else ts_values[1:length(time_seq)]
#'     df <- data.frame(Date = time_seq, Value = ts_values)
#'     
#'     ts_df(df)
#'     
#'     output$ts_plot <- renderPlotly({
#'       p <- ggplot(df, aes(x = Date, y = Value)) +
#'         geom_line(color = "#788554", linewidth = 1) +
#'         geom_point(color = "#244C20", size=0.8) +
#'         labs(y = display_labels[[vr]] %||% input$variable, x = "") +
#'         theme_minimal(base_size = 10) +
#'         xlim(as.Date(c('2014-01-01', '2022-12-31')))
#'       ggplotly(p, tooltip = c("x", "y"))
#'     })
#'   })
#'   
#'   ## time series for area ----
#'   observeEvent(input$map_draw_new_feature, {
#'     if (input$selection_mode != "area") return(NULL)
#'     vr <- varname()
#'     r_stack <- rast_list[[vr]]
#'     feature <- input$map_draw_new_feature
#'     coords_raw <- feature$geometry$coordinates[[1]]
#'     lng <- sapply(coords_raw, `[[`, 1)
#'     lat <- sapply(coords_raw, `[[`, 2)
#'     coords <- cbind(lng, lat)
#'     poly_sf <- sf::st_sf(geometry = sf::st_sfc(sf::st_polygon(list(coords)), crs = 4326))
#'     poly_vect <- terra::vect(poly_sf)
#'     values <- terra::extract(r_stack, poly_vect)
#'     ts_means <- colMeans(values[, -1], na.rm = TRUE)
#'     ts_means <- if (is.null(ts_means) || length(ts_means)==0) rep(NA, length(time_seq)) else ts_means[1:length(time_seq)]
#'     df <- data.frame(Date = time_seq, Value = ts_means)
#'     
#'     ts_df(df)
#'     
#'     leafletProxy("map") %>%
#'       clearGroup("selected_area") %>%
#'       addPolygons(lng = lng, lat = lat, group = "selected_area",
#'                   color = "black", fillOpacity = 0.2, weight = 2)
#'     
#'     output$ts_plot <- renderPlotly({
#'       p <- ggplot(df, aes(x = Date, y = Value)) +
#'         geom_line(color = "#788554", linewidth = 1) +
#'         geom_point(color = "#244C20", size=0.8) +
#'         labs(y = display_labels[[vr]] %||% input$variable, x = "") +
#'         theme_minimal(base_size = 10) +
#'         xlim(as.Date(c('2014-01-01', '2022-12-31')))
#'       ggplotly(p, tooltip = c("x", "y"))
#'     })
#'   })
#' 
#'   # CSV output -----
#'   output$download_csv_timeseries <- downloadHandler(
#'     filename = function() {
#'       paste0("timeseries_data_", Sys.Date(), ".csv")
#'     },
#'     content = function(file) {
#'       data <- ts_df()
#'       if (is.null(data) || nrow(data) == 0) {
#'         showNotification("No time series data available for download.", type = "error")
#'         return(NULL)
#'       }
#'       write.csv(data, file, row.names = FALSE)
#'     }
#'   )
#'   
#'   # New GIF stuff -----
#'   # Reactive: Get raster subset based on variable and date range
#'   gif_rasters <- reactive({
#'     vr <- input$gif_variable
#'     print(paste("Selected variable:", vr))
#'     r_stack <- rast_list[[vr]]
#'     
#'     # Check if r_stack is a list of rasters instead of a SpatRaster
#'     if (inherits(r_stack, "list")) {
#'       print("r_stack is a list, combining into a SpatRaster")
#'       r_stack <- terra::rast(r_stack)
#'     }
#'     
#'     print(class(r_stack))
#'     print(str(r_stack))
#'     
#'     dates <- time_seq
#'     sel_dates <- dates >= input$gif_date_range[1] & dates <= input$gif_date_range[2]
#'     if (sum(sel_dates) == 0) {
#'       showNotification("No rasters found for selected date range.", type = "error")
#'       return(NULL)
#'     }
#'     print(paste("sel_dates:", which(sel_dates)))
#'     print(paste("Date range input:", input$gif_date_range[1], "to", input$gif_date_range[2]))
#'     
#'     subset_stack <- terra::subset(r_stack, which(sel_dates))
#'     
#'     print(class(subset_stack))
#'     print(str(subset_stack))
#'     
#'     names(subset_stack) <- as.character(dates[sel_dates])
#'     subset_stack
#'   })
#'   
#'   # Temporary storage for GIF
#'   gif_file <- reactiveVal(NULL)
#'   
#'   # Observe Generate button
#'   observeEvent(input$generate_gif, {
#'     req(gif_rasters())
#'     vr <- input$gif_variable
#'     r_stack <- gif_rasters()
#'     if (terra::nlyr(r_stack) == 0) {
#'       showNotification("No raster layers selected — check your date range.", type = "error")
#'       return()
#'     }
#'     dates <- time_seq[time_seq >= input$gif_date_range[1] & time_seq <= input$gif_date_range[2]]
#'     print(paste("Dates in layer:", paste(dates, collapse = ", ")))
#'     temp_dir <- tempdir()
#'     png_files <- file.path(temp_dir, paste0("frame_", seq_len(n_layers), ".png"))
#'     
#'     # Get custom palette
#'     pal <- get_palette_for_var(vr, rast_list, var_groups, group_palettes)
#'     var_range <- var_range_list[[vr]]
#'     if (is.null(var_range)) {
#'       showNotification("No variable range defined for this variable.", type = "error")
#'       return()
#'     }
#'     main_title <- var_labels[[vr]]
#'     legend_title <- display_labels[[vr]] %||% vr
#'     
#'     n_layers <- terra::nlyr(r_stack)
#'     
#'     print(paste("Number of layers:", n_layers))
#'     print(paste("Length of dates vector:", length(dates)))
#'     for (i in seq_len(n_layers)) {
#'       png(png_files[i], width = 600, height = 500)
#'       
#'       # Plot with custom palette and legend
#'       terra::plot(
#'         r_stack[[i]],
#'         col = pal(seq(var_range[1], var_range[2], length.out = 100)),
#'         #main = paste(main_title, "\n", dates[i]),
#'         main = paste0(dates[i]),
#'         plg = c(legend = legend_title),
#'         range = var_range,
#'         axes = FALSE,
#'         legend = TRUE
#'       )
#'       
#'       # Optional: Add base map if you have a basemap raster or tiles
#'       # (e.g., using `tmap` or pre-rendered hillshade, etc.)
#'       
#'       dev.off()
#'     }
#'    
#'     # Create the animated GIF
#'     gif <- magick::image_read(png_files) %>%
#'       magick::image_animate(fps = 5)
#'     
#'     out_file <- tempfile(fileext = ".gif")
#'     magick::image_write(gif, out_file)
#'     gif_file(out_file)
#'   })
#'   
#'   # Show preview
#'   output$gif_preview <- renderImage({
#'     req(gif_file())
#'     list(src = gif_file(), contentType = 'image/gif')
#'   }, deleteFile = FALSE)
#'   
#'   # Download handler
#'   output$download_gif <- downloadHandler(
#'     filename = function() {
#'       paste0("map_animation_", Sys.Date(), ".gif")
#'     },
#'     content = function(file) {
#'       req(gif_file())
#'       file.copy(gif_file(), file)
#'     }
#'   )
#'   
#' }
#' 
#' shinyApp(ui, server)
#' 
#' 
#' 
#' 
#' 
#' 
#' 
#' 
#' 
#' 
#' 
#' 
#' 
#' 
#' #08/26 version with export attempt ----
#' # this one has gif export and csv export, unsure if both will end up working or not 
#' # UI -----
#' ui <- navbarPage(
#'   ## title ----
#'   title = div("California Carbon Explorer", style = "font-size: 22px; padding-left: 10px; padding-top: 12px; padding-bottom: 0px; color: #2f3714"),
#'   
#'   
#'   theme = bslib::bs_theme(
#'     heading_font = bslib::font_google("Tenor Sans")
#'   ),
#'   
#'   id = "main_navbar",
#'   position = "fixed-top",
#'   inverse = TRUE,
#'   collapsible = TRUE,
#'   header = tagList(
#'     tags$head(
#'       ## font, panels, navbar styling ----
#'       tags$style(HTML("
#'          @font-face {
#'           font-family: 'DMSans';
#'           src: url('DMSans-Variable.ttf') format('truetype');
#'           font-weight: 100 900;
#'           font-style: normal;
#'         }
#' 
#'         html, body {
#'           height: 100%;
#'           margin: 0;
#'           padding: 0;
#'           overflow: hidden;
#'         }
#' 
#'         body, input, button, select, textarea {
#'           font-family: 'DMSans', sans-serif;
#'           font-weight: 300;
#'           font-size: 14px;
#'           color: #333;
#'         }
#'         
#'          h2, h3, h4, h5, h6 {
#'           font-family: 'DMSans', sans-serif;
#'           font-weight: 600;
#'           font-size: 18px;
#'         }
#'         
#'         h1 {
#'           font-family: 'Tenor Sans', sans-serif;
#'         }
#' 
#'         .panel-well {
#'           padding: 10px;
#'           border-radius: 8px;
#'           box-shadow: 0 2px 4px rgba(0,0,0,0.2);
#'           font-weight: 300;
#'           overflow: hidden;
#'           background-color: #f4efe7;
#'         }
#'         
#'         .panel-legend {
#'           background-color: rgba(255, 255, 255, 0.0);
#'           box-shadow: none;
#'           border: none;
#'         }
#' 
#'         .legend-title {
#'           font-family: 'DMSans', sans-serif;
#'           font-weight: 510;
#'           font-size: 16px;
#'           margin-bottom: 4px;
#'           color: #333;
#'         }
#' 
#'         .legend-label {
#'           font-family: 'DMSans', sans-serif;
#'           font-weight: 300;
#'           font-size: 14px;
#'           color: #333;
#'         }
#'         
#'         .irs-single, .irs-bar, .irs-grid-text {
#'           font-size: 10px !important;
#'           font-family: 'DMSans', sans-serif;
#'         }
#' 
#'         .ui-resizable-handle.ui-resizable-sw {
#'           width: 20px;
#'           height: 20px;
#'           position: absolute;
#'           left: 0;
#'           bottom: 0;
#'           cursor: nesw-resize;
#'           background: url(\"data:image/svg+xml,%3Csvg xmlns='http://www.w3.org/2000/svg' width='20' height='20'%3E%3Cline x1='2' y1='14' x2='6' y2='18' stroke='rgba(0,0,0,0.3)' stroke-width='1'/%3E%3Cline x1='2' y1='8' x2='12' y2='18' stroke='rgba(0,0,0,0.3)' stroke-width='1'/%3E%3C/svg%3E\") no-repeat center center;
#'           background-size: 100% 100%;
#'         }
#' 
#'         .navbar, .container-fluid, .tab-content {
#'           margin: 0 !important;
#'           padding: 0 !important;
#'         }
#'         
#'         /* ========== NAVBAR OVERALL BACKGROUND ========== */
#'         .navbar {
#'           background-color: #f3e6d3 !important;   /* Navbar background color */
#'           border-bottom: 0px solid #c5d68d;       /* Bottom border/accent line */
#'         }
#' 
#'         /* ========== NAVBAR BRAND TITLE & TABS (RESTING STATE) ========== */
#'         .navbar .navbar-brand,
#'         .navbar-default .navbar-nav > li > a {
#'           color: #72654c !important;              /* Color of title and tab text when idle */
#'           font-weight: 500;                       /* Slight boldness */
#'         }
#' 
#'         /* ========== NAVBAR TABS ON HOVER ========== */
#'         .navbar-default .navbar-nav > li > a:hover {
#'           color: #C29A6D !important;              /* Text color when hovered */
#'              /* Background color when hovered */
#'         }
#' 
#'         /* ========== NAVBAR TABS WHEN ACTIVE (CURRENTLY SELECTED) ========== */
#'         /* This may not always be visible depending on whether you're using tabs inside navbarPage or tabsetPanel. */
#'         /* For tabs inside navbarPage, this is typically handled by Bootstrap nav-tabs styles instead. */
#'         .navbar-default .navbar-nav > .active > a,
#'         .navbar-default .navbar-nav > .active > a:focus,
#'         .navbar-default .navbar-nav > .active > a:hover {
#'           background-color: #c5d68d !important;   /* Background of selected tab */
#'           color: #9c7854 !important;              /* Text color of selected tab */
#'           border-bottom: 2px solid #c5d68d !important; /* Optional: bottom border accent for active tab */
#'         }
#' 
#'       /* === SLIMMER NAVBAR === */
#'         .navbar {
#'           min-height: 40px !important;   /* reduce total navbar height */
#'           padding-top: 0px !important;
#'           padding-bottom: 0px !important;
#'         }
#' 
#'         .navbar-nav > li > a {
#'           padding-top: 0px !important;
#'           padding-bottom: 0px !important;
#'         }
#' 
#'         /* === ALIGN NAVBAR TITLE LOWER === */
#'         .navbar .navbar-brand {
#'          padding-top: 2px !important;     /* lowers the title */
#'         padding-bottom: 8px !important;  /* optional, adds balance */
#'         line-height: 1.2 !important;     /* ensures text doesn’t stretch vertically */
#'         display: flex;
#'         align-items: flex-end;           /* align text to bottom of the bar */
#'       }
#'         #initial-loader {
#'           position: fixed;
#'           top: 0; left: 0;
#'           width: 100vw; height: 100vh;
#'           background-color: white;
#'           z-index: 99999;
#'           display: flex;
#'           align-items: center;
#'           justify-content: center;
#'         }
#' 
#'         .lds-dual-ring-initial:after {
#'           content: ' ';
#'           display: block;
#'           width: 64px;
#'           height: 64px;
#'           border-radius: 50%;
#'           border: 6px solid #244C20;
#'           border-color: #244C20 transparent #244C20 transparent;
#'           animation: lds-dual-ring 1.2s linear infinite;
#'         }
#'         
#'         /* === NAVBAR FONT OVERRIDE === */
#'         .navbar .navbar-brand,
#'         .navbar-nav > li > a {
#'           font-family: 'Tenor Sans', sans-serif !important;
#'           letter-spacing: 0.3px;
#'         }
#'         
#'         @keyframes lds-dual-ring {
#'           0% { transform: rotate(0deg); }
#'           100% { transform: rotate(360deg); }
#'         }
#'       ")),
#'       ## panel line spacing ----
#'       tags$style(HTML("
#'   .panel-well, .card, .panel, .card-body, .panel-body {
#'     line-height: 1.2;   /* default is ~1.5 */
#'   }
#' ")),
#'       # offset for fixed-top navbar so tab content isn't hidden
#'       tags$style(HTML("#shiny-tab-Export,
#' #shiny-tab-Export > .container-fluid {
#'   padding-top: 65px !important;
#' }
#' ")),
#'       ## loader ----
#'       tags$script(HTML("
#'         $(document).on('shiny:connected', function() {
#'           $('#initial-loader').fadeOut();
#'         });
#'       ")),
#'       ## draw toolbar ----
#'       tags$script(HTML("
#'   Shiny.addCustomMessageHandler('showSpinner', function(show) {
#'     var spinner = document.getElementById('map-spinner');
#'     if (spinner) {
#'       spinner.style.display = show ? 'block' : 'none';
#'     }
#'   });
#'   .leaflet-draw-toolbar {
#'           top: 30px !important;
#'           left: 10px !important;
#'           z-index: 1000 !important;
#'         }
#' ")),
#'       ## info tabs styling ----
#'       tags$style(HTML("
#'   /* --- Info tab styling --- */
#'   .info-tab {
#'     background-color: #f8f1e7; /* lighter than #f3e6d3 */
#'     padding: 80px 80px;
#'     border-radius: 8px;
#'     max-width: 1200px;
#'     margin: 20px auto;
#'     line-height: 1.6;
#'     font-size: 17px;
#'     color: #2c2c2c;
#'   }
#' 
#'   .info-tab h2 {
#'     font-family: 'Tenor Sans', sans-serif;
#'     font-size: 25px;
#'     font-weight: 600;
#'     margin-bottom: 20px;
#'     color: #3b2d1f;
#'   }
#' 
#'   .info-tab p {
#'     font-family: 'Nanum Myeongjo', serif;
#'     margin-bottom: 20px;
#'   }
#' 
#'   /* Optional: a subtle shadow and border for separation */
#'   .info-tab {
#'     box-shadow: 0px 2px 6px rgba(0,0,0,0.1);
#'     border: 1px solid #e6d9c7;
#'   }
#' ")),
#'       ## navbar dropdown menu ----
#'       tags$style(HTML("
#'   /* --- Match dropdown to navbar color --- */
#'   .navbar .dropdown-menu {
#'     background-color: #f3e6d3 !important; /* same as navbar */
#'     border: none;
#'     box-shadow: 0px 2px 6px rgba(0,0,0,0.15);
#'   }
#' 
#'   /* Dropdown items text and hover behavior */
#'   .navbar .dropdown-menu > li > a {
#'     color: #3b2d1f !important; /* dark brown text for readability */
#'     font-family: 'Tenor Sans', sans-serif;
#'     font-size: 14px;
#'     padding: 5px 10px;
#'   }
#' 
#'   .navbar .dropdown-menu > li > a:hover {
#'     background-color: #f8f1e7 !important; /* lighter tone on hover */
#'     color: #2a1d12 !important;
#'   }
#' 
#'   /* Optional: improve spacing around dropdown toggle */
#'   .navbar-nav > li > .dropdown-toggle {
#'     padding-bottom: 10px;
#'   }
#' 
#'   /* Optional: adjust caret (arrow) color */
#'   .navbar .dropdown-toggle::after {
#'     border-top-color: #3b2d1f !important;
#'   }
#' ")),
#'       tags$link(rel = "stylesheet", href = "https://cdnjs.cloudflare.com/ajax/libs/leaflet.draw/1.0.4/leaflet.draw.css"),
#'       tags$script(src = "https://cdnjs.cloudflare.com/ajax/libs/leaflet.draw/1.0.4/leaflet.draw.js")
#'     )
#'   ),
#'   shinyjs::useShinyjs(),
#'   ## --- Map View Tab ----
#'   tabPanel(
#'     title = "Map View",
#'     tags$div(
#'       id = "map-container",
#'       style = "position: relative; height: 100vh; width: 100vw;",
#'       leafletOutput("map", height = "100%", width = "100%"),
#'       tags$div(
#'         id = "map-spinner",
#'         style = "position: absolute; top: 50%; left: 50%; transform: translate(-50%, -50%);
#'                  z-index: 9999; display: none;",
#'         tags$div(class = "lds-dual-ring-initial")
#'       )
#'     ),
#'     
#'     ### Controls panel ----
#'     absolutePanel(
#'       top = 60, left = 15, width = 325, bottom=367,
#'       class = "panel-well",
#'       h4("Controls"),
#'       selectInput("variable", "Select Variable:",
#'                   choices = names(var_labels),
#'                   selected = names(var_labels)[1]),
#'       radioButtons("selection_mode", "Selection Mode:",
#'                    choices = c("Point" = "point", "Area" = "area"),
#'                    selected = "point",
#'                    inline = TRUE),
#'       tags$p("Click a point or draw a box on the map to view time series."),
#'       div(style = "margin-bottom:0px;",
#'           fileInput("roi_upload", "Upload Shapefile (.zip)", accept = ".zip")
#'       ),
#'       div(style = "margin-top:0px; margin-bottom:0px;",
#'           actionButton("add_roi", "Add ROI to Map", class = "btn-primary")
#'       ),
#'       textOutput("date_text")
#'     ),
#'     
#'     ### Time slider -----
#'     absolutePanel(
#'       top = 465, bottom = 245, left = 15, width = 325,
#'       class = "panel-well",
#'       style = "padding-left: 20px",
#'       shinyWidgets::sliderTextInput(
#'         inputId = "time",
#'         label = "Select Month:",
#'         choices = time_labels,
#'         selected = time_labels[1],
#'         grid = FALSE,
#'         animate = animationOptions(interval = 1500, loop = TRUE),
#'         width = "100%"
#'       )
#'     ),
#'     
#'     ### Time series panel ----
#'     jqui_resizable(
#'       absolutePanel(
#'         top = 60, right = 15,
#'         class = "panel-well",
#'         style = "min-width: 300px; min-height: 200px; width: 410px; height: 300px; position: absolute;",
#'         h4("Time Series of Selected Pixel / Area", style = "text-align: center"),
#'         tags$div(
#'           style = "height: calc(100% - 35px); width: 100%;",
#'           plotlyOutput("ts_plot", height = "100%", width = "100%")
#'         )
#'       ),
#'       options = list(handles = "sw")
#'     ),
#'     
#'     ### Legend panel ----
#'     absolutePanel(
#'       bottom = 10, left = 15, width = 290,
#'       class = "panel-well panel-legend",
#'       uiOutput("custom_legend")
#'     )
#'   ),
#'   
#'   ## Export tab ----
#'   tabPanel(
#'     "Export",
#'     div(style = "padding-top: 50px;",
#'         sidebarLayout(
#'       
#'       # Sidebar
#'       sidebarPanel(
#'         h4("Export Options"),
#'         
#'         tags$p(
#'           style = "font-style: italic; color: #666;",
#'           "Note: Exporting a csv uses the point or area selected on the 'Map View' tab."
#'         ),
#'         
#'         # Export type selector
#'         radioButtons(
#'           "export_type", 
#'           "Export Type:",
#'           choices = c(
#'             "CSV (Point or Area)" = "csv",
#'             "GIF (Time Series Map)" = "gif"
#'           )
#'         ),
#'         
#'         # CSV options
#'         conditionalPanel(
#'           condition = "input.export_type == 'csv'",
#'           selectInput("csv_variable", "Variable:", choices = var_labels),
#'           selectInput("export_point_or_area", "Extract Type:", choices = c("Point", "Polygon")),
#'           actionButton("generate_csv_preview", "Generate Preview"),
#'           uiOutput("csv_download_ui")
#'         ),
#'         
#'         # GIF options
#'         conditionalPanel(
#'           condition = "input.export_type == 'gif'",
#'           selectInput("gif_variable", "Variable:", choices = var_labels),
#'           dateRangeInput(
#'             "gif_timerange", 
#'             "Date Range:",
#'             start = "2014-01-01", 
#'             end = "2022-12-01"
#'           ),
#'           sliderInput("gif_fps", "Frames per Second:", min = 1, max = 10, value = 3),
#'           actionButton("generate_gif", "Generate GIF Preview")
#'         )
#'       ),
#'       
#'       # Main panel for previews
#'       mainPanel(
#'         style = "margin-top: 20px;",
#'         
#'         # CSV preview container
#'         conditionalPanel(
#'           condition = "input.export_type == 'csv'",
#'           div(
#'             id = "csv_panel",
#'             DT::dataTableOutput("csv_preview")
#'           )
#'         ),
#'         
#'         # GIF preview container
#'         conditionalPanel(
#'           condition = "input.export_type == 'gif'",
#'           div(
#'             id = "gif_panel",
#'             style = "margin-top: 10px; text-align: center; padding: 0; width: 100%;",
#'             # Single UI output for everything
#'             uiOutput("gif_preview_block")
#'           )
#'         )
#'       )
#'     )
#'   )
#'   ),
#'   
#'   ## --- Other Pages ----
#'   navbarMenu("About this tool", 
#'              tabPanel("Overview", div(class = "info-tab",
#'                                       h2("Overview"),
#'                                       p("Page under construction...")
#'              )),
#'              tabPanel("Instructions", div(class = "info-tab",
#'                                           h2("Instructions"),
#'                                           p("Page under construction...")
#'              )),
#'              tabPanel("The CARDAMOM Framework", div(class = "info-tab", 
#'                                                   h2("The CARDAMOM Framework"), 
#'                                                   p("The terrestrial carbon cycle is complex, involving multiple “pools” and processes that transfer carbon through an ecosystem. 
#'                                                       While remote sensing techniques and field data collection can give us estimates of photosynthetic rates or biomass accumulation, it would be near impossible to measure the sizes and transfer rates of all ecosystem carbon pools. 
#'                                                       Mechanistic carbon models can provide insight into turnover and allocation rates and ecosystem fluxes, helping us to better understand the entirety of an ecosystem’s carbon cycle. 
#'                                                       The CARbon DAta MOdel FraMework (CARDAMOM; ) is a model-data assimilation framework that incorporates an ecosystem carbon model with meteorological and observational data to simulate carbon fluxes in an ecosystem of interest."), 
#' 	                                                p("To understand the CARDAMOM framework, and model-data assimilation more generally, it can be helpful to start by visualizing a traditional “forward” ecosystem model. 
#' 	                                                    This method would input “forcing” data, usually climatic variables, into a complex process-based model, with assigned parameter values that are informed by the region’s plant functional type or land cover class. 
#' 	                                                    The output from this model would then be validated with an independent external dataset, usually with a large degree of mismatch."))),
#'              tabPanel("Variable Descriptions", div(class = "info-tab",
#'                                                    h2("Variable Info"),
#'                                                    p("Page under construction...")
#'              )),
#'              tabPanel("Model Information", div(class = "info-tab",
#'                                                h2("Model Info"),
#'                                                p("Page under construction...")
#'              ))
#'             
#'   ),
#'   
#'   # --- Initial Loading Spinner ---
#'   #tags$div(id = "initial-loader", tags$div(class = "lds-dual-ring-initial"))
#' )
#' 
#' # SERVER ----------------------
#' server <- function(input, output, session) {
#'   
#'   observe({
#'     invalidateLater(500, session)  # wait 500ms
#'     session$sendCustomMessage("enableResizable", TRUE)
#'   })
#'   
#'   # --- Reactive value to store uploaded ROI ---
#'   roi_data <- reactiveVal(NULL)
#'   
#'   varname <- reactive({
#'     var_labels[[input$variable]]
#'   })
#'   
#'   # When user uploads zipped shapefile
#'   observeEvent(input$roi_upload, {
#'     req(input$roi_upload)
#'     
#'     tmpdir <- tempdir()
#'     unzip(input$roi_upload$datapath, exdir = tmpdir)
#'     shp_file <- list.files(tmpdir, pattern = "\\.shp$", full.names = TRUE)
#'     
#'     if (length(shp_file) > 0) {
#'       roi_sf <- tryCatch(
#'         sf::st_read(shp_file, quiet = TRUE),
#'         error = function(e) NULL
#'       )
#'       roi_data(roi_sf)   # store in the reactiveVal
#'     }
#'   })
#'   
#' 
#'   
#'   ## pre-caching raster slices -----
#'   r_slices <- reactiveValues()
#'   observe({
#'     for (vr_name in names(rast_list)) {
#'       r_stack <- rast_list[[vr_name]]
#'       r_slices[[vr_name]] <- lapply(1:nlyr(r_stack), function(i) terra::subset(r_stack, i))
#'     }
#'   })
#'   
#'   ## render map -----
#'   output$map <- renderLeaflet({
#'     leaflet(options = leafletOptions(zoomControl = FALSE)) %>%
#'       addProviderTiles(providers$CartoDB.Positron) %>%
#'       fitBounds(lng1 = -128.4, lat1 = 35.5, lng2 = -115.1, lat2 = 41.5) %>%
#'       htmlwidgets::onRender("
#'         function(el, x) {
#'           var map = this;
#'           map._drawnItems = new L.FeatureGroup();
#'           map.addLayer(map._drawnItems);
#'           // create empty feature group for drawings
#'       }
#'       ") %>%
#'       htmlwidgets::onRender(
#'         "function(el, x) {
#'           L.control.zoom({position:'bottomright'}).addTo(this);
#'         }") %>%
#'       addScaleBar(position = "bottomright",   # add scale bar
#'                   options = scaleBarOptions(maxWidth = 100, 
#'                                             imperial = TRUE,
#'                                             updateWhenIdle = TRUE))
#' 
#'        
#'   })
#'   
#'   # Add ROI to map when button clicked
#'   observeEvent(input$add_roi, {
#'     req(roi_data())
#'     
#'     leafletProxy("map") %>%
#'       clearGroup("roi") %>%  # remove old ROI if present
#'       addPolygons(
#'         data = roi_data(),
#'         group = "roi",
#'         color = "red", weight = 2, fillOpacity = 0.2
#'       ) %>%
#'       addLayersControl(
#'         overlayGroups = c("roi"),
#'         options = layersControlOptions(collapsed = FALSE)
#'       )
#'   })
#'   
#'   ## selection mode ----
#'   observeEvent(input$selection_mode, {
#'     proxy <- leafletProxy("map")
#' 
#'     proxy %>%
#'       clearControls() %>%
#'       clearGroup("clicked_point") %>%
#'       clearGroup("selected_area") %>%
#'       clearGroup("draw")
#' 
#'     if (input$selection_mode == "point") {
#'       # Remove the draw toolbar UI completely
#'       proxy %>% removeDrawToolbar()
#' 
#' 
#'     } else if (input$selection_mode == "area") {
#'       # Clear point selection and timeseries
#'       clicked_point(NULL)
#' 
#' 
#'       # Add the draw toolbar back for area selection
#'       proxy %>%
#'         addDrawToolbar(
#'           targetGroup = "_drawnItems",
#'           position = "bottomleft",
#'           rectangleOptions = drawRectangleOptions(
#'             shapeOptions = drawShapeOptions(color = "#788554", weight = 2, fillOpacity = 0.2),
#'             showArea = TRUE
#'           ),
#'           polylineOptions = FALSE,
#'           polygonOptions = drawPolygonOptions(
#'             shapeOptions = drawShapeOptions(color = "#788554", weight = 2, fillOpacity = 0.2),
#'           showArea = TRUE),
#'           markerOptions = FALSE,
#'           circleOptions = FALSE,
#'           circleMarkerOptions = FALSE,
#'           editOptions = editToolbarOptions(edit = TRUE)
#'         )
#'     }
#' 
#'     output$ts_plot <- renderPlotly(NULL)
#'   })
#' 
#' 
#'   # # ## selection mode ----
#'   # observeEvent(input$selection_mode, {
#'   #   proxy <- leafletProxy("map")
#'   #   
#'   #   # Clear previous drawings and point/area selections
#'   #   proxy %>%
#'   #     clearGroup("clicked_point") %>%
#'   #     clearGroup("selected_area") %>%
#'   #     clearGroup("draw")
#'   #   
#'   #   # Use JavaScript to enable/disable the draw toolbar
#'   #   shinyjs::runjs(sprintf("
#'   #   var map = $('#map').data('leaflet-map');
#'   #   if (!map || !map._customDrawControl) return;
#'   #   var handler = map._customDrawControl._toolbars.draw._modes.rectangle.handler;
#'   #   if ('%s' === 'area') {
#'   #     handler.enable();         // show and enable toolbar
#'   #   } else {
#'   #     handler.disable();        // hide/disable toolbar
#'   #   }
#'   # ", input$selection_mode))
#'   #   
#'   #   # Clear the timeseries plot when switching modes
#'   #   output$ts_plot <- renderPlotly(NULL)
#'   # })
#'   # 
#'   # # clear points/polygons when choosing new variable ----
#'   # observeEvent(input$variable, {
#'   #   # Clear any selected point
#'   #   clicked_point(NULL)
#'   #   # Clear any selected area polygon group from the map
#'   #   leafletProxy("map") %>%
#'   #     clearGroup("selected_area") %>%
#'   #     clearGroup("clicked_point") %>%
#'   #     clearGroup("draw") %>%
#'   #     clearControls() %>%
#'   #     removeDrawToolbar()
#'   #   
#'   #   # Clear the timeseries plot
#'   #   output$ts_plot <- renderPlotly(NULL)
#'   #   
#'   #   
#'   # })
#'   
#'   # Show/hide spinner for Map View tab updates
#'   observeEvent({ input$main_navbar; input$variable; input$selection_mode; input$time }, {
#'     if (input$main_navbar != "Map View") return()  # only map tab
#'     
#'     session$sendCustomMessage("showSpinner", TRUE)  # show spinner immediately
#'     
#'     vr <- varname()
#'     selected_idx <- which(time_labels == input$time)
#'     r <- r_slices[[vr]][[selected_idx]]
#'     pal <- get_palette_for_var(vr, rast_list, var_groups, group_palettes, slice_rast = r)
#'     
#'     leafletProxy("map") %>%
#'       clearControls() %>%
#'       removeImage("raster") %>%
#'       addRasterImage(
#'         r,
#'         colors = pal,
#'         opacity = if (input$selection_mode == "area") 0.6 else 0.8,
#'         group = "raster",
#'         layerId = "raster"
#'       )
#'     
#'      later::later(function() {
#'        session$sendCustomMessage("showSpinner", FALSE)
#'      }, 0.5)
#'      
#'   })
#'   
#'   # Show spinner during GIF generation only
#'   # observeEvent(gif_generating(), {
#'   #   session$sendCustomMessage("showSpinner", gif_generating() && input$main_navbar == "Export")
#'   # })
#'   
#'   ## legend -----
#'   output$custom_legend <- renderUI({
#'     vr <- varname()
#'     var_range <- var_range_list[[vr]]
#'     pal <- get_palette_for_var(vr, rast_list, var_groups, group_palettes)
#'     n_colors <- 100
#'     vals <- seq(var_range[1], var_range[2], length.out = n_colors)
#'     colors <- pal(vals)
#'     gradient_css <- paste0("linear-gradient(to top, ", paste0(colors, collapse = ", "), ");")
#'     range_diff <- diff(var_range)
#'     digits_to_use <- if (range_diff < 10) 2 else 0
#'     label_vals <- pretty(var_range, n = 6)
#'     label_texts <- formatC(label_vals, format = "f", digits = digits_to_use, big.mark = ",")
#'     legend_title <- display_labels[[vr]] %||% input$variable
#'     
#'     tagList(
#'       tags$div(class = "legend-title", legend_title),
#'       tags$div(style = "display: flex; align-items: center;",
#'                tags$div(style = paste0("height: 180px; width: 35px; background: ", gradient_css, 
#'                                        " border: 1px solid #000; margin-right: 0px; position: relative;")),
#'                tags$div(style = "height: 180px; display: flex; flex-direction: column; justify-content: space-between;",
#'                         lapply(rev(label_texts), function(txt) {
#'                           tags$div(style = "display: flex; align-items: center;",
#'                                    tags$div(style="width:5px; height:2px; background:black; margin-right:2px;"),
#'                                    tags$div(class = "legend-label", txt))
#'                         }))
#'       )
#'     )
#'   })
#'   
#'   ## clicked point -----
#'   clicked_point <- reactiveVal(NULL)
#'   observeEvent(input$map_click, {
#'     if (input$selection_mode != "point") return(NULL)
#'     click <- input$map_click
#'     if (!is.null(click)) {
#'       clicked_point(c(click$lng, click$lat))
#'       leafletProxy("map") %>%
#'         clearGroup("clicked_point") %>%
#'         addCircleMarkers(
#'           lng = click$lng, lat = click$lat,
#'           group = "clicked_point",
#'           radius = 3, fillColor = "black", color = "black",
#'           fillOpacity = 0.9, opacity = 1
#'         )
#'     }
#'   })
#'   
#'   ## time series for point -----
#'   observeEvent(clicked_point(), {
#'     coords <- clicked_point()
#'     vr <- varname()
#'     r_stack <- rast_list[[vr]]
#'     values <- terra::extract(r_stack, matrix(coords, ncol = 2), method = "simple")
#'     ts_values <- unlist(values[1, -1], use.names = FALSE)
#'     ts_values <- if (is.null(ts_values) || length(ts_values)==0) rep(NA, length(time_seq)) else ts_values[1:length(time_seq)]
#'     df <- data.frame(Date = time_seq, Value = ts_values)
#'     output$ts_plot <- renderPlotly({
#'       p <- ggplot(df, aes(x = Date, y = Value)) +
#'         geom_line(color = "#788554", linewidth = 1) +
#'         geom_point(color = "#244C20", size=0.8) +
#'         labs(y = display_labels[[vr]] %||% input$variable, x = "") +
#'         theme_minimal(base_size = 10) +
#'         xlim(as.Date(c('2014-01-01', '2022-12-31')))
#'       ggplotly(p, tooltip = c("x", "y"))
#'     })
#'   })
#'   
#'   ## time series for area
#'   observeEvent(input$map_draw_new_feature, {
#'     if (input$selection_mode != "area") return(NULL)
#'     vr <- varname()
#'     r_stack <- rast_list[[vr]]
#'     feature <- input$map_draw_new_feature
#'     coords_raw <- feature$geometry$coordinates[[1]]
#'     lng <- sapply(coords_raw, `[[`, 1)
#'     lat <- sapply(coords_raw, `[[`, 2)
#'     coords <- cbind(lng, lat)
#'     poly_sf <- sf::st_sf(geometry = sf::st_sfc(sf::st_polygon(list(coords)), crs = 4326))
#'     poly_vect <- terra::vect(poly_sf)
#'     values <- terra::extract(r_stack, poly_vect)
#'     ts_means <- colMeans(values[, -1], na.rm = TRUE)
#'     ts_means <- if (is.null(ts_means) || length(ts_means)==0) rep(NA, length(time_seq)) else ts_means[1:length(time_seq)]
#'     df <- data.frame(Date = time_seq, Value = ts_means)
#'     leafletProxy("map") %>%
#'       clearGroup("selected_area") %>%
#'       addPolygons(lng = lng, lat = lat, group = "selected_area",
#'                   color = "#788554", fillOpacity = 0.2, weight = 2)
#'     output$ts_plot <- renderPlotly({
#'       p <- ggplot(df, aes(x = Date, y = Value)) +
#'         geom_line(color = "#788554", linewidth = 1) +
#'         geom_point(color = "#244C20", size=0.8) +
#'         labs(y = display_labels[[vr]] %||% input$variable, x = "") +
#'         theme_minimal(base_size = 10) +
#'         xlim(as.Date(c('2014-01-01', '2022-12-31')))
#'       ggplotly(p, tooltip = c("x", "y"))
#'     })
#'   })
#'   
#'   ## GIF related -----
#'   gif_file <- reactiveVal(NULL)
#'   gif_generating <- reactiveVal(FALSE)
#'   
#'   # Reactive to indicate whether a GIF exists
#'   output$gif_file <- reactive({
#'     !is.null(gif_file())
#'   })
#'   outputOptions(output, "gif_file", suspendWhenHidden = FALSE)
#'   
#'   output$gif_preview_block <- renderUI({
#'     req(input$export_type == "gif")
#'     
#'     if (gif_generating()) {
#'       # Only status message while generating
#'       tags$div(
#'         style = "color: #244C20; font-weight: bold;",
#'         "Generating GIF, please wait..."
#'       )
#'     } else
#'       
#'       if (!is.null(gif_file())) {
#'       # Status message + GIF preview + download button
#'       tags$div(
#'         tags$img(
#'           src = base64enc::dataURI(file = gif_file(), mime = "image/gif"),
#'           style = "max-width: 100%; height: auto; border: none; display: block; margin-top:0px; margin-bottom:0px"
#'         ),
#'         tags$div(
#'           style = "margin-top: 2px;",
#'           downloadButton("download_gif", "Download GIF", style = "display: inline-block;")
#'         )
#'       )
#'     } 
#'     
#'     else {
#'       # Default message
#'       tags$p("GIF preview will generate below...")
#'     }
#'   })
#'   
#'   
#'   # gif palette fxn
#'   gif_get_palette_for_var <- function(var_name, rast_list, var_groups, group_palettes, slice_rast = NULL) {
#'     
#'     group <- var_groups[[var_name]]
#'     pal_name <- group_palettes[[group]]
#'     
#'     # Compute global min/max across all time
#'     var_stack <- rast_list[[var_name]]
#' 
#'     var_range <- terra::global(var_stack, fun = range, na.rm = TRUE)
#'     
#'     
#'     vr_min <- min(var_range$X1, na.rm=TRUE)
#'     vr_max <- max(var_range$X2, na.rm=TRUE)
#'     domain <- c(vr_min, vr_max)
#'     
#'     
#'     # Use viridisLite or RColorBrewer or scico depending on palette
#'     viridis_opts <- c("viridis", "magma", "plasma", "inferno", "cividis", "turbo", "mako", "rocket")
#'     reverse_pal_groups <- c("Foliage")
#'     if (pal_name %in% viridis_opts) {
#'       cols <- viridisLite::viridis(100, option = pal_name)
#'     } else {
#'       if (group %in% reverse_pal_groups) {
#'         cols <- scico(100, direction = -1, palette = pal_name)
#'       } 
#'           
#'           else {
#'         #cols <- colorRampPalette(RColorBrewer::brewer.pal(9, pal_name))(100)
#'         cols <- scico(100, palette = pal_name)
#'           }
#'       
#'     }
#'     return(cols)
#'   }
#'   
#'   # Compute global min/max across all frames
#' 
#'   
#'   # GIF generator function
#'   generate_gif <- function(vr, rast_subset, filename, fps = 5, width = 950, height = 750) {
#'     temp_dir <- tempdir()
#'     png_paths <- file.path(temp_dir, sprintf("frame_%03d.png", seq_len(nlyr(rast_subset))))
#'     
#'     # Ensure width/height are integers
#'     width <- as.integer(width)
#'     height <- as.integer(height)
#'     
#'     # Create palette
#'     pal_fun <- gif_get_palette_for_var(vr, rast_list, var_groups, group_palettes)
#'     
#'     var_range <- terra::global(rast_subset, fun = range, na.rm = TRUE)
#'     vr_min <- min(var_range$X1, na.rm=TRUE)
#'     vr_max <- max(var_range$X2, na.rm=TRUE)
#'     domain <- c(vr_min, vr_max)
#'     
#'     rast_subset_p <- terra::project(rast_subset, "EPSG:3857")
#'     projected_ext <- st_bbox(rast_subset_p, crs=st_crs(3857))
#'     
#'     # Define the buffer distance
#'     r <- 80000 # 10,000 meters
#'     
#'     bbox_expanded <- projected_ext + c(-r, -r, -60000, r)
#'     
#'     basemap <- basemap_terra(bbox_expanded, map_service="carto", map_type="voyager")
#'     
#'     group <- var_groups[[vr]]
#'     if (group == "Fire") {
#'       pal_fun 
#'       for (i in seq_len(nlyr(rast_subset_p))) {
#'         png(png_paths[i], width = width, height = height)
#'         
#'         # Plot basemap
#'         plot(basemap, 
#'              main = paste("Date:", names(rast_subset)[i]), box=FALSE, mar=c(3, 3, 6, 2), axes=FALSE)
#'         
#'         # Make a copy of this slice
#'         #slice <- rast_subset_p[[i]]
#'         
#'         # Mask zeros for plotting, but only temporarily
#'         #slice[slice == 0] <- NA
#'         
#'         #plot(rast_subset_p[[i]] == 0, col = "#000000", legend = FALSE, add=TRUE)
#'         # Plot non-zero values with palette
#'         plot(rast_subset_p[[i]], col = pal_fun, range = domain, type="continuous", 
#'              plg=list(title = display_labels[[vr]]), axes=FALSE, legend=TRUE, add=TRUE)
#'         
#'         # Overlay zeros in black
#'         
#'         
#'         dev.off()
#'       }
#'     }
#'     
#'     else {
#'     for (i in seq_len(nlyr(rast_subset_p))) {
#'       png(png_paths[i], width = width, height = height)
#'       
#'       # Plot basemap first 
#'       plot(basemap, 
#'            main = paste("Date:", names(rast_subset)[i]), box=FALSE, mar=c(3, 3, 6, 2), axes=FALSE)
#'       
#'       # Plot raster frame with consistent legend
#'       plot(rast_subset_p[[i]], col = pal_fun, range = domain, 
#'            plg=list(title = display_labels[[vr]]), add = TRUE, axes=FALSE)
#'       
#'       dev.off()
#'     }
#'       }
#'     
#'     # Ensure frames exist
#'     if (!all(file.exists(png_paths))) {
#'       stop("Some PNG frames were not created properly.")
#'     }
#'     
#'     # Generate GIF
#'     ok <- tryCatch({
#'       gifski::gifski(
#'         png_paths,
#'         gif_file  = filename,
#'         width     = width,
#'         height    = height,
#'         delay     = 1 / fps
#'       )
#'       TRUE
#'     }, error = function(e) {
#'       message("GIF generation failed: ", e$message)
#'       FALSE
#'     })
#'     
#'     if (!ok) stop("GIF generation failed.")
#'     
#'     # Cleanup PNG frames
#'     unlink(png_paths)
#'     
#'     return(filename)
#'   }
#'   
#'   # Observe event for Generate GIF button
#'   observeEvent(input$generate_gif, {
#'     req(input$gif_variable)
#'     req(input$gif_timerange)
#'     req(length(input$gif_timerange) == 2)
#'     
#'     # Validate date format
#'     if (any(is.na(as.Date(input$gif_timerange, format = "%Y-%m-%d")))) {
#'       showNotification("Invalid date format", type = "error")
#'       return()
#'     }
#'     
#'     gif_generating(TRUE)
#'     gif_file(NULL)
#'     
#'     vr <- input$gif_variable
#'     r_stack <- rast_list[[vr]]
#'     
#'     rast_dates <- tryCatch({
#'       as.Date(paste0(as.character(names(r_stack)), "-01"), format = "%Y-%m-%d")
#'     }, error = function(e) {
#'       showNotification("Error parsing raster dates.", type = "error")
#'       return(NULL)
#'     })
#'     
#'     req(!is.null(rast_dates))
#'     
#'     start_date <- as.Date(input$gif_timerange[1])
#'     end_date <- as.Date(input$gif_timerange[2])
#'     
#'     keep_idx <- which(rast_dates >= start_date & rast_dates <= end_date)
#'     if (length(keep_idx) == 0) {
#'       showNotification("No layers found for the selected date range.", type = "error")
#'       gif_generating(FALSE)
#'       return(NULL)
#'     }
#'     
#'     r_subset <- terra::subset(r_stack, keep_idx)
#'     filename <- file.path(tempdir(), paste0("map_animation_", Sys.Date(), ".gif"))
#'     
#'     tryCatch({
#'       gif_path <- generate_gif(vr, r_subset, filename, fps = input$gif_fps, width = 950, height = 750)
#'       gif_file(gif_path)
#'     }, error = function(e) {
#'       showNotification(paste("GIF generation failed:", e$message), type = "error")
#'       gif_file(NULL)
#'     }, finally = {
#'       gif_generating(FALSE)
#'     })
#'   })
#'   
#'   
#'   # Download handler uses the generated GIF directly
#'   output$download_gif <- downloadHandler(
#'     filename = function() {
#'       paste0("map_animation_", Sys.Date(), ".gif")
#'     },
#'     content = function(file) {
#'       req(gif_file())
#'       file.copy(gif_file(), file, overwrite = TRUE)
#'     }
#'   )
#'   
#'   # Reactive values to store drawn point/polygon for export
#'   selected_geom <- reactiveVal(NULL)
#'   
#'   observeEvent(input$map_draw_new_feature, {
#'     selected_geom(input$map_draw_new_feature)
#'   })
#'   
#'   observeEvent(input$map_click, {
#'     if (input$selection_mode == "point") {
#'       click <- input$map_click
#'       if (!is.null(click)) {
#'         # Store point geometry as GeoJSON-like list
#'         selected_geom(list(
#'           type = "Feature",
#'           geometry = list(type = "Point", coordinates = c(click$lng, click$lat))
#'         ))
#'       }
#'     }
#'   })
#'   
#'   # ## CSV Preview: only generate when button pressed ----
#'   # csv_preview_data <- eventReactive(input$generate_csv_preview, {
#'   #   req(selected_geom())
#'   #   req(input$csv_variable)
#'   #   
#'   #   geom <- selected_geom()
#'   #   csv_preview_data <- eventReactive(input$csv_variable, {
#'   #     req(input$csv_variable, input$clicked_lat, input$clicked_lng)
#'   #     
#'   #     selected_var <- names(var_labels)[var_labels == input$csv_variable]
#'   #     req(length(selected_var) == 1)  # Ensure we found exactly one match
#'   #     
#'   #     variable_stack <- rast_list[[selected_var]]
#'   #     req(!is.null(variable_stack))  # Ensure stack is not NULL
#'   #     
#'   #     extract_point <- vect(data.frame(x = input$clicked_lng, y = input$clicked_lat),
#'   #                           crs = crs(variable_stack))
#'   #     values <- terra::extract(variable_stack, extract_point)[1, -1]
#'   #     dates <- as.Date(names(variable_stack))
#'   #     
#'   #     data.frame(Date = dates, Value = as.numeric(values))
#'   #   })
#'   #   
#'   #   if (is.null(geom)) return(NULL)
#'   #   
#'   #   if (geom$geometry$type == "Point") {
#'   #     coords <- unlist(geom$geometry$coordinates)
#'   #     sv <- terra::vect(matrix(coords, ncol = 2), type = "points", crs = crs(variable_stack))
#'   #     vals <- terra::extract(variable_stack, sv)
#'   #   } else if (geom$geometry$type == "Polygon") {
#'   #     poly <- sf::st_as_sf(geom)
#'   #     terra_poly <- terra::vect(poly)
#'   #     vals <- terra::extract(variable_stack, terra_poly, fun = mean, na.rm = TRUE)
#'   #   } else {
#'   #     vals <- NULL
#'   #   }
#'   #   
#'   #   if (!is.null(vals)) {
#'   #     # Convert Date columns to character (avoid charToDate warnings)
#'   #     vals[] <- lapply(vals, function(col) {
#'   #       if (inherits(col, "Date")) as.character(col) else col
#'   #     })
#'   #   }
#'   #   
#'   #   vals
#'   # })
#'   # 
#'   # output$csv_preview <- DT::renderDataTable({
#'   #   req(input$export_type == "csv")
#'   #   req(selected_geom())
#'   #   req(input$csv_variable)
#'   #   
#'   #   selected_var <- names(var_labels)[var_labels == input$csv_variable]
#'   #   req(length(selected_var) == 1)
#'   #   variable_stack <- rast_list[[selected_var]]
#'   #   req(!is.null(variable_stack))
#'   #   
#'   #   geom <- selected_geom()
#'   #   
#'   #   if (input$export_point_or_area == "Point") {
#'   #     coords <- unlist(geom$geometry$coordinates)
#'   #     sv <- terra::vect(matrix(coords, ncol = 2), type = "points", crs = crs(variable_stack))
#'   #     vals <- terra::extract(variable_stack, sv)
#'   #   } else if (input$export_point_or_area == "Polygon") {
#'   #     poly <- sf::st_as_sf(geom)
#'   #     terra_poly <- terra::vect(poly)
#'   #     vals <- terra::extract(variable_stack, terra_poly, fun = mean, na.rm = TRUE)
#'   #   }
#'   #   
#'   #   datatable(vals)
#'   # })
#'   
#'   csv_preview_data <- eventReactive(input$generate_csv_preview, {
#'     req(selected_geom())
#'     req(input$csv_variable)
#'     
#'     geom <- selected_geom()
#'     rast_obj <- rast_list[[input$csv_variable]]
#'     
#'     dates <- (names(rast_obj))
#'     
#'     validate(
#'       need(!is.null(rast_obj), "No raster found for selected variable.")
#'     )
#'     
#'     vals <- NULL
#'     
#'     # Handle points
#'     if (!is.null(geom$geometry$type) && geom$geometry$type == "Point") {
#'       coords <- unlist(geom$geometry$coordinates)
#'       sv <- terra::vect(matrix(coords, ncol = 2),
#'                         crs = crs(rast_obj))  # make a point SpatVector
#'       vals <- terra::extract(rast_obj, sv)
#'       
#'       # Handle polygons
#'     } else if (!is.null(geom$geometry$type) && geom$geometry$type == "Polygon") {
#'       poly_sf <- sf::st_as_sf(geom)   # turn GeoJSON list into sf object
#'       terra_poly <- terra::vect(poly_sf)
#'       vals <- terra::extract(rast_obj, terra_poly, fun = mean, na.rm = TRUE)
#'     }
#'     
#'     validate(
#'       need(!is.null(vals) && ncol(vals) > 1, "No values extracted for selected geometry.")
#'     )
#'     
#'     # Drop ID column
#'     vals <- vals[, -1, drop = FALSE]
#'     
#'     data.frame(
#'       Date = as.character(dates),
#'       Value = as.numeric(vals[1, ])
#'     )
#'   })
#'   
#'   # CSV Preview Output ----
#'   output$csv_preview <- renderDT({
#'     req(csv_preview_data())
#'     datatable(
#'       head(csv_preview_data(), 10),
#'       options = list(pageLength = 10, searching = FALSE, dom = 't')
#'     )
#'   })
#'   
#'   ## CSV Download UI ----
#'   output$csv_download_ui <- renderUI({
#'     req(csv_preview_data())
#'     downloadButton("download_csv", "Download CSV")
#'   })
#'   
#'   output$download_csv <- downloadHandler(
#'     filename = function() {
#'       paste0("data_export_", Sys.Date(), ".csv")
#'     },
#'     content = function(file) {
#'       vals <- csv_preview_data()
#'       req(vals)
#'       # Defensive: convert Date columns to character
#'       vals[] <- lapply(vals, function(col) {
#'         if (inherits(col, "Date")) as.character(col) else col
#'       })
#'       write.csv(vals, file, row.names = FALSE)
#'     }
#'   )
#'   
#' }
#' 
#' shinyApp(ui, server)
#' 
#' 
#' 
#' 
#' 
#' 
#' 
#' 

library(shiny)
library(leaflet)
library(tidyverse)
library(terra)
library(ncdf4)
library(sp)
library(raster)
library(sf)
library(bslib)
library(shinyWidgets)
library(base64enc)
library(leafem)
library(stars)
library(leaflet.extras)
library(rlang)
library(ggplot2)
library(shinyjqui)
library(plotly)
library(shinycssloaders)
library(shinyjs)
library(gifski)
library(png)
library(grDevices)
library(DT)
library(magick)
library(scico)
library(basemaps)


# Load processed data objects ----
data_objects <- readRDS("data/processed_data.Rds")
var_labels <- data_objects$var_labels
display_labels <- data_objects$display_labels
var_names <- data_objects$var_names
var_range_list <- data_objects$var_range_list


rast_list <- list()

for (var in var_names) {
  path <- file.path("data/processed_rasters", paste0(var, ".tif"))
  cat("Loading:", var, "\n")
  rast_list[[var]] <- rast(path)
}



# Load variable groups & palettes ----
source('R/var_groups_and_palettes.R')

# Load function for palette selection ----
source('R/get_palette_for_var.R')

# Set time sequence and labels ----
time_seq <- seq(as.Date("2014-01-01"), as.Date("2022-12-31"), by = "month")
time_labels <- format(time_seq, "%Y-%m")

# Helper infix for default values
`%||%` <- function(a, b) if (!is.null(a)) a else b




# 12/10 version, trying to fix map feature drawing ----
# UI -----
ui <- navbarPage(
  ## title ----
  title = div("California Carbon Explorer", style = "font-size: 22px; padding-left: 10px; padding-top: 12px; padding-bottom: 0px; color: #2f3714"),
  
  
  theme = bslib::bs_theme(
    heading_font = bslib::font_google("Tenor Sans")
  ),
  
  id = "main_navbar",
  position = "fixed-top",
  inverse = TRUE,
  collapsible = TRUE,
  header = tagList(
    tags$head(
      tags$script(src = "custom.js")
    ),
    tags$head(
      ## font, panels, navbar styling ----
      tags$style(HTML("
         @font-face {
          font-family: 'DMSans';
          src: url('DMSans-Variable.ttf') format('truetype');
          font-weight: 100 900;
          font-style: normal;
        }

        html, body {
          height: 100%;
          margin: 0;
          padding: 0;
          overflow: hidden;
        }

        body, input, button, select, textarea {
          font-family: 'DMSans', sans-serif;
          font-weight: 300;
          font-size: 14px;
          color: #333;
        }
        
         h2, h3, h4, h5, h6 {
          font-family: 'DMSans', sans-serif;
          font-weight: 600;
          font-size: 18px;
        }
        
        h1 {
          font-family: 'Tenor Sans', sans-serif;
        }

        .panel-well {
          padding: 10px;
          border-radius: 8px;
          box-shadow: 0 2px 4px rgba(0,0,0,0.2);
          font-weight: 300;
          overflow: hidden;
          background-color: #f4efe7;
        }
        
        .panel-legend {
          background-color: rgba(255, 255, 255, 0.0);
          box-shadow: none;
          border: none;
        }

        .legend-title {
          font-family: 'DMSans', sans-serif;
          font-weight: 510;
          font-size: 16px;
          margin-bottom: 4px;
          color: #333;
        }

        .legend-label {
          font-family: 'DMSans', sans-serif;
          font-weight: 300;
          font-size: 14px;
          color: #333;
        }
        
        .irs-single, .irs-bar, .irs-grid-text {
          font-size: 10px !important;
          font-family: 'DMSans', sans-serif;
        }

        .ui-resizable-handle.ui-resizable-sw {
          width: 20px;
          height: 20px;
          position: absolute;
          left: 0;
          bottom: 0;
          cursor: nesw-resize;
          background: url(\"data:image/svg+xml,%3Csvg xmlns='http://www.w3.org/2000/svg' width='20' height='20'%3E%3Cline x1='2' y1='14' x2='6' y2='18' stroke='rgba(0,0,0,0.3)' stroke-width='1'/%3E%3Cline x1='2' y1='8' x2='12' y2='18' stroke='rgba(0,0,0,0.3)' stroke-width='1'/%3E%3C/svg%3E\") no-repeat center center;
          background-size: 100% 100%;
        }

        .navbar, .container-fluid, .tab-content {
          margin: 0 !important;
          padding: 0 !important;
        }
        
        /* ========== NAVBAR OVERALL BACKGROUND ========== */
        .navbar {
          background-color: #f3e6d3 !important;   /* Navbar background color */
          border-bottom: 0px solid #c5d68d;       /* Bottom border/accent line */
        }

        /* ========== NAVBAR BRAND TITLE & TABS (RESTING STATE) ========== */
        .navbar .navbar-brand,
        .navbar-default .navbar-nav > li > a {
          color: #72654c !important;              /* Color of title and tab text when idle */
          font-weight: 500;                       /* Slight boldness */
        }

        /* ========== NAVBAR TABS ON HOVER ========== */
        .navbar-default .navbar-nav > li > a:hover {
          color: #C29A6D !important;              /* Text color when hovered */
             /* Background color when hovered */
        }

        /* ========== NAVBAR TABS WHEN ACTIVE (CURRENTLY SELECTED) ========== */
        /* This may not always be visible depending on whether you're using tabs inside navbarPage or tabsetPanel. */
        /* For tabs inside navbarPage, this is typically handled by Bootstrap nav-tabs styles instead. */
        .navbar-default .navbar-nav > .active > a,
        .navbar-default .navbar-nav > .active > a:focus,
        .navbar-default .navbar-nav > .active > a:hover {
          background-color: #c5d68d !important;   /* Background of selected tab */
          color: #9c7854 !important;              /* Text color of selected tab */
          border-bottom: 2px solid #c5d68d !important; /* Optional: bottom border accent for active tab */
        }

      /* === SLIMMER NAVBAR === */
        .navbar {
          min-height: 40px !important;   /* reduce total navbar height */
          padding-top: 0px !important;
          padding-bottom: 0px !important;
        }

        .navbar-nav > li > a {
          padding-top: 0px !important;
          padding-bottom: 0px !important;
        }

        /* === ALIGN NAVBAR TITLE LOWER === */
        .navbar .navbar-brand {
         padding-top: 2px !important;     /* lowers the title */
        padding-bottom: 8px !important;  /* optional, adds balance */
        line-height: 1.2 !important;     /* ensures text doesn’t stretch vertically */
        display: flex;
        align-items: flex-end;           /* align text to bottom of the bar */
      }
        #initial-loader {
          position: fixed;
          top: 0; left: 0;
          width: 100vw; height: 100vh;
          background-color: white;
          z-index: 99999;
          display: flex;
          align-items: center;
          justify-content: center;
        }

        .lds-dual-ring-initial:after {
          content: ' ';
          display: block;
          width: 64px;
          height: 64px;
          border-radius: 50%;
          border: 6px solid #244C20;
          border-color: #244C20 transparent #244C20 transparent;
          animation: lds-dual-ring 1.2s linear infinite;
        }
        
        /* === NAVBAR FONT OVERRIDE === */
        .navbar .navbar-brand,
        .navbar-nav > li > a {
          font-family: 'Tenor Sans', sans-serif !important;
          letter-spacing: 0.3px;
        }
        
        @keyframes lds-dual-ring {
          0% { transform: rotate(0deg); }
          100% { transform: rotate(360deg); }
        }
      ")),
      ## panel line spacing ----
      tags$style(HTML("
  .panel-well, .card, .panel, .card-body, .panel-body {
    line-height: 1.2;   /* default is ~1.5 */
  }
")),
      # offset for fixed-top navbar so tab content isn't hidden
      tags$style(HTML("#shiny-tab-Export,
#shiny-tab-Export > .container-fluid {
  padding-top: 65px !important;
}
")),
      ## loader ----
      tags$script(HTML("
        $(document).on('shiny:connected', function() {
          $('#initial-loader').fadeOut();
        });
      ")),
      ## draw toolbar ----
      tags$script(HTML("
  Shiny.addCustomMessageHandler('showSpinner', function(show) {
    var spinner = document.getElementById('map-spinner');
    if (spinner) {
      spinner.style.display = show ? 'block' : 'none';
    }
  });
  .leaflet-draw-toolbar {
          top: 30px !important;
          left: 10px !important;
          z-index: 1000 !important;
  }
        
")),
      ## info tabs styling ----
      tags$style(HTML("
  /* --- Info tab styling --- */
  .info-tab {
    background-color: #f8f1e7; /* lighter than #f3e6d3 */
    padding: 80px 80px;
    border-radius: 8px;
    max-width: 1200px;
    margin: 20px auto;
    line-height: 1.6;
    font-size: 17px;
    color: #2c2c2c;
  }

  .info-tab h2 {
    font-family: 'Tenor Sans', sans-serif;
    font-size: 25px;
    font-weight: 600;
    margin-bottom: 20px;
    color: #3b2d1f;
  }

  .info-tab p {
    font-family: 'Nanum Myeongjo', serif;
    margin-bottom: 20px;
  }

  /* Optional: a subtle shadow and border for separation */
  .info-tab {
    box-shadow: 0px 2px 6px rgba(0,0,0,0.1);
    border: 1px solid #e6d9c7;
  }
")),
      ## navbar dropdown menu ----
      tags$style(HTML("
  /* --- Match dropdown to navbar color --- */
  .navbar .dropdown-menu {
    background-color: #f3e6d3 !important; /* same as navbar */
    border: none;
    box-shadow: 0px 2px 6px rgba(0,0,0,0.15);
  }

  /* Dropdown items text and hover behavior */
  .navbar .dropdown-menu > li > a {
    color: #3b2d1f !important; /* dark brown text for readability */
    font-family: 'Tenor Sans', sans-serif;
    font-size: 14px;
    padding: 5px 10px;
  }

  .navbar .dropdown-menu > li > a:hover {
    background-color: #f8f1e7 !important; /* lighter tone on hover */
    color: #2a1d12 !important;
  }

  /* Optional: improve spacing around dropdown toggle */
  .navbar-nav > li > .dropdown-toggle {
    padding-bottom: 10px;
  }

  /* Optional: adjust caret (arrow) color */
  .navbar .dropdown-toggle::after {
    border-top-color: #3b2d1f !important;
  }
")),
      tags$link(rel = "stylesheet", href = "https://cdnjs.cloudflare.com/ajax/libs/leaflet.draw/1.0.4/leaflet.draw.css"),
      tags$script(src = "https://cdnjs.cloudflare.com/ajax/libs/leaflet.draw/1.0.4/leaflet.draw.js")
    )
  ),
  shinyjs::useShinyjs(),
  ## --- Map View Tab ----
  tabPanel(
    title = "Map View",
    tags$div(
      id = "map-container",
      style = "position: relative; height: 100vh; width: 100vw;",
      leafletOutput("map", height = "100%", width = "100%"),
      tags$div(
        id = "map-spinner",
        style = "position: absolute; top: 50%; left: 50%; transform: translate(-50%, -50%);
                 z-index: 9999; display: none;",
        tags$div(class = "lds-dual-ring-initial")
      )
    ),
    
    ### Controls panel ----
    absolutePanel(
      top = 60, left = 15, width = 325, bottom=367,
      class = "panel-well",
      h4("Controls"),
      selectInput("variable", "Select Variable:",
                  choices = names(var_labels),
                  selected = names(var_labels)[1]),
      radioButtons("selection_mode", "Selection Mode:",
                   choices = c("Point" = "point", "Area" = "area"),
                   selected = "point",
                   inline = TRUE),
      tags$p("Click a point or draw a box on the map to view time series."),
      div(style = "margin-bottom:0px;",
          fileInput("roi_upload", "Upload Shapefile (.zip)", accept = ".zip")
      ),
      div(style = "margin-top:0px; margin-bottom:0px;",
          actionButton("add_roi", "Add ROI to Map", class = "btn-primary")
      ),
      textOutput("date_text")
    ),
    
    ### Time slider -----
    absolutePanel(
      top = 465, bottom = 245, left = 15, width = 325,
      class = "panel-well",
      style = "padding-left: 20px",
      shinyWidgets::sliderTextInput(
        inputId = "time",
        label = "Select Month:",
        choices = time_labels,
        selected = time_labels[1],
        grid = FALSE,
        animate = animationOptions(interval = 1500, loop = TRUE),
        width = "100%"
      )
    ),
    
    ### Time series panel ----
    jqui_resizable(
      absolutePanel(
        top = 60, right = 15,
        class = "panel-well",
        style = "min-width: 300px; min-height: 200px; width: 410px; height: 300px; position: absolute;",
        h4("Time Series of Selected Pixel / Area", style = "text-align: center"),
        tags$div(
          style = "height: calc(100% - 35px); width: 100%;",
          plotlyOutput("ts_plot", height = "100%", width = "100%")
        )
      ),
      options = list(handles = "sw")
    ),
    
    ### Legend panel ----
    absolutePanel(
      bottom = 10, left = 15, width = 290,
      class = "panel-well panel-legend",
      uiOutput("custom_legend")
    )
  ),
  
  ## Export tab ----
  tabPanel(
    "Export",
    div(style = "padding-top: 50px;",
        sidebarLayout(
          
          # Sidebar
          sidebarPanel(
            h4("Export Options"),
            
            tags$p(
              style = "font-style: italic; color: #666;",
              "Note: Exporting a csv uses the point or area selected on the 'Map View' tab."
            ),
            
            # Export type selector
            radioButtons(
              "export_type", 
              "Export Type:",
              choices = c(
                "CSV (Point or Area)" = "csv",
                "GIF (Time Series Map)" = "gif"
              )
            ),
            
            # CSV options
            conditionalPanel(
              condition = "input.export_type == 'csv'",
              selectInput("csv_variable", "Variable:", choices = var_labels),
              selectInput("export_point_or_area", "Extract Type:", choices = c("Point", "Polygon")),
              actionButton("generate_csv_preview", "Generate Preview"),
              uiOutput("csv_download_ui")
            ),
            
            # GIF options
            conditionalPanel(
              condition = "input.export_type == 'gif'",
              selectInput("gif_variable", "Variable:", choices = var_labels),
              dateRangeInput(
                "gif_timerange", 
                "Date Range:",
                start = "2014-01-01", 
                end = "2022-12-01"
              ),
              sliderInput("gif_fps", "Frames per Second:", min = 1, max = 10, value = 3),
              actionButton("generate_gif", "Generate GIF Preview")
            )
          ),
          
          # Main panel for previews
          mainPanel(
            style = "margin-top: 20px;",
            
            # CSV preview container
            conditionalPanel(
              condition = "input.export_type == 'csv'",
              div(
                id = "csv_panel",
                DT::dataTableOutput("csv_preview")
              )
            ),
            
            # GIF preview container
            conditionalPanel(
              condition = "input.export_type == 'gif'",
              div(
                id = "gif_panel",
                style = "margin-top: 10px; text-align: center; padding: 0; width: 100%;",
                # Single UI output for everything
                uiOutput("gif_preview_block")
              )
            )
          )
        )
    )
  ),
  
  ## --- Other Pages ----
  navbarMenu("About this tool", 
             tabPanel("Overview", div(class = "info-tab",
                                      h2("Overview"),
                                      p("Page under construction...")
             )),
             tabPanel("Instructions", div(class = "info-tab",
                                          h2("Instructions"),
                                          p("Page under construction...")
             )),
             tabPanel("The CARDAMOM Framework", div(class = "info-tab", 
                                                    h2("The CARDAMOM Framework"), 
                                                    p("The terrestrial carbon cycle is complex, involving multiple “pools” and processes that transfer carbon through an ecosystem. 
                                                      While remote sensing techniques and field data collection can give us estimates of photosynthetic rates or biomass accumulation, it would be near impossible to measure the sizes and transfer rates of all ecosystem carbon pools. 
                                                      Mechanistic carbon models can provide insight into turnover and allocation rates and ecosystem fluxes, helping us to better understand the entirety of an ecosystem’s carbon cycle. 
                                                      The CARbon DAta MOdel FraMework (CARDAMOM; ) is a model-data assimilation framework that incorporates an ecosystem carbon model with meteorological and observational data to simulate carbon fluxes in an ecosystem of interest."), 
                                                    p("To understand the CARDAMOM framework, and model-data assimilation more generally, it can be helpful to start by visualizing a traditional “forward” ecosystem model. 
	                                                    This method would input “forcing” data, usually climatic variables, into a complex process-based model, with assigned parameter values that are informed by the region’s plant functional type or land cover class. 
	                                                    The output from this model would then be validated with an independent external dataset, usually with a large degree of mismatch."))),
             tabPanel("Variable Descriptions", div(class = "info-tab",
                                                   h2("Variable Info"),
                                                   p("Page under construction...")
             )),
             tabPanel("Model Information", div(class = "info-tab",
                                               h2("Model Info"),
                                               p("Page under construction...")
             ))
             
  ),
  
  # --- Initial Loading Spinner ---
  #tags$div(id = "initial-loader", tags$div(class = "lds-dual-ring-initial"))
)

# SERVER ----------------------
server <- function(input, output, session) {

  
  # JS helper: clear LeafletDraw items ----
  clear_drawings <- function() {
    session$sendCustomMessage("clearDrawnItems", list())
  }
  
  # Reactive value for clicked point ----
  clicked_point <- reactiveVal(NULL)
  
  # Enable resize----
  observe({
    invalidateLater(500, session)
    session$sendCustomMessage("enableResizable", TRUE)
  })

  # 1. MODE SWITCHING — point <-> area ----
  observeEvent(input$selection_mode, {
    proxy <- leafletProxy("map")
    
    # Reset clicked point
    clicked_point(NULL)
    
    # Clear any old markers or polygons
    proxy %>% clearGroup("clicked_point") %>% clearGroup("selected_area") %>%
      clearGroup("_drawnItems") %>%
      clearControls() %>%
      removeDrawToolbar()
    
    # Clear time series plot
    output$ts_plot <- renderPlotly(NULL)
    
    # Clear Leaflet.Draw internal layer
    clear_drawings()
    
    if (input$selection_mode == "point") {
      proxy %>% removeDrawToolbar() %>% clearGroup("selected_area") %>% clearGroup("_drawnItems")
    } else if (input$selection_mode == "area") {
      proxy %>% addDrawToolbar(
        targetGroup = "_drawnItems",
        position = "bottomleft",
        rectangleOptions = drawRectangleOptions(
          shapeOptions = drawShapeOptions(color = "#788554", weight = 2, fillOpacity = 0.2),
          showArea = TRUE
        ),
        polygonOptions = drawPolygonOptions(
          shapeOptions = drawShapeOptions(color = "#788554", weight = 2, fillOpacity = 0.2),
          showArea = TRUE
        ),
        polylineOptions = FALSE,
        markerOptions = FALSE,
        circleOptions = FALSE,
        circleMarkerOptions = FALSE,
        editOptions = editToolbarOptions(edit = TRUE)
      )
    }
  })
  
  # 2. POINT CLICK HANDLER ----
  observeEvent(input$map_click, {
    req(input$selection_mode == "point")
    
    lng <- input$map_click$lng
    lat <- input$map_click$lat
    clicked_point(c(lng, lat))
    
    # Clear old polygons and time series
    leafletProxy("map") %>% clearGroup("selected_area") %>% clearMarkers() %>%
      clearGroup("_drawnItems")
    clear_drawings()
    
    output$ts_plot <- renderPlotly(NULL)
    
    # Add clicked point
    leafletProxy("map") %>%
      clearGroup("clicked_point") %>%
      addCircleMarkers(lng, lat, radius = 4, color = "#244C20", fillOpacity = 1, group = "clicked_point")
    
    # Extract + plot time series
    vr <- varname()
    r_stack <- rast_list[[vr]]
    vals <- terra::extract(r_stack, matrix(c(lng, lat), ncol = 2))
    ts_vals <- unlist(vals[1, -1], use.names = FALSE)
    ts_vals <- ts_vals[1:length(time_seq)]
    if (length(ts_vals) < length(time_seq)) {
      ts_vals <- c(ts_vals, rep(NA, length(time_seq) - length(ts_vals)))
    } else if (length(ts_vals) > length(time_seq)) {
      ts_vals <- ts_vals[1:length(time_seq)]
    }

    df <- data.frame(Date = time_seq, Value = ts_vals)
    
    output$ts_plot <- renderPlotly({
      ggplotly(
        ggplot(df, aes(Date, Value)) +
          geom_line(color = "#788554") +
          geom_point(color = "#244C20", size = 0.8) +
          labs(y = display_labels[[vr]] %||% vr, x = "") +
          theme_minimal(10)
      )
    })
  })
  
  # 3. AREA DRAW HANDLER ----
  observeEvent(input$map_draw_new_feature, {
    req(input$selection_mode == "area")
    
    # Clear old polygons and time series
    leafletProxy("map") %>% clearGroup("selected_area")
    output$ts_plot <- renderPlotly(NULL)
    
    f <- input$map_draw_new_feature
    coords_raw <- f$geometry$coordinates[[1]]
    lng <- sapply(coords_raw, `[[`, 1)
    lat <- sapply(coords_raw, `[[`, 2)
    coords <- cbind(lng, lat)
    poly_sf <- sf::st_sf(geometry = sf::st_sfc(sf::st_polygon(list(coords)), crs = 4326))
    poly_vect <- terra::vect(poly_sf)
    
    # Add new polygon
    leafletProxy("map") %>%
      addPolygons(lng = lng, lat = lat, group = "selected_area",
                  color = "#788554", fillOpacity = 0.2, weight = 2)
    
    # Extract + plot time series
    vr <- varname()
    r_stack <- rast_list[[vr]]
    vals <- terra::extract(r_stack, poly_vect)
    ts_means <- colMeans(vals[, -1], na.rm = TRUE)
    ts_means <- ts_means[1:length(time_seq)]
    
    if (length(ts_means) < length(time_seq)) {
      ts_means <- c(ts_means, rep(NA, length(time_seq) - length(ts_means)))
    } else if (length(ts_means) > length(time_seq)) {
      ts_means <- ts_means[1:length(time_seq)]
    }
    df <- data.frame(Date = time_seq, Value = ts_means)
    
    output$ts_plot <- renderPlotly({
      ggplotly(
        ggplot(df, aes(Date, Value)) +
          geom_line(color = "#788554") +
          geom_point(color = "#244C20", size = 0.8) +
          labs(y = display_labels[[vr]] %||% vr, x = "") +
          theme_minimal(10)
      )
    })
  })
  
  
  
  
  
  
  
  # 2. VARIABLE SWITCHING — always clear selection ----
  observeEvent(input$variable, {
    
    # Reset selected point and clear plot
    clicked_point(NULL)
    output$ts_plot <- renderPlotly(NULL)
    
    # Clear map layers
    leafletProxy("map") %>%
      clearGroup("clicked_point") %>%
      clearGroup("selected_area")
    
    # Clear internal draw layer
    clear_drawings()
  })
  
  
  # --- Reactive value to store uploaded ROI 
  roi_data <- reactiveVal(NULL)
  
  varname <- reactive({
    var_labels[[input$variable]]
  })
  
  # When user uploads zipped shapefile
  observeEvent(input$roi_upload, {
    req(input$roi_upload)
    
    tmpdir <- tempdir()
    unzip(input$roi_upload$datapath, exdir = tmpdir)
    shp_file <- list.files(tmpdir, pattern = "\\.shp$", full.names = TRUE)
    
    if (length(shp_file) > 0) {
      roi_sf <- tryCatch(
        sf::st_read(shp_file, quiet = TRUE),
        error = function(e) NULL
      )
      roi_data(roi_sf)   # store in the reactiveVal
    }
  })
  
  
  
  ## pre-caching raster slices -----
  r_slices <- reactiveValues()
  observe({
    for (vr_name in names(rast_list)) {
      r_stack <- rast_list[[vr_name]]
      r_slices[[vr_name]] <- lapply(1:nlyr(r_stack), function(i) terra::subset(r_stack, i))
    }
  })
  
  ## render map -----
  output$map <- renderLeaflet({
    leaflet(options = leafletOptions(zoomControl = FALSE)) %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      fitBounds(lng1 = -128.4, lat1 = 35.5, lng2 = -115.1, lat2 = 41.5) %>%
      htmlwidgets::onRender("
        function(el, x) {
          var map = this;
          map._drawnItems = new L.FeatureGroup();
          map.addLayer(map._drawnItems);
          // create empty feature group for drawings
      }
      ") %>%
      htmlwidgets::onRender(
        "function(el, x) {
          L.control.zoom({position:'bottomright'}).addTo(this);
        }") %>%
      addScaleBar(position = "bottomright",   # add scale bar
                  options = scaleBarOptions(maxWidth = 100, 
                                            imperial = TRUE,
                                            updateWhenIdle = TRUE))
    
    
  })
  
  session$onFlushed(function() {
    session$sendCustomMessage("clearDrawnItems", list())
  })
  
  # Add ROI to map when button clicked
  observeEvent(input$add_roi, {
    req(roi_data())
    
    leafletProxy("map") %>%
      clearGroup("roi") %>%  # remove old ROI if present
      addPolygons(
        data = roi_data(),
        group = "roi",
        color = "red", weight = 2, fillOpacity = 0.2
      ) %>%
      addLayersControl(
        overlayGroups = c("roi"),
        options = layersControlOptions(collapsed = FALSE)
      )
  })
  
 
  # Show/hide spinner for Map View tab updates
  observeEvent({ input$main_navbar; input$variable; input$selection_mode; input$time }, {
    if (input$main_navbar != "Map View") return()  # only map tab
    
    session$sendCustomMessage("showSpinner", TRUE)  # show spinner immediately
    
    vr <- varname()
    selected_idx <- which(time_labels == input$time)
    r <- r_slices[[vr]][[selected_idx]]
    pal <- get_palette_for_var(vr, rast_list, var_groups, group_palettes, slice_rast = r)
    
    leafletProxy("map") %>%
      clearControls() %>%
      removeImage("raster") %>%
      addRasterImage(
        r,
        colors = pal,
        opacity = if (input$selection_mode == "area") 0.6 else 0.8,
        group = "raster",
        layerId = "raster"
      )
    
    later::later(function() {
      session$sendCustomMessage("showSpinner", FALSE)
    }, 0.5)
    
  })
  
  # Show spinner during GIF generation only
  # observeEvent(gif_generating(), {
  #   session$sendCustomMessage("showSpinner", gif_generating() && input$main_navbar == "Export")
  # })
  
  ## legend -----
  output$custom_legend <- renderUI({
    vr <- varname()
    var_range <- var_range_list[[vr]]
    pal <- get_palette_for_var(vr, rast_list, var_groups, group_palettes)
    n_colors <- 100
    vals <- seq(var_range[1], var_range[2], length.out = n_colors)
    colors <- pal(vals)
    gradient_css <- paste0("linear-gradient(to top, ", paste0(colors, collapse = ", "), ");")
    range_diff <- diff(var_range)
    digits_to_use <- if (range_diff < 10) 2 else 0
    label_vals <- pretty(var_range, n = 6)
    label_texts <- formatC(label_vals, format = "f", digits = digits_to_use, big.mark = ",")
    legend_title <- display_labels[[vr]] %||% input$variable
    
    tagList(
      tags$div(class = "legend-title", legend_title),
      tags$div(style = "display: flex; align-items: center;",
               tags$div(style = paste0("height: 180px; width: 35px; background: ", gradient_css, 
                                       " border: 1px solid #000; margin-right: 0px; position: relative;")),
               tags$div(style = "height: 180px; display: flex; flex-direction: column; justify-content: space-between;",
                        lapply(rev(label_texts), function(txt) {
                          tags$div(style = "display: flex; align-items: center;",
                                   tags$div(style="width:5px; height:2px; background:black; margin-right:2px;"),
                                   tags$div(class = "legend-label", txt))
                        }))
      )
    )
  })
  
  
 
  
  
  ## GIF related -----
  gif_file <- reactiveVal(NULL)
  gif_generating <- reactiveVal(FALSE)
  
  # Reactive to indicate whether a GIF exists
  output$gif_file <- reactive({
    !is.null(gif_file())
  })
  outputOptions(output, "gif_file", suspendWhenHidden = FALSE)
  
  output$gif_preview_block <- renderUI({
    req(input$export_type == "gif")
    
    if (gif_generating()) {
      # Only status message while generating
      tags$div(
        style = "color: #244C20; font-weight: bold;",
        "Generating GIF, please wait..."
      )
    } else
      
      if (!is.null(gif_file())) {
        # Status message + GIF preview + download button
        tags$div(
          tags$img(
            src = base64enc::dataURI(file = gif_file(), mime = "image/gif"),
            style = "max-width: 100%; height: auto; border: none; display: block; margin-top:0px; margin-bottom:0px"
          ),
          tags$div(
            style = "margin-top: 2px;",
            downloadButton("download_gif", "Download GIF", style = "display: inline-block;")
          )
        )
      } 
    
    else {
      # Default message
      tags$p("GIF preview will generate below...")
    }
  })
  
  
  # gif palette fxn
  gif_get_palette_for_var <- function(var_name, rast_list, var_groups, group_palettes, slice_rast = NULL) {
    
    group <- var_groups[[var_name]]
    pal_name <- group_palettes[[group]]
    
    # Compute global min/max across all time
    var_stack <- rast_list[[var_name]]
    
    var_range <- terra::global(var_stack, fun = range, na.rm = TRUE)
    
    
    vr_min <- min(var_range$X1, na.rm=TRUE)
    vr_max <- max(var_range$X2, na.rm=TRUE)
    domain <- c(vr_min, vr_max)
    
    
    # Use viridisLite or RColorBrewer or scico depending on palette
    viridis_opts <- c("viridis", "magma", "plasma", "inferno", "cividis", "turbo", "mako", "rocket")
    reverse_pal_groups <- c("Foliage")
    if (pal_name %in% viridis_opts) {
      cols <- viridisLite::viridis(100, option = pal_name)
    } else {
      if (group %in% reverse_pal_groups) {
        cols <- scico(100, direction = -1, palette = pal_name)
      } 
      
      else {
        #cols <- colorRampPalette(RColorBrewer::brewer.pal(9, pal_name))(100)
        cols <- scico(100, palette = pal_name)
      }
      
    }
    return(cols)
  }
  
  # Compute global min/max across all frames
  
  
  # GIF generator function
  generate_gif <- function(vr, rast_subset, filename, fps = 5, width = 950, height = 750) {
    temp_dir <- tempdir()
    png_paths <- file.path(temp_dir, sprintf("frame_%03d.png", seq_len(nlyr(rast_subset))))
    
    # Ensure width/height are integers
    width <- as.integer(width)
    height <- as.integer(height)
    
    # Create palette
    pal_fun <- gif_get_palette_for_var(vr, rast_list, var_groups, group_palettes)
    
    var_range <- terra::global(rast_subset, fun = range, na.rm = TRUE)
    vr_min <- min(var_range$X1, na.rm=TRUE)
    vr_max <- max(var_range$X2, na.rm=TRUE)
    domain <- c(vr_min, vr_max)
    
    rast_subset_p <- terra::project(rast_subset, "EPSG:3857")
    projected_ext <- st_bbox(rast_subset_p, crs=st_crs(3857))
    
    # Define the buffer distance
    r <- 80000 # 10,000 meters
    
    bbox_expanded <- projected_ext + c(-r, -r, -60000, r)
    
    basemap <- basemap_terra(bbox_expanded, map_service="carto", map_type="voyager")
    
    group <- var_groups[[vr]]
    if (group == "Fire") {
      pal_fun 
      for (i in seq_len(nlyr(rast_subset_p))) {
        png(png_paths[i], width = width, height = height)
        
        # Plot basemap
        plot(basemap, 
             main = paste("Date:", names(rast_subset)[i]), box=FALSE, mar=c(3, 3, 6, 2), axes=FALSE)
        
        # Make a copy of this slice
        #slice <- rast_subset_p[[i]]
        
        # Mask zeros for plotting, but only temporarily
        #slice[slice == 0] <- NA
        
        #plot(rast_subset_p[[i]] == 0, col = "#000000", legend = FALSE, add=TRUE)
        # Plot non-zero values with palette
        plot(rast_subset_p[[i]], col = pal_fun, range = domain, type="continuous", 
             plg=list(title = display_labels[[vr]]), axes=FALSE, legend=TRUE, add=TRUE)
        
        # Overlay zeros in black
        
        
        dev.off()
      }
    }
    
    else {
      for (i in seq_len(nlyr(rast_subset_p))) {
        png(png_paths[i], width = width, height = height)
        
        # Plot basemap first 
        plot(basemap, 
             main = paste("Date:", names(rast_subset)[i]), box=FALSE, mar=c(3, 3, 6, 2), axes=FALSE)
        
        # Plot raster frame with consistent legend
        plot(rast_subset_p[[i]], col = pal_fun, range = domain, 
             plg=list(title = display_labels[[vr]]), add = TRUE, axes=FALSE)
        
        dev.off()
      }
    }
    
    # Ensure frames exist
    if (!all(file.exists(png_paths))) {
      stop("Some PNG frames were not created properly.")
    }
    
    # Generate GIF
    ok <- tryCatch({
      gifski::gifski(
        png_paths,
        gif_file  = filename,
        width     = width,
        height    = height,
        delay     = 1 / fps
      )
      TRUE
    }, error = function(e) {
      message("GIF generation failed: ", e$message)
      FALSE
    })
    
    if (!ok) stop("GIF generation failed.")
    
    # Cleanup PNG frames
    unlink(png_paths)
    
    return(filename)
  }
  
  # Observe event for Generate GIF button
  observeEvent(input$generate_gif, {
    req(input$gif_variable)
    req(input$gif_timerange)
    req(length(input$gif_timerange) == 2)
    
    # Validate date format
    if (any(is.na(as.Date(input$gif_timerange, format = "%Y-%m-%d")))) {
      showNotification("Invalid date format", type = "error")
      return()
    }
    
    gif_generating(TRUE)
    gif_file(NULL)
    
    vr <- input$gif_variable
    r_stack <- rast_list[[vr]]
    
    rast_dates <- tryCatch({
      as.Date(paste0(as.character(names(r_stack)), "-01"), format = "%Y-%m-%d")
    }, error = function(e) {
      showNotification("Error parsing raster dates.", type = "error")
      return(NULL)
    })
    
    req(!is.null(rast_dates))
    
    start_date <- as.Date(input$gif_timerange[1])
    end_date <- as.Date(input$gif_timerange[2])
    
    keep_idx <- which(rast_dates >= start_date & rast_dates <= end_date)
    if (length(keep_idx) == 0) {
      showNotification("No layers found for the selected date range.", type = "error")
      gif_generating(FALSE)
      return(NULL)
    }
    
    r_subset <- terra::subset(r_stack, keep_idx)
    filename <- file.path(tempdir(), paste0("map_animation_", Sys.Date(), ".gif"))
    
    tryCatch({
      gif_path <- generate_gif(vr, r_subset, filename, fps = input$gif_fps, width = 950, height = 750)
      gif_file(gif_path)
    }, error = function(e) {
      showNotification(paste("GIF generation failed:", e$message), type = "error")
      gif_file(NULL)
    }, finally = {
      gif_generating(FALSE)
    })
  })
  
  
  # Download handler uses the generated GIF directly
  output$download_gif <- downloadHandler(
    filename = function() {
      paste0("map_animation_", Sys.Date(), ".gif")
    },
    content = function(file) {
      req(gif_file())
      file.copy(gif_file(), file, overwrite = TRUE)
    }
  )
  
  # Reactive values to store drawn point/polygon for export
  selected_geom <- reactiveVal(NULL)
  
  observeEvent(input$map_draw_new_feature, {
    selected_geom(input$map_draw_new_feature)
  })
  
  observeEvent(input$map_click, {
    if (input$selection_mode == "point") {
      click <- input$map_click
      if (!is.null(click)) {
        # Store point geometry as GeoJSON-like list
        selected_geom(list(
          type = "Feature",
          geometry = list(type = "Point", coordinates = c(click$lng, click$lat))
        ))
      }
    }
  })
  
 
  csv_preview_data <- eventReactive(input$generate_csv_preview, {
    req(selected_geom())
    req(input$csv_variable)
    
    geom <- selected_geom()
    rast_obj <- rast_list[[input$csv_variable]]
    
    dates <- (names(rast_obj))
    
    validate(
      need(!is.null(rast_obj), "No raster found for selected variable.")
    )
    
    vals <- NULL
    
    # Handle points
    if (!is.null(geom$geometry$type) && geom$geometry$type == "Point") {
      coords <- unlist(geom$geometry$coordinates)
      sv <- terra::vect(matrix(coords, ncol = 2),
                        crs = crs(rast_obj))  # make a point SpatVector
      vals <- terra::extract(rast_obj, sv)
      
      # Handle polygons
    } else if (!is.null(geom$geometry$type) && geom$geometry$type == "Polygon") {
      poly_sf <- sf::st_as_sf(geom)   # turn GeoJSON list into sf object
      terra_poly <- terra::vect(poly_sf)
      vals <- terra::extract(rast_obj, terra_poly, fun = mean, na.rm = TRUE)
    }
    
    validate(
      need(!is.null(vals) && ncol(vals) > 1, "No values extracted for selected geometry.")
    )
    
    # Drop ID column
    vals <- vals[, -1, drop = FALSE]
    
    data.frame(
      Date = as.character(dates),
      Value = as.numeric(vals[1, ])
    )
  })
  
  # CSV Preview Output ----
  output$csv_preview <- renderDT({
    req(csv_preview_data())
    datatable(
      head(csv_preview_data(), 10),
      options = list(pageLength = 10, searching = FALSE, dom = 't')
    )
  })
  
  ## CSV Download UI ----
  output$csv_download_ui <- renderUI({
    req(csv_preview_data())
    downloadButton("download_csv", "Download CSV")
  })
  
  output$download_csv <- downloadHandler(
    filename = function() {
      paste0("data_export_", Sys.Date(), ".csv")
    },
    content = function(file) {
      vals <- csv_preview_data()
      req(vals)
      # Defensive: convert Date columns to character
      vals[] <- lapply(vals, function(col) {
        if (inherits(col, "Date")) as.character(col) else col
      })
      write.csv(vals, file, row.names = FALSE)
    }
  )
  
}

shinyApp(ui, server)

