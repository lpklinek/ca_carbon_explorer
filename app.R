# Load packages
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

# empty list
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


addResourcePath("images", "www")

# v02.19.26, beta ----
# UI -----
ui <- navbarPage(
  ## title ----
  title = div("California Carbon Explorer", style = "font-size: 22px; padding-left: 10px; padding-top: 12px; padding-bottom: 0px; color: #2f3714"),
  ## heading font ----
  theme = bslib::bs_theme(
    heading_font = bslib::font_google("Tenor Sans")
  ),
  ## creating navbar ----
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
          overflow-x: hidden;   /* prevent sideways scroll */
          overflow-y: auto;     /* allow vertical scroll */
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
        
        /* ----- navbar background color ----- */
        .navbar {
          background-color: #f3e6d3 !important;   /* background color */
          border-bottom: 0px solid #c5d68d;       /* bottom line */
        }

        /* ----- navbar title, resting state (when not moused over) -----  */
        .navbar .navbar-brand,
        .navbar-default .navbar-nav > li > a {
          color: #72654c !important;             
          font-weight: 500;                      
        }

        /* ----- tabs when moused over ----- */
        .navbar-default .navbar-nav > li > a:hover {
          color: #C29A6D !important;              
             
        }

        /* ----- tabs when currently clicked/selected ----- */
        
        .navbar-default .navbar-nav > .active > a,
        .navbar-default .navbar-nav > .active > a:focus,
        .navbar-default .navbar-nav > .active > a:hover {
          background-color: #c5d68d !important;   /* background of selected tab */
          color: #9c7854 !important;              
          border-bottom: 2px solid #c5d68d !important; 
        }

      /* ----- navbar dimensions ----- */
        .navbar {
          min-height: 40px !important;   /* reduce total navbar height */
          padding-top: 0px !important;
          padding-bottom: 0px !important;
        }

        .navbar-nav > li > a {
          padding-top: 0px !important;
          padding-bottom: 0px !important;
        }

        /* ----- padding/aligning the navbar title ----- */
        .navbar .navbar-brand {
         padding-top: 2px !important;     /* lower title */
        padding-bottom: 8px !important;  
        line-height: 1.2 !important;     
        display: flex;
        align-items: flex-end;           /* align text to bottom of bar */
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
        
        .disabled-section {
  opacity: 0.4;
  pointer-events: none;
}
        
        /* ----- ensuring navbar font ----- */
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
    background-color: #f8f1e7;
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
  
  .info-tab h3 {
    font-family: 'Tenor Sans', sans-serif;
    font-size: 19px;
    font-weight: 500;
    margin-bottom: 15px;
    color: #3b2d1f;
  }

  /* .info-tab p {
    font-family: 'Nanum Myeongjo', serif;
    margin-bottom: 20px;
  } */

  /* adding shadow / border */
  .info-tab {
    box-shadow: 0px 2px 6px rgba(0,0,0,0.1);
    border: 1px solid #e6d9c7;
  }
  
  /* --- Scrollable Info Tab --- */
.info-tab-scroll {
  background-color: #f8f1e7;
  padding: 90px 80px 60px 80px;  /* extra top padding for fixed navbar */
  border-radius: 8px;
  max-width: 1200px;
  margin: 40px auto;
  line-height: 1.6;
  font-size: 17px;
  color: #2c2c2c;
  box-shadow: 0px 2px 6px rgba(0,0,0,0.1);
  border: 1px solid #e6d9c7;
  min-height: auto;
}
  .info-tab-scroll h2 {
    font-family: 'Tenor Sans', sans-serif;
    font-size: 25px;
    font-weight: 600;
    margin-bottom: 20px;
    color: #3b2d1f;
  }
  
  /* --- Info Page Table Styling --- */
.info-table {
  width: 100%;
  border-collapse: collapse;
  margin-top: 20px;
  margin-bottom: 40px;
  font-size: 16px;
}

.info-table th {
  text-align: left;
  padding: 10px;
  background-color: #e6d9c7;
  border-bottom: 2px solid #d4c3af;
}

.info-table td {
  padding: 10px;
  border-bottom: 1px solid #e6d9c7;
  vertical-align: top;
}

.info-table tr:hover {
  background-color: #f3ebe2;
}
")),
      ## navbar dropdown menu ----
      tags$style(HTML("
  /* ----- match dropdown to navbar color ----- */
  .navbar .dropdown-menu {
    background-color: #f3e6d3 !important; /* same as navbar */
    border: none;
    box-shadow: 0px 2px 6px rgba(0,0,0,0.15);
  }

  /* ----- dropdown items text and hover behavior ----- */
  .navbar .dropdown-menu > li > a {
    color: #3b2d1f !important; /* dark brown text */
    font-family: 'Tenor Sans', sans-serif;
    font-size: 14px;
    padding: 5px 10px;
  }

  .navbar .dropdown-menu > li > a:hover {
    background-color: #f8f1e7 !important; /* lighter color when hovered */
    color: #2a1d12 !important;
  }

  .navbar-nav > li > .dropdown-toggle {
    padding-bottom: 10px;
  }

  .navbar .dropdown-toggle::after {
    border-top-color: #3b2d1f !important;
  }
")),
      tags$link(rel = "stylesheet", href = "https://cdnjs.cloudflare.com/ajax/libs/leaflet.draw/1.0.4/leaflet.draw.css"),
      tags$script(src = "https://cdnjs.cloudflare.com/ajax/libs/leaflet.draw/1.0.4/leaflet.draw.js")
    )
  ),
  shinyjs::useShinyjs(),
  ## Map View Tab ----
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
    
    ### controls panel ----
    absolutePanel(
      top = 60, left = 15, width = 355, bottom=367,
      class = "panel-well",
      h4("Controls",style = "text-align: center"),
      selectInput("variable", "Select Variable:",
                  choices = names(var_labels),
                  selected = names(var_labels)[1]),
      #radioButtons("selection_mode", "Selection Mode:",
                   #choices = c("Point" = "point", "Area" = "area"),
                   #selected = "point",
                   #inline = TRUE),
      radioButtons("selection_mode", "Selection Mode:",
                   choices = c("Point" = "point",
                               "Drawn Area" = "drawn_area",
                               "Uploaded Area" = "uploaded_area"),
                   selected = "point",
                   inline = TRUE),
      tags$p("Click a point or draw a box on the map to view time series."),
      # div(style = "margin-bottom:0px;",
      #     fileInput("roi_upload", "Upload Shapefile (.zip)", accept = ".zip")
      # ),
      # div(style = "margin-top:0px; margin-bottom:0px;",
      #     actionButton("add_roi", "Add ROI to Map", class = "btn-primary")
      # ),
      div(id = "roi_section",
          fileInput("roi_upload", "Upload Shapefile (.zip)", accept = ".zip"),
          actionButton("add_roi", "Add ROI to Map", class = "btn-primary")
      ),
      textOutput("date_text")
    ),
    
    ### time slider -----
    absolutePanel(
      top = 465, bottom = 245, left = 15, width = 355,
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
    
    ### time series panel/plot ----
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
    
    ### legend panel ----
    absolutePanel(
      bottom = 10, left = 15, width = 290,
      class = "panel-well panel-legend",
      uiOutput("custom_legend")
    )
  ),
  # Export Data tab
  tabPanel(
    "Export Data",
    div(
      style = "padding: 70px 30px 30px 30px; background-color: #f8f9fa; min-height: 100vh;",
      
      div(
        class = "panel panel-default",
        style = "padding: 20px; background-color: white; border-radius: 6px;",
    sidebarLayout(
      
      sidebarPanel(
        width = 3,
        
        h3("Export Data"),
        
        tags$p(
          style = "font-size: 13px; color: #666;",
          "Exports a CSV using the current selection from the Map View tab."
        ),
        
        tags$hr(),
        
        selectInput(
          "csv_variable",
          "Variable",
          choices = var_labels
        ),
        
        actionButton(
          "generate_csv_preview",
          "Generate Preview",
          class = "btn-primary"
        ),
        
        br(), br(),
        
        uiOutput("csv_download_ui")
      ),
      
      mainPanel(
        width = 9,
        
        h4("Preview"),
        
        DT::dataTableOutput("csv_preview")
      )
    )
      )
    )
  ),
  
  
  
  tabPanel(
    "Generate GIF",
    div(
      style = "padding: 70px 30px 30px 30px; background-color: #f8f9fa; min-height: 100vh;",
      div(
        class = "panel panel-default",
        style = "padding: 20px; background-color: white; border-radius: 6px;",
    sidebarLayout(
      
      sidebarPanel(
        width = 3,
        
        h3("Generate GIF"),
        
        tags$hr(),
        
        selectInput(
          "gif_variable",
          "Variable:",
          choices = var_labels
        ),
        
        dateRangeInput(
          "gif_timerange",
          "Date Range:",
          start = "2014-01-01",
          end = "2022-12-01"
        ),
        
        sliderInput(
          "gif_fps",
          "Frames per Second:",
          min = 1,
          max = 10,
          value = 3
        ),
        
        actionButton(
          "generate_gif",
          "Generate GIF Preview"
        )
      ),
      
      mainPanel(
        width = 9,
        
        #### GIF preview container ----
        div(
          id = "gif_panel",
          style = "margin-top: 10px; text-align: center; padding: 0; width: 100%;",
          
          # single UI output for everything
          uiOutput("gif_preview_block")
        )
      )
    )
      )
    )
    
  ),
  
  
  ## Info Pages ----
  navbarMenu("About this tool", 
             tabPanel("Overview", div(class = "info-tab",
                                      h2("Overview"),
                                      p("Welcome to the California Carbon Explorer! This is an open-source tool for quantifying and visualizing the forest carbon cycle in California, and understanding the resilience and vulnerability of forested ecosystems. 
                                      We hope that scientists and land managers, from government to non-profit, private, or tribal, can use this platform to monitor changes over time and ultimately inform adaptive management strategies to increase ecosystem health and carbon storage."), 
	                                    p("This platform was coded and designed by Lily Klinek. The development of this tool was funded by the CAL FIRE Forest Health Research Program grant (#8GG20808) awarded to PI Troy Magney. The platform was built using R Shiny, and used data from a CARDAMOM model run completed by Jessie Au, Luke Smallman, and David Milodowski. 
	                                    All source code for the current beta version of the platform can be found on the following github page: ")
             )),
             tabPanel("Instructions", div(class = "info-tab",
                                          h2("Instructions"),
                                          h3("Map View"),
                                          p("The “Map” tab of the dashboard can be used to explore visualizations of the model output layers, and to plot time series of any output variable for points or regions of interest. The toolbar on the left-hand side of the page can be used to select an output variable (see “Variable Descriptions” tab for more information), and the slider bar can be used to select a timestamp to display on the map (between January 2014 and December 2022 on a monthly resolution)."
                                            ),
                                          br(),
                                          p("When “Point” mode is selected, users can click on a single pixel on the map to view a time series plot for that point. Users can mouse over the plot to view values for each plotted point. When “Drawn Area” mode is selected, users can draw a polygon of interest on the map using the “Draw” toolbar."
                                            ),
                                          br(),
                                          p("When “Uploaded Area” mode is selected, users can upload a .zip folder (must include a .shp file) of their region of interest (ROI)."
                                            ),
                                          br(),
                                          h3("Export Data"),
                                          p("The “Export Data” tab of the dashboard can be used to download .csv files for the variable and point or area of interest that has been selected on the “Map” tab."
                                            ),
                                          br(),
                                          h3("Generate GIF"),
                                          p("The “Generate GIF” tab can be used to generate and download a time-series animation of any variable for the entire state."
                                            ),
                                          br(),
                                          br(),
                                          p("The app is currently in beta mode as we work to test and debug all functionalities. Please reach out to us at lpklinek@ucdavis.edu if you run into any issues while using the dashboard, or if you have any feedback you’d like to share! We’d love to hear from you."
                                            )
             )),
             tabPanel("The CARDAMOM Framework", div(class = "info-tab-scroll",
                                                    
                                                    h2("The CARDAMOM Framework"), 
                                                    p("The terrestrial carbon cycle is complex, involving multiple “pools” and processes that transfer carbon through an ecosystem. 
                                                      While remote sensing techniques and field data collection can give us estimates of photosynthetic rates or biomass accumulation, it would be near impossible to measure the sizes and transfer rates of all ecosystem carbon pools. 
                                                      Mechanistic carbon models can provide insight into turnover and allocation rates and ecosystem fluxes, helping us to better understand the entirety of an ecosystem’s carbon cycle. 
                                                      The CARbon DAta MOdel FraMework (CARDAMOM) is a model-data assimilation framework that incorporates an ecosystem carbon model with meteorological and observational data to simulate carbon fluxes in an ecosystem of interest."
                                                      ), 
                                                    p("To understand the CARDAMOM framework, and model-data assimilation more generally, it can be helpful to start by visualizing a traditional “forward” ecosystem model. 
	                                                    This method would input “forcing” data, usually climatic variables, into a complex process-based model, with assigned parameter values that are informed by the region’s plant functional type or land cover class. 
	                                                    The output from this model would then be validated with an independent external dataset, usually with a large degree of mismatch."
                                                      ),
                                                    tags$img(
                                                      src = "images/cardamom_figure1.png",
                                                      style = "max-width: 100%; height: auto; display: block; margin: 20px auto;"
                                                    ),
                                                    br(),
                                                    p("Model-data assimilation, on the other hand, uses more of an iterative approach. Forcing data is fed into a simple, process-based model that has user assigned “priors,” or starting parameter values, and accompanying uncertainty estimates. 
                                                      The modeled output, with uncertainty estimates, is then compared to observational data, and a data assimilation algorithm then works to tweak the parameters of the process model so that the modeled output best matches our observations."
                                                      ),
                                                    tags$img(
                                                      src = "images/cardamom_figure2.png",
                                                      style = "max-width: 100%; height: auto; display: block; margin: 20px auto;"
                                                    ),
                                                    p("CARDAMOM combines the mechanistic carbon cycle model DALEC (Data Assimilation Linked Ecosystem Carbon) with parameter priors and climate and disturbance forcing. The model parameters are then optimized and constrained from their initial values using Markov Chain Monte-Carlo methods. 
                                                    The MCMC approach searches parameter space to find parameter sets that produce outputs that are consistent with observations and observational error. The approach, for each parameter set, assesses the likelihood that model output and the dataset are consistent. Then that likelihood is stored with each parameter set. 
                                                    The search process ends when the likelihoods have converged, yielding a group of accepted parameter sets. CARDAMOM runs end with parameter posteriors and model analyses with uncertainty.
                                                    "),
                                                    p("DALEC is a pool-based, mass balance model of the terrestrial C cycle, of intermediate complexity. It can be run from daily to monthly time steps, and at a broad range of spatial scales. GPP is determined using an aggregated canopy model. 
                                                      Then the modeled GPP is allocated to seven pools. Pools include labile C, foliar C, root C, wood C, litter C, and SOM C. Losses of C from the ecosystem include autotrophic respiration, heterotrophic respiration, and fire or harvest. 
                                                      DALEC requires 33 unknown parameters, which include initial conditions for each pool."
                                                      ),
                                                    p("Ecological Dynamic Constraints are used in CARDAMOM to maintain simple ecological rules and known relationships. They act as sanity checks, imposing realistic conditions on the model and reducing uncertainty."
                                                      ),
                                                    p("In comparison to other similar terrestrial ecosystem model-data frameworks, CARDAMOM is unique in that it does not use the concept of plant functional types (PFTs) to assign parameters. 
                                                    While other ecosystem models might group, for example, all evergreen coniferous forests together, and assign one parameter set for all ecosystems in this category, CARDAMOM instead determines each pixel’s parameter set independently using only the driving and observational data available for that pixel. 
                                                    Parameters are thus able to vary spatially along natural climate gradients. Additionally, CARDAMOM allows for an uncertainty characterization by producing simulation ensembles as opposed to single simulations. 
                                                    Gaps in available constraining data are represented in model output by wider bounds in the parameter posterior, thus ensuring that model output reflects uncertainty or sparsity in model input data."
                                                      ),
                                                    p("As a model-data assimilation framework, CARDAMOM combines the strengths of both direct measurement and mechanistic modeling. It follows theory, reflecting our best scientific understanding of the carbon cycle. 
                                                      It provides a spatially and temporally continuous analysis. And, importantly, it allows for precise quantifications of uncertainty, both in model parameters and output."
                                                      ),
                                                    br(),
                                                    br(),
                                                    p("For further information on CARDAMOM, DALEC, and model-data assimilation, please refer to the following resources:"
                                                      ),
                                                    br(),
                                                    p("Bloom, A. A., & Williams, M. (2015). Constraining ecosystem carbon dynamics in a data-limited world: Integrating ecological “common sense” in a model–data fusion framework. Biogeosciences, 12(5), 1299–1315.",
                                                      tags$a(
                                                      href = "https://doi.org/10.5194/bg-12-1299-2015",
                                                      "https://doi.org/10.5194/bg-12-1299-2015",
                                                      target = "_blank"
                                                    )
                                                      ),
                                                    p("Smallman, T. L., Exbrayat, J.-F., Mencuccini, M., Bloom, A. A., & Williams, M. (2017). Assimilation of repeated woody biomass observations constrains decadal ecosystem carbon cycle uncertainty in aggrading forests. Journal of Geophysical Research: Biogeosciences, 122(3), 528–545.",
                                                      tags$a(
                                                        href = "https://doi.org/10.1002/2016JG003520",
                                                        "https://doi.org/10.1002/2016JG003520",
                                                        target = "_blank"
                                                      )),
                                                    p("Smallman, T. L., Milodowski, D. T., & Williams, M. (2022). From Ecosystem Observation to Environmental Decision-Making: Model-Data Fusion as an Operational Tool. Frontiers in Forests and Global Change, 4. https://doi.org/10.3389/ffgc.2021.818661",
                                                      tags$a(
                                                        href = "https://doi.org/10.3389/ffgc.2021.818661",
                                                        "https://doi.org/10.3389/ffgc.2021.818661",
                                                        target = "_blank"
                                                      )),
                                                    p("Williams, M. (2022). Global Carbon Cycle Data Assimilation Using Earth Observation: The CARDAMOM Approach. In Land Carbon Cycle Modeling. CRC Press. https://doi.org/10.1201/9780429155659-34",
                                                      tags$a(
                                                        href = "https://doi.org/10.1201/9780429155659-34",
                                                        "https://doi.org/10.1201/9780429155659-34",
                                                        target = "_blank"
                                                      )),
                                                    p("Worden, M. A., Bilir, T. E., Bloom, A. A., Fang, J., Klinek, L. P., Konings, A. G., Levine, P. A., Milodowski, D. T., Quetin, G. R., Smallman, T. L., Bar‐On, Y. M., Braghiere, R. K., David, C. H., Fischer, N. A., Gentine, P., Green, T. J., Jones, A., Liu, J., Longo, M., … Zhu, S. (2025). Combining Observations and Models: A Review of the CARDAMOM Framework for Data‐Constrained Terrestrial Ecosystem Modeling. Global Change Biology, 31(8), e70462.",
                                                      tags$a(
                                                        href = "https://doi.org/10.1111/gcb.70462",
                                                        "https://doi.org/10.1111/gcb.70462",
                                                        target = "_blank"
                                                      )),
                                                    )),
             tabPanel("Variable Descriptions", div(class = "info-tab",
                                                   h2("Variable Descriptions"),
                                                   tags$table(
                                                     class = "info-table",
                                                     
                                                     tags$thead(
                                                       tags$tr(
                                                         tags$th("Variable"),
                                                         tags$th("Description"),
                                                         tags$th("Units")
                                                       )
                                                     ),
                                                     
                                                     tags$tbody(
                                                       
                                                       # ---- Carbon Stocks (gC/m²) ----
                                                       tags$tr(tags$td("Total Ecosystem C Stock"), tags$td("Sum of all ecosystem carbon pools."), tags$td("gC/m²")),
                                                       tags$tr(tags$td("Soil Organic Matter C"), tags$td("Carbon stored in stabilized soil organic matter."), tags$td("gC/m²")),
                                                       tags$tr(tags$td("Dead Organic Matter C"), tags$td("Combined litter and soil carbon pools."), tags$td("gC/m²")),
                                                       tags$tr(tags$td("Foliage C"), tags$td("Carbon stored in leaves."), tags$td("gC/m²")),
                                                       tags$tr(tags$td("Labile C Pool"), tags$td("Short-term storage carbon available for allocation."), tags$td("gC/m²")),
                                                       tags$tr(tags$td("Wood C"), tags$td("Carbon stored in woody biomass."), tags$td("gC/m²")),
                                                       tags$tr(tags$td("Litter C"), tags$td("Carbon stored in surface litter."), tags$td("gC/m²")),
                                                       tags$tr(tags$td("Biomass C Stock"), tags$td("Total living biomass carbon."), tags$td("gC/m²")),
                                                       
                                                       # ---- Allocation & Carbon Fluxes (gC/m²/day) ----
                                                       tags$tr(tags$td("Alloc. to Foliage"), tags$td("Carbon allocated to leaf production."), tags$td("gC/m²/day")),
                                                       tags$tr(tags$td("Alloc. to Labile C"), tags$td("Carbon allocated to labile pool."), tags$td("gC/m²/day")),
                                                       tags$tr(tags$td("Alloc. to Roots"), tags$td("Carbon allocated to roots."), tags$td("gC/m²/day")),
                                                       tags$tr(tags$td("Alloc. to Wood"), tags$td("Carbon allocated to woody biomass."), tags$td("gC/m²/day")),
                                                       
                                                       tags$tr(tags$td("Net Biome Exchange (NBE)"), tags$td("Net carbon exchange including disturbance."), tags$td("gC/m²/day")),
                                                       tags$tr(tags$td("Net Biome Production (NBP)"), tags$td("Net ecosystem carbon balance after disturbance."), tags$td("gC/m²/day")),
                                                       tags$tr(tags$td("Net Ecosystem Exchange (NEE)"), tags$td("Net CO₂ flux between ecosystem and atmosphere."), tags$td("gC/m²/day")),
                                                       tags$tr(tags$td("Net Primary Production (NPP)"), tags$td("GPP minus autotrophic respiration."), tags$td("gC/m²/day")),
                                                       tags$tr(tags$td("Gross Primary Production (GPP)"), tags$td("Total carbon fixed by photosynthesis."), tags$td("gC/m²/day")),
                                                       
                                                       tags$tr(tags$td("Autotrophic Respiration"), tags$td("Plant respiration flux."), tags$td("gC/m²/day")),
                                                       tags$tr(tags$td("Heterotrophic Respiration"), tags$td("Microbial decomposition respiration."), tags$td("gC/m²/day")),
                                                       tags$tr(tags$td("Ecosystem Respiration"), tags$td("Total ecosystem respiration."), tags$td("gC/m²/day")),
                                                       
                                                       # ---- Hydrology (kg H₂O units) ----
                                                       tags$tr(tags$td("Evapotranspiration (ET)"), tags$td("Total evaporation and transpiration."), tags$td("kg H₂O/m²/day")),
                                                       tags$tr(tags$td("Soil Evaporation"), tags$td("Water loss directly from soil."), tags$td("kg H₂O/m²/day")),
                                                       tags$tr(tags$td("Transpiration"), tags$td("Water flux through plant stomata."), tags$td("kg H₂O/m²/day")),
                                                       tags$tr(tags$td("Wet Canopy Evap."), tags$td("Evaporation of intercepted precipitation."), tags$td("kg H₂O/m²/day")),
                                                       tags$tr(tags$td("Runoff"), tags$td("Lateral water flow."), tags$td("kg H₂O/m²/day")),
                                                       tags$tr(tags$td("Total Drainage"), tags$td("Vertical water loss from soil column."), tags$td("kg H₂O/m²/day")),
                                                       tags$tr(tags$td("Surface Water Change"), tags$td("Net change in surface water storage."), tags$td("kg H₂O/m²")),
                                                       tags$tr(tags$td("Snow Water Equivalent"), tags$td("Water stored in snowpack."), tags$td("kg H₂O/m²")),
                                                       
                                                       # ---- Water Efficiency ----
                                                       tags$tr(tags$td("Ecosystem WUE"), tags$td("GPP per unit evapotranspiration."), tags$td("gC/kg H₂O")),
                                                       tags$tr(tags$td("Plant WUE"), tags$td("GPP per unit transpiration."), tags$td("gC/kg H₂O")),
                                                       
                                                       # ---- Gas Exchange ----
                                                       tags$tr(tags$td("Stomatal Conductance"), tags$td("Gas exchange rate through stomata."), tags$td("mmol H₂O/m²/s")),
                                                       tags$tr(tags$td("Boundary Layer Conductance"), tags$td("Leaf boundary layer conductance."), tags$td("mmol H₂O/m²/s")),
                                                       tags$tr(tags$td("Intercellular/Ambient CO₂ (Ci/Ca)"), tags$td("Ratio of internal to ambient CO₂ concentration."), tags$td("unitless")),
                                                       tags$tr(tags$td("Stomatal Demand/Supply Ratio"), tags$td("Index of stomatal limitation."), tags$td("unitless")),
                                                       
                                                       # ---- Structural Variables ----
                                                       tags$tr(tags$td("Leaf Area Index (LAI)"), tags$td("Leaf area per unit ground area."), tags$td("m²/m²")),
                                                       tags$tr(tags$td("Root Depth Change"), tags$td("Change in effective rooting depth."), tags$td("m")),
                                                       
                                                       # ---- Soil Water Potential ----
                                                       tags$tr(tags$td("Soil Water Potential"), tags$td("Soil matric potential affecting plant water stress."), tags$td("MPa"))
                                                       
                                                     )
                                                   )))
             
  )
  
  # Initial Loading Spinner ---
  #tags$div(id = "initial-loader", tags$div(class = "lds-dual-ring-initial"))
)

# SERVER ----------------------
server <- function(input, output, session) {

  
  # clear LeafletDraw items ----
  clear_drawings <- function() {
    session$sendCustomMessage("clearDrawnItems", list())
  }
  
  # reactive value for clicked point ----
  clicked_point <- reactiveVal(NULL)
  
  # enable resize----
  observe({
    invalidateLater(500, session)
    session$sendCustomMessage("enableResizable", TRUE)
  })

  # 1. Mode Switching (point to area) ----
  observeEvent(input$selection_mode, {
    proxy <- leafletProxy("map")
    
    # reset clicked point
    clicked_point(NULL)
    selected_geom(NULL)
    output$ts_plot <- renderPlotly(NULL)
    
    # clear any old markers or polygons
    proxy %>% clearGroup("clicked_point") %>% 
      clearGroup("selected_area") %>%
      clearGroup("roi") %>%  
      clearGroup("_drawnItems") %>%
      clearMarkers() %>%
      clearControls() %>%
      removeDrawToolbar()
    
    
    # clear time series plot
    output$ts_plot <- renderPlotly(NULL)
    
    # clear Leaflet.Draw internal layer
    clear_drawings()
    # 
    # 
    # if (input$selection_mode == "point") {
    #   proxy %>% removeDrawToolbar() %>% 
    #     clearGroup("selected_area") %>% 
    #     clearGroup("roi") %>%  
    #     clearMarkers() %>%
    #     clearControls() %>%
    #     clearGroup("_drawnItems")
    # }
    
    #else 
      if (input$selection_mode == "drawn_area") {
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
        editOptions = editToolbarOptions(edit = TRUE, remove=TRUE)
      )
    }
    
    # else if (input$selection_mode == "uploaded_area") {
    #   proxy %>% removeDrawToolbar()
    # }
  })
  
  # change control panel based on mode selection
  observe({
    if (input$selection_mode == "uploaded_area") {
      shinyjs::enable("roi_upload")
      shinyjs::enable("add_roi")
      shinyjs::removeClass("roi_section", "disabled-section")
    } else {
      shinyjs::disable("roi_upload")
      shinyjs::disable("add_roi")
      shinyjs::addClass("roi_section", "disabled-section")
    }
  })
  
  
  # 2. Point Click ----
  observeEvent(input$map_click, {
    req(input$selection_mode == "point")
    
    lng <- input$map_click$lng
    lat <- input$map_click$lat
    clicked_point(c(lng, lat))
    
    # clear old polygons and time series
    leafletProxy("map") %>% clearGroup("selected_area") %>% clearMarkers() %>%
      clearGroup("_drawnItems")
    clear_drawings()
    
    output$ts_plot <- renderPlotly(NULL)
    
    # add clicked point
    leafletProxy("map") %>%
      clearGroup("clicked_point") %>%
      addCircleMarkers(lng, lat, radius = 4, color = "#244C20", fillOpacity = 1, group = "clicked_point")
    
    # extract + plot time series
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
          geom_line(color = "#788554") + #plot line color
          geom_point(color = "#244C20", size = 0.8) + #plot point color
          labs(y = display_labels[[vr]] %||% vr, x = "") +
          theme_minimal(10)
      )
    })
  })
  
  # 3. Area Drawing ----
  # observeEvent(input$map_draw_new_feature, {
  #   req(input$selection_mode == "area")
  #   
    observeEvent(input$map_draw_new_feature, {
      req(input$selection_mode == "drawn_area")
      
    # clear old polygons and time series
    leafletProxy("map") %>% clearGroup("selected_area")
    output$ts_plot <- renderPlotly(NULL)
    
    f <- input$map_draw_new_feature
    coords_raw <- f$geometry$coordinates[[1]]
    lng <- sapply(coords_raw, `[[`, 1)
    lat <- sapply(coords_raw, `[[`, 2)
    coords <- cbind(lng, lat)
    poly_sf <- sf::st_sf(geometry = sf::st_sfc(sf::st_polygon(list(coords)), crs = 4326))
    poly_vect <- terra::vect(poly_sf)
    
    # add new polygon
    # leafletProxy("map") %>%
    #   addPolygons(lng = lng, lat = lat, group = "selected_area",
    #               color = "#788554", fillOpacity = 0.2, weight = 2) # polygon viz
    # 
    # extract + plot time series
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
  }, ignoreInit = TRUE)
  
  # timeseries for uploaded roi
  observeEvent({
    input$add_roi
    input$selection_mode
  }, {
    
    req(input$selection_mode == "uploaded_area")
    req(roi_data())
    
    vr <- varname()
    r_stack <- rast_list[[vr]]
    
    poly_vect <- roi_data()
    
    vals <- terra::extract(r_stack, poly_vect, fun = mean, na.rm = TRUE)
    ts_means <- as.numeric(vals[1, -1])
    
    ts_means <- ts_means[1:length(time_seq)]
    
    if (length(ts_means) < length(time_seq)) {
      ts_means <- c(ts_means, rep(NA, length(time_seq) - length(ts_means)))
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
  
  
  
  
  # 4. Variable Switching ----
  #     need to always clear selection in between
  # observeEvent(input$variable, {
  #   
  #   # reset selected point and clear plot
  #   clicked_point(NULL)
  #   output$ts_plot <- renderPlotly(NULL)
  #   
  #   # clear map layers
  #   leafletProxy("map") %>%
  #     clearGroup("clicked_point") %>%
  #     clearGroup("selected_area")
  #   
  #   # clear internal draw layer
  #   clear_drawings()
  # })
  
  # observeEvent(input$variable, {
  #   
  #   vr <- varname()
  #   
  #   # if a geometry exists, recompute time series
  #   if (!is.null(selected_geom())) {
  #     
  #     geom <- selected_geom()
  #     r_stack <- rast_list[[vr]]
  #     
  #     if (geom$geometry$type == "Point") {
  #       
  #       coords <- matrix(unlist(geom$geometry$coordinates), ncol = 2)
  #       vals <- terra::extract(r_stack, coords)
  #       ts_vals <- as.numeric(vals[1, -1])
  #       
  #     } else if (geom$geometry$type == "Polygon") {
  #       
  #       poly_sf <- sf::st_as_sf(geom)
  #       terra_poly <- terra::vect(poly_sf)
  #       vals <- terra::extract(r_stack, terra_poly, fun = mean, na.rm = TRUE)
  #       ts_vals <- as.numeric(vals[1, -1])
  #     }
  #     
  #     df <- data.frame(Date = time_seq, Value = ts_vals)
  #     
  #     output$ts_plot <- renderPlotly({
  #       ggplotly(
  #         ggplot(df, aes(Date, Value)) +
  #           geom_line(color = "#788554") +
  #           geom_point(color = "#244C20", size = 0.8) +
  #           labs(y = display_labels[[vr]] %||% vr, x = "") +
  #           theme_minimal(10)
  #       )
  #     })
  #   }
  # })

  
  observeEvent(input$variable, {
    
    req(selected_geom())
    
    geom <- selected_geom()
    vr <- varname()
    r_stack <- rast_list[[vr]]
    
    validate(
      need(!is.null(r_stack), "Raster not found.")
    )
    
    # 
    # CASE 1: Uploaded shapefile (sf object)
    # 
    if (inherits(geom, "sf")) {
      
      terra_poly <- terra::vect(geom)
      vals <- terra::extract(r_stack, terra_poly, fun = mean, na.rm = TRUE)
      ts_vals <- as.numeric(vals[1, -1])
      
      # 
      # CASE 2: Drawn or clicked GeoJSON
      # 
    } else if (!is.null(geom$geometry$type)) {
      
      if (geom$geometry$type == "Point") {
        
        coords <- matrix(unlist(geom$geometry$coordinates), ncol = 2)
        vals <- terra::extract(r_stack, coords)
        ts_vals <- as.numeric(vals[1, -1])
        
      } else if (geom$geometry$type == "Polygon") {
        
        poly_sf <- sf::st_as_sf(geom)
        terra_poly <- terra::vect(poly_sf)
        vals <- terra::extract(r_stack, terra_poly, fun = mean, na.rm = TRUE)
        ts_vals <- as.numeric(vals[1, -1])
      }
      
    } else {
      return(NULL)
    }
    
    df <- data.frame(
      Date = time_seq,
      Value = ts_vals
    )
    
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
  
  # reactive value to store uploaded ROI 
  roi_data <- reactiveVal(NULL)
  
  varname <- reactive({
    var_labels[[input$variable]]
  })
  
  # 5. Uploading shapefile -----
  observeEvent(input$roi_upload, {
    req(input$roi_upload)
    
    tmpdir <- tempdir()
    unzip(input$roi_upload$datapath, exdir = tmpdir)
    shp_file <- list.files(tmpdir, pattern = "\\.shp$", full.names = TRUE)
    
    if (length(shp_file) > 0) {
      roi_sf <- tryCatch(
        #sf::st_read(shp_file, quiet = TRUE),
        terra::project(terra::vect(shp_file),'+proj=longlat +datum=WGS84'),
        error = function(e) NULL
      )
      roi_data(roi_sf)   # store in the reactiveVal
    }
  })
  

  # 6. Pre-caching raster slices -----
  r_slices <- reactiveValues()
  observe({
    for (vr_name in names(rast_list)) {
      r_stack <- rast_list[[vr_name]]
      r_slices[[vr_name]] <- lapply(1:nlyr(r_stack), function(i) terra::subset(r_stack, i))
    }
  })
  
  # 7. Render map -----
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
      addScaleBar(position = "bottomright", 
                  options = scaleBarOptions(maxWidth = 100, 
                                            imperial = TRUE,
                                            updateWhenIdle = TRUE))
    
    
  })
  
  session$onFlushed(function() {
    session$sendCustomMessage("clearDrawnItems", list())
  })
  
  # 8. Add ROI to map when button clicked ----
  # observeEvent(input$add_roi, {
  #   req(roi_data())
  #   
  #   leafletProxy("map") %>%
  #     clearGroup("roi") %>%  # remove old ROI if present
  #     addPolygons(
  #       data = roi_data(),
  #       group = "roi",
  #       color = "red", weight = 2, fillOpacity = 0.2
  #     ) %>%
  #     addLayersControl(
  #       overlayGroups = c("roi"),
  #       options = layersControlOptions(collapsed = FALSE)
  #     )
  # })
  
  observeEvent(input$add_roi, {
    req(roi_data())
    
    # Add to map
    leafletProxy("map") %>%
      clearGroup("roi") %>%
      addPolygons(
        data = roi_data(),
        group = "roi",
        color = "red",
        weight = 2,
        fillOpacity = 0.2
      )
    
    # Only activate as selection if in uploaded mode
    if (input$selection_mode == "uploaded_area") {
      
      # Convert terra vect to GeoJSON-like structure
      sf_obj <- sf::st_as_sf(roi_data())
      
      # geom_list <- lapply(sf::st_geometry(sf_obj), function(g) {
      #   coords <- sf::st_coordinates(g)[,1:2]
      #   list(
      #     type = "Feature",
      #     geometry = list(
      #       type = "Polygon",
      #       coordinates = list(as.matrix(coords))
      #     )
      #   )
      # })
      # 
      selected_geom(sf_obj)
      
      # For now assume first polygon (can extend later)
      #selected_geom(geom_list[[1]])
    }
  })
 
  # show/hide spinner for Map View tab updates -- this isn't working
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
  
  # 9. Legend -----
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
  
  
 
  
  
  # 10. GIF rendering / download -----
  gif_file <- reactiveVal(NULL)
  gif_generating <- reactiveVal(FALSE)
  
  # reactive to indicate whether a GIF exists
  output$gif_file <- reactive({
    !is.null(gif_file())
  })
  outputOptions(output, "gif_file", suspendWhenHidden = FALSE)
  
  output$gif_preview_block <- renderUI({
    #req(input$export_type == "gif")
    
    if (gif_generating()) {
      # only show status message while generating
      tags$div(
        style = "color: #244C20; font-weight: bold;",
        "Generating GIF, please wait..."
      )
    } else
      
      if (!is.null(gif_file())) {
        # status message + GIF preview + download button
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
      # default message
      tags$p("GIF preview will generate below...")
    }
  })
  
  
  # gif palette fxn
  gif_get_palette_for_var <- function(var_name, rast_list, var_groups, group_palettes, slice_rast = NULL) {
    
    group <- var_groups[[var_name]]
    pal_name <- group_palettes[[group]]
    
    # compute global min/max across all time
    var_stack <- rast_list[[var_name]]
    
    var_range <- terra::global(var_stack, fun = range, na.rm = TRUE)
    
    
    vr_min <- min(var_range$X1, na.rm=TRUE)
    vr_max <- max(var_range$X2, na.rm=TRUE)
    domain <- c(vr_min, vr_max)
    
    
    # setting palettes
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
  
  
  # GIF generator function
  generate_gif <- function(vr, rast_subset, filename, fps = 5, width = 950, height = 750) {
    temp_dir <- tempdir()
    png_paths <- file.path(temp_dir, sprintf("frame_%03d.png", seq_len(nlyr(rast_subset))))
    
    width <- as.integer(width)
    height <- as.integer(height)
    
    # create palette using fxn
    pal_fun <- gif_get_palette_for_var(vr, rast_list, var_groups, group_palettes)
    
    var_range <- terra::global(rast_subset, fun = range, na.rm = TRUE)
    vr_min <- min(var_range$X1, na.rm=TRUE)
    vr_max <- max(var_range$X2, na.rm=TRUE)
    domain <- c(vr_min, vr_max)
    
    rast_subset_p <- terra::project(rast_subset, "EPSG:3857")
    projected_ext <- st_bbox(rast_subset_p, crs=st_crs(3857))
    
    # define buffer distance
    r <- 80000 # 10,000 m
    
    bbox_expanded <- projected_ext + c(-r, -r, -60000, r)
    
    basemap <- basemap_terra(bbox_expanded, map_service="carto", map_type="voyager")
    # separate spec for fire variables
    group <- var_groups[[vr]]
    if (group == "Fire") {
      pal_fun 
      for (i in seq_len(nlyr(rast_subset_p))) {
        png(png_paths[i], width = width, height = height)
        
        # plot basemap
        plot(basemap, 
             main = paste("Date:", names(rast_subset)[i]), box=FALSE, mar=c(3, 3, 6, 2), axes=FALSE)
        
        # copy this slice
        #slice <- rast_subset_p[[i]]
        
        # mask zeros for plotting
        #slice[slice == 0] <- NA
        
        #plot(rast_subset_p[[i]] == 0, col = "#000000", legend = FALSE, add=TRUE)
        # plot non-zero values with palette
        plot(rast_subset_p[[i]], col = pal_fun, range = domain, type="continuous", 
             plg=list(title = display_labels[[vr]]), axes=FALSE, legend=TRUE, add=TRUE)
      
        
        dev.off()
      }
    }
    
    else {
      for (i in seq_len(nlyr(rast_subset_p))) {
        png(png_paths[i], width = width, height = height)
        
        # plot basemap first 
        plot(basemap, 
             main = paste("Date:", names(rast_subset)[i]), box=FALSE, mar=c(3, 3, 6, 2), axes=FALSE)
        
        # plot raster frame with consistent legend
        plot(rast_subset_p[[i]], col = pal_fun, range = domain, 
             plg=list(title = display_labels[[vr]]), add = TRUE, axes=FALSE)
        
        dev.off()
      }
    }
    
    # ensure frames exist
    if (!all(file.exists(png_paths))) {
      stop("Some PNG frames were not created properly.")
    }
    
    # generate GIF
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
    
    # cleanup PNG frames
    unlink(png_paths)
    
    return(filename)
  }
  
  # observe event for Generate GIF button
  observeEvent(input$generate_gif, {
    req(input$gif_variable)
    req(input$gif_timerange)
    req(length(input$gif_timerange) == 2)
    
    # validate date format
    if (any(is.na(as.Date(input$gif_timerange, format = "%Y-%m-%d")))) {
      showNotification("Invalid date format", type = "error")
      return()
    }
    
    gif_generating(TRUE)
    gif_file(NULL)
    
    vr <- input$gif_variable
    r_stack <- rast_list[[vr]]
    # potential error messages
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
  
  
  # download handler, uses the generated GIF
  output$download_gif <- downloadHandler(
    filename = function() {
      paste0("map_animation_", Sys.Date(), ".gif")
    },
    content = function(file) {
      req(gif_file())
      file.copy(gif_file(), file, overwrite = TRUE)
    }
  )
  
  # reactive values to store drawn point/polygon for export
  selected_geom <- reactiveVal(NULL)
  
  observeEvent(input$map_draw_new_feature, {
    selected_geom(input$map_draw_new_feature)
  })
  
  observeEvent(input$map_click, {
    if (input$selection_mode == "point") {
      click <- input$map_click
      if (!is.null(click)) {
        # store point geometry
        selected_geom(list(
          type = "Feature",
          geometry = list(type = "Point", coordinates = c(click$lng, click$lat))
        ))
      }
    }
  })
  
 # 11. CSV generation / download ----
  # csv_preview_data <- eventReactive(input$generate_csv_preview, {
  #   req(selected_geom())
  #   req(input$csv_variable)
  #   
  #   geom <- selected_geom()
  #   rast_obj <- rast_list[[input$csv_variable]]
  #   
  #   dates <- (names(rast_obj))
  #   # potential error message
  #   validate(
  #     need(!is.null(rast_obj), "No raster found for selected variable.")
  #   )
  #   
  #   vals <- NULL
  #   
  #   # handle points
  #   if (!is.null(geom$geometry$type) && geom$geometry$type == "Point") {
  #     coords <- unlist(geom$geometry$coordinates)
  #     sv <- terra::vect(matrix(coords, ncol = 2),
  #                       crs = crs(rast_obj))  # make a point SpatVector
  #     vals <- terra::extract(rast_obj, sv)
  #     
  #     # handle polygons
  #   } else if (!is.null(geom$geometry$type) && geom$geometry$type == "Polygon") {
  #     poly_sf <- sf::st_as_sf(geom) 
  #     terra_poly <- terra::vect(poly_sf)
  #     vals <- terra::extract(rast_obj, terra_poly, fun = mean, na.rm = TRUE)
  #   }
  #   # need values to generate
  #   validate(
  #     need(!is.null(vals) && ncol(vals) > 1, "No values extracted for selected geometry.")
  #   )
  #   
  #   # drop ID column
  #   vals <- vals[, -1, drop = FALSE]
  #   
  #   data.frame(
  #     Date = as.character(dates),
  #     Value = as.numeric(vals[1, ])
  #   )
  # })
  
  # csv_preview_data <- eventReactive(input$generate_csv_preview, {
  #   
  #   req(selected_geom())
  #   req(input$csv_variable)
  #   
  #   geom <- selected_geom()
  #   rast_obj <- rast_list[[input$csv_variable]]
  #   
  #   validate(
  #     need(!is.null(rast_obj), "No raster found for selected variable.")
  #   )
  #   
  #   # If geometry is sf (uploaded polygon)
  #   if (inherits(geom, "sf")) {
  #     
  #     terra_poly <- terra::vect(geom)
  #     vals <- terra::extract(rast_obj, terra_poly, fun = mean, na.rm = TRUE)
  #     
  #   } else if (geom$geometry$type == "Point") {
  #     
  #     coords <- matrix(unlist(geom$geometry$coordinates), ncol = 2)
  #     vals <- terra::extract(rast_obj, coords)
  #     
  #   } else if (geom$geometry$type == "Polygon") {
  #     
  #     poly_sf <- sf::st_as_sf(geom)
  #     terra_poly <- terra::vect(poly_sf)
  #     vals <- terra::extract(rast_obj, terra_poly, fun = mean, na.rm = TRUE)
  #   }
  #   
  #   validate(
  #     need(!is.null(vals) && ncol(vals) > 1, "No values extracted.")
  #   )
  #   
  #   vals <- vals[, -1, drop = FALSE]
  #   
  #   data.frame(
  #     Date = names(rast_obj),
  #     Value = as.numeric(vals[1, ])
  #   )
  # })
  # 
  
  csv_preview_data <- eventReactive(input$generate_csv_preview, {
    
    req(selected_geom())
    req(input$csv_variable)
    
    geom <- selected_geom()
    rast_obj <- rast_list[[input$csv_variable]]
    
    validate(
      need(!is.null(rast_obj), "No raster found for selected variable.")
    )
    
    vals <- NULL
    
    # ---- CASE 1: Uploaded ROI (sf object) ----
    if (inherits(geom, "sf")) {
      
      terra_poly <- terra::vect(geom)
      vals <- terra::extract(rast_obj, terra_poly, fun = mean, na.rm = TRUE)
      
      # ---- CASE 2: GeoJSON-like list ----
    } else if (is.list(geom) && !is.null(geom$geometry)) {
      
      geom_type <- geom$geometry$type
      
      if (identical(geom_type, "Point")) {
        
        coords <- matrix(unlist(geom$geometry$coordinates), ncol = 2)
        vals <- terra::extract(rast_obj, coords)
        
        # drawn polygon
      }  else if (is.list(geom) && 
                   !is.null(geom$geometry$type) &&
                   identical(geom$geometry$type, "Polygon")) {
        
        coords <- geom$geometry$coordinates[[1]]
        
        coords_mat <- matrix(
          as.numeric(unlist(coords)),
          ncol = 2,
          byrow = TRUE
        )
        
        poly_sf <- sf::st_sf(
          geometry = sf::st_sfc(
            sf::st_polygon(list(coords_mat)),
            crs = 4326
          )
        )
        
        terra_poly <- terra::vect(poly_sf)
        
        vals <- terra::extract(rast_obj, terra_poly, fun = mean, na.rm = TRUE)
      }
    }
    
    validate(
      need(!is.null(vals) && ncol(vals) > 1,
           "No values extracted for selected geometry.")
    )
    vals <- vals[, -1, drop = FALSE]
    
    ts_vals <- as.numeric(vals[1, ])
    
    layer_names <- names(rast_obj)
    
    ts_vals_full <- rep(NA_real_, length(layer_names))
    ts_vals_full[seq_along(ts_vals)] <- ts_vals
    
    data.frame(
      Date  = layer_names,
      Value = ts_vals_full
    )
  })
  
  
  ## CSV Preview Output ----
  output$csv_preview <- renderDT({
    req(csv_preview_data())
    datatable(
      head(csv_preview_data(), 10),
      options = list(pageLength = 10, searching = FALSE, dom = 't')
    )
  })
  
  ## CSV download UI ----
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
      # convert Date columns to character
      vals[] <- lapply(vals, function(col) {
        if (inherits(col, "Date")) as.character(col) else col
      })
      write.csv(vals, file, row.names = FALSE)
    }
  )
  
}

# final call woohoo!
shinyApp(ui, server)

