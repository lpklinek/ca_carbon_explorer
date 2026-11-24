

info_menu <- navbarMenu("About this tool", 
                        tabPanel("Overview", div(class = "info-tab",
                                                 h2("Overview"),
                                                 p("Welcome to the California Carbon Explorer! This is an open-source tool for quantifying and visualizing the forest carbon cycle in California, and understanding the resilience and vulnerability of forested ecosystems. 
                                      We hope that scientists and land managers, from government to non-profit, private, or tribal, can use this platform to monitor changes over time and ultimately inform adaptive management strategies to increase ecosystem health and carbon storage."), 
                                                 p("This platform was coded and designed by Lily Klinek. The development of this tool was funded by the CAL FIRE Forest Health Research Program grant (#8GG20808) awarded to PI Troy Magney. The platform was built using R Shiny, and used data from a CARDAMOM model run completed by Jessie Au, Luke Smallman, and David Milodowski. 
	                                    All source code for the current version of the platform can be found on the following github page:", 
                                                   tags$a(
	                                      href = "https://github.com/lpklinek/ca_carbon_explorer",
	                                      "https://github.com/lpklinek/ca_carbon_explorer",
	                                      target = "_blank"
	                                    )),
                        )),
                        tabPanel("Instructions", div(class = "info-tab",
                                                     h2("Instructions"),
                                                     h3("Map View"),
                                                     p("The “Map” tab of the dashboard can be used to explore visualizations of the model output layers, and to plot time series of any output variable for points or regions of interest. The toolbar on the left-hand side of the page can be used to select an output variable (see “Variable Descriptions” tab for more information), and to toggle between mapping median values and uncertainty widths (95th percentile value - 5th percentile value) for each pixel. The slider bar can be used to select a timestamp to display on the map (between January 2014 and December 2022 on a monthly resolution)."
                                                     ),
                                                     br(),
                                                     p("When “Point” mode is selected, users can click on a single pixel on the map to view a time series plot for that point. Users can mouse over the plot to view values for each plotted point. When “Drawn Area” mode is selected, users can draw a polygon of interest on the map using the “Draw” toolbar."
                                                     ),
                                                     br(),
                                                     p("When “Uploaded Area” mode is selected, users can upload a .zip folder (must include a .shp file) of their region of interest (ROI)."
                                                     ),
                                                     br(),
                                                     h3("Export Data"),
                                                     p("The “Export Data” tab of the dashboard can be used to download .csv files for the variable and point or area of interest that has been selected on the “Map” tab. If a point is selected, the pixel's posterior median and upper/lower percentiles for each timestep will be downloaded. If an area is selected, the area-mean of the pixel-level percentiles for each timestep will be downloaded."
                                                     ),
                                                     br(),
                                                     h3("Generate GIF"),
                                                     p("The “Generate GIF” tab can be used to generate and download a time-series animation of any variable for the entire state."
                                                     ),
                                                     br(),
                                                     br(),
                                                     p("We are continuing to test and debug all functionalities of the California Carbon Explorer. Please reach out to us at lpklinek@ucdavis.edu if you run into any issues while using the dashboard, or if you have any feedback you’d like to share! We’d love to hear from you."
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
