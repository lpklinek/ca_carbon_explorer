# #Libraries
# library(leaflet)
# library(tidyverse)
# library(terra)
# library(ncdf4)
# library(sp)
# library(scico)
# 
# library(raster)
# library(sf)
# 
# 
# # Open NetCDF file
# nc <- nc_open("/Users/lklinek/Desktop/CAforest_dalec4_soildailynonfire0.125deg_stock_flux.nc")
# 
# # Variable names to extract
# vars_to_extract <- c(
#   "Ctotal_gCm2", "som_gCm2", "dom_gCm2", "foliage_gCm2", "labile_gCm2", "wood_gCm2",
#   "litter_gCm2", "biomass_gCm2", "alloc_foliage_gCm2day", "alloc_labile_gCm2day",
#   "alloc_roots_gCm2day", "alloc_wood_gCm2day",
# 
#   "dCbiomass_gCm2", "dCdom_gCm2", "dCfoliage_gCm2", "dClabile_gCm2", "dClitter_gCm2",
#   "dCroots_gCm2", "dCsom_gCm2", "dCwood_gCm2",
# 
#   "nbe_gCm2day", "nbp_gCm2day", "nee_gCm2day", "npp_gCm2day",
# 
#   "fire_gCm2day", "FIREemiss_biomass_gCm2day", "FIREemiss_dom_gCm2day",
#   "FIREemiss_foliage_gCm2day", "FIREemiss_labile_gCm2day", "FIREemiss_litter_gCm2day",
#   "FIREemiss_roots_gCm2day", "FIREemiss_som_gCm2day", "FIREemiss_wood_gCm2day",
# 
#   "dSurfWater_kgH2Om2", "dwSWP_MPa", "ET_kgH2Om2day", "Esoil_kgH2Om2day",
#   "Etrans_kgH2Om2day", "Ewetcanopy_kgH2Om2day", "runoff_kgH2Om2day", "snow_kgH2Om2",
#   "total_drainage_kgH2Om2day", "wue_eco_gCkgH2O", "wue_plant_gCkgH2O",
# 
#   "CiCa", "gpp_gCm2day", "gs_mmolH2Om2s", "gb_mmolH2Om2s", "gs_demand_supply_ratio",
#   "rauto_gCm2day", "reco_gCm2day", "rhet_gCm2day",
# 
#   "foliage_to_litter_gCm2day", "litter_to_som_gCm2day", "labile_to_foliage_gCm2day",
#   "biomass_to_litter_gCm2day", "wood_to_litter_gCm2day", "roots_to_litter_gCm2day",
# 
#   "harvest_gCm2day", "HARVESTextracted_biomass_gCm2day", "HARVESTextracted_wood_gCm2day",
#   "HARVESTextracted_foliage_gCm2day", "HARVESTextracted_roots_gCm2day",
#   "HARVESTextracted_litter_gCm2day", "HARVESTextracted_labile_gCm2day",
#   "HARVESTextracted_som_gCm2day",
# 
#   "lai_m2m2", "dRootDepth_m"
# )
# 
# # Get dimensions
# lon <- ncvar_get(nc, "longitude")  # [83, 77]
# lat <- ncvar_get(nc, "latitude")   # [83, 77]
# quantiles <- ncvar_get(nc, "quantile")
# q_index <- which(quantiles == 0.5)
# time_steps <- 108
# 
# # Reverse latitude order if needed because raster rows run from top to bottom
# if (lat[1] < lat[2]) {
#   lat <- rev(lat)
# }
# 
# 
# # Create an empty list to store stacks
# rast_list <- list()
# 
# for (v in vars_to_extract) {
#   # Extract variable for quantile = 0.5: [site, quantile, time]
#   message("Processing: ", v)
#   data_raw <- ncvar_get(nc, v)  # [1534, 7, 108]
#   data_q <- data_raw[, q_index, ]  # [1534, 108]
# 
#   # Initialize stack
#   layers <- vector("list", time_steps)
# 
#   for (t in 1:time_steps) {
#     mat <- matrix(NA, nrow = 83, ncol = 77)
# 
#     # Get index mapping
#     i_loc <- ncvar_get(nc, "i_location")  # x index (1-based)
#     j_loc <- ncvar_get(nc, "j_location")  # y index (1-based)
# 
#     # Remove NA locations
#     valid_idx <- which(!is.na(i_loc) & !is.na(j_loc))
# 
#     # Assign values to matrix
#     mat[cbind(i_loc[valid_idx], j_loc[valid_idx])] <- data_q[valid_idx, t]
# 
#     # Rotate matrix 90 degrees to the right
#     mat_rotated <- t(mat)[, nrow(mat):1]
# 
#     # Create raster
#     r <- rast(mat_rotated)
#     ext(r) <- ext(min(lon), max(lon), min(lat), max(lat))
#     crs(r) <- "EPSG:4326"
# 
#     layers[[t]] <- r
#   }
# 
#   rast_stack <- rast(layers)
#   names(rast_stack) <- format(seq(as.Date("2014-01-01"), by = "month", length.out = time_steps), "%Y-%m")
#   rast_stack <- flip(rast_stack, direction="v")
#   rast_stack <- flip(rast_stack, direction="h")
#   rast_list[[v]] <- rast_stack
# }
# 
# 
# # Clean up
# nc_close(nc)
# 
# 
# 
# # variable labels and palette assignments -----
# ## variable labels ----
# var_labels <- c(
#   "Total Ecosystem C Stock" = "Ctotal_gCm2",
#   "Soil Organic Matter C" = "som_gCm2",
#   "Dead Organic Matter C" = "dom_gCm2",
#   "Foliage C" = "foliage_gCm2",
#   "Labile C Pool" = "labile_gCm2",
#   "Wood C" = "wood_gCm2",
#   "Litter C" = "litter_gCm2",
#   "Biomass C Stock" = "biomass_gCm2",
#   "Alloc. to Foliage" = "alloc_foliage_gCm2day",
#   "Alloc. to Labile C" = "alloc_labile_gCm2day",
#   "Alloc. to Roots" = "alloc_roots_gCm2day",
#   "Alloc. to Wood" = "alloc_wood_gCm2day",
# 
#   "Change Biomass C" = "dCbiomass_gCm2",
#   "Change DOM C" = "dCdom_gCm2",
#   "Change Foliage C" = "dCfoliage_gCm2",
#   "Change Labile C" = "dClabile_gCm2",
#   "Change Litter C" = "dClitter_gCm2",
#   "Change Roots C" = "dCroots_gCm2",
#   "Change SOM C" = "dCsom_gCm2",
#   "Change Wood C" = "dCwood_gCm2",
# 
#   "Net Biome Exchange (NBE)" = "nbe_gCm2day",
#   "Net Biome Production (NBP)" = "nbp_gCm2day",
#   "Net Ecosystem Exchange (NEE)" = "nee_gCm2day",
#   "Net Primary Production (NPP)" = "npp_gCm2day",
# 
#   "Fire Carbon Loss" = "fire_gCm2day",
#   "Fire Emiss. Biomass" = "FIREemiss_biomass_gCm2day",
#   "Fire Emiss. DOM" = "FIREemiss_dom_gCm2day",
#   "Fire Emiss. Foliage" = "FIREemiss_foliage_gCm2day",
#   "Fire Emiss. Labile" = "FIREemiss_labile_gCm2day",
#   "Fire Emiss. Litter" = "FIREemiss_litter_gCm2day",
#   "Fire Emiss. Roots" = "FIREemiss_roots_gCm2day",
#   "Fire Emiss. SOM" = "FIREemiss_som_gCm2day",
#   "Fire Emiss. Wood" = "FIREemiss_wood_gCm2day",
# 
#   "Surface Water Change" = "dSurfWater_kgH2Om2",
#   "Soil Water Potential" = "dwSWP_MPa",
#   "Evapotranspiration (ET)" = "ET_kgH2Om2day",
#   "Soil Evaporation" = "Esoil_kgH2Om2day",
#   "Transpiration" = "Etrans_kgH2Om2day",
#   "Wet Canopy Evap." = "Ewetcanopy_kgH2Om2day",
#   "Runoff" = "runoff_kgH2Om2day",
#   "Snow Water Equivalent" = "snow_kgH2Om2",
#   "Total Drainage" = "total_drainage_kgH2Om2day",
#   "Ecosystem WUE" = "wue_eco_gCkgH2O",
#   "Plant WUE" = "wue_plant_gCkgH2O",
# 
#   "Intercellular/Ambient CO2 (Ci/Ca)" = "CiCa",
#   "Gross Primary Production (GPP)" = "gpp_gCm2day",
#   "Stomatal Conductance" = "gs_mmolH2Om2s",
#   "Boundary Layer Conductance" = "gb_mmolH2Om2s",
#   "Stomatal Demand/Supply Ratio" = "gs_demand_supply_ratio",
#   "Autotrophic Respiration" = "rauto_gCm2day",
#   "Ecosystem Respiration" = "reco_gCm2day",
#   "Heterotrophic Respiration" = "rhet_gCm2day",
# 
#   "Foliage to Litter" = "foliage_to_litter_gCm2day",
#   "Litter to SOM" = "litter_to_som_gCm2day",
#   "Labile to Foliage" = "labile_to_foliage_gCm2day",
#   "Biomass to Litter" = "biomass_to_litter_gCm2day",
#   "Wood to Litter" = "wood_to_litter_gCm2day",
#   "Roots to Litter" = "roots_to_litter_gCm2day",
# 
#   "Harvested Carbon" = "harvest_gCm2day",
#   "Harvest Biomass" = "HARVESTextracted_biomass_gCm2day",
#   "Harvest Wood" = "HARVESTextracted_wood_gCm2day",
#   "Harvest Foliage" = "HARVESTextracted_foliage_gCm2day",
#   "Harvest Roots" = "HARVESTextracted_roots_gCm2day",
#   "Harvest Litter" = "HARVESTextracted_litter_gCm2day",
#   "Harvest Labile" = "HARVESTextracted_labile_gCm2day",
#   "Harvest SOM" = "HARVESTextracted_som_gCm2day",
# 
#   "Leaf Area Index (LAI)" = "lai_m2m2",
#   "Root Depth Change" = "dRootDepth_m"
# )
# 
# 
# 
# ## display labels -- version with units ----
# display_labels <- c(
#   "Ctotal_gCm2" = "Total Ecosystem C Stock (gC/m²)",
#   "som_gCm2" = "Soil Organic Matter C (gC/m²)",
#   "dom_gCm2" = "Dead Organic Matter C (gC/m²)",
#   "foliage_gCm2" = "Foliage C (gC/m²)",
#   "labile_gCm2" = "Labile C Pool (gC/m²)",
#   "wood_gCm2" = "Wood C (gC/m²)",
#   "litter_gCm2" = "Litter C (gC/m²)",
#   "biomass_gCm2" = "Biomass C Stock (gC/m²)",
#   "alloc_foliage_gCm2day" = "Alloc. to Foliage (gC/m²/day)",
#   "alloc_labile_gCm2day" = "Alloc. to Labile C (gC/m²/day)",
#   "alloc_roots_gCm2day" = "Alloc. to Roots (gC/m²/day)",
#   "alloc_wood_gCm2day" = "Alloc. to Wood (gC/m²/day)",
#   "dCbiomass_gCm2" = "Change Biomass C (gC/m²)",
#   "dCdom_gCm2" = "Change DOM C (gC/m²)",
#   "dCfoliage_gCm2" = "Change Foliage C (gC/m²)",
#   "dClabile_gCm2" = "Change Labile C (gC/m²)",
#   "dClitter_gCm2" = "Change Litter C (gC/m²)",
#   "dCroots_gCm2" = "Change Roots C (gC/m²)",
#   "dCsom_gCm2" = "Change SOM C (gC/m²)",
#   "dCwood_gCm2" = "Change Wood C (gC/m²)",
#   "nbe_gCm2day" = "Net Biome Exchange (NBE) (gC/m²/day)",
#   "nbp_gCm2day" = "Net Biome Production (NBP) (gC/m²/day)",
#   "nee_gCm2day" = "Net Ecosystem Exchange (NEE) (gC/m²/day)",
#   "npp_gCm2day" = "Net Primary Production (NPP) (gC/m²/day)",
#   "fire_gCm2day" = "Fire Carbon Loss (gC/m²/day)",
#   "FIREemiss_biomass_gCm2day" = "Fire Emiss. Biomass (gC/m²/day)",
#   "FIREemiss_dom_gCm2day" = "Fire Emiss. DOM (gC/m²/day)",
#   "FIREemiss_foliage_gCm2day" = "Fire Emiss. Foliage (gC/m²/day)",
#   "FIREemiss_labile_gCm2day" = "Fire Emiss. Labile (gC/m²/day)",
#   "FIREemiss_litter_gCm2day" = "Fire Emiss. Litter (gC/m²/day)",
#   "FIREemiss_roots_gCm2day" = "Fire Emiss. Roots (gC/m²/day)",
#   "FIREemiss_som_gCm2day" = "Fire Emiss. SOM (gC/m²/day)",
#   "FIREemiss_wood_gCm2day" = "Fire Emiss. Wood (gC/m²/day)",
#   "dSurfWater_kgH2Om2" = "Surface Water Change (kg H₂O/m²)",
#   "dwSWP_MPa" = "Soil Water Potential (MPa)",
#   "ET_kgH2Om2day" = "Evapotranspiration (ET) (kg H₂O/m²/day)",
#   "Esoil_kgH2Om2day" = "Soil Evaporation (kg H₂O/m²/day)",
#   "Etrans_kgH2Om2day" = "Transpiration (kg H₂O/m²/day)",
#   "Ewetcanopy_kgH2Om2day" = "Wet Canopy Evap. (kg H₂O/m²/day)",
#   "runoff_kgH2Om2day" = "Runoff (kg H₂O/m²/day)",
#   "snow_kgH2Om2" = "Snow Water Equivalent (kg H₂O/m²)",
#   "total_drainage_kgH2Om2day" = "Total Drainage (kg H₂O/m²/day)",
#   "wue_eco_gCkgH2O" = "Ecosystem WUE (gC/kg H₂O)",
#   "wue_plant_gCkgH2O" = "Plant WUE (gC/kg H₂O)",
#   "CiCa" = "Intercellular/Ambient CO2 (Ci/Ca) (unitless)",
#   "gpp_gCm2day" = "Gross Primary Production (GPP) (gC/m²/day)",
#   "gs_mmolH2Om2s" = "Stomatal Conductance (mmol H₂O/m²/s)",
#   "gb_mmolH2Om2s" = "Boundary Layer Conductance (mmol H₂O/m²/s)",
#   "gs_demand_supply_ratio" = "Stomatal Demand/Supply Ratio (m)",
#   "rauto_gCm2day" = "Autotrophic Respiration (gC/m²/day)",
#   "reco_gCm2day" = "Ecosystem Respiration (gC/m²/day)",
#   "rhet_gCm2day" = "Heterotrophic Respiration (gC/m²/day)",
#   "foliage_to_litter_gCm2day" = "Foliage to Litter (gC/m²/day)",
#   "litter_to_som_gCm2day" = "Litter to SOM (gC/m²/day)",
#   "labile_to_foliage_gCm2day" = "Labile to Foliage (gC/m²/day)",
#   "biomass_to_litter_gCm2day" = "Biomass to Litter (gC/m²/day)",
#   "wood_to_litter_gCm2day" = "Wood to Litter (gC/m²/day)",
#   "roots_to_litter_gCm2day" = "Roots to Litter (gC/m²/day)",
#   "harvest_gCm2day" = "Harvested Carbon (gC/m²/day)",
#   "HARVESTextracted_biomass_gCm2day" = "Harvest Biomass (gC/m²/day)",
#   "HARVESTextracted_wood_gCm2day" = "Harvest Wood (gC/m²/day)",
#   "HARVESTextracted_foliage_gCm2day" = "Harvest Foliage (gC/m²/day)",
#   "HARVESTextracted_roots_gCm2day" = "Harvest Roots (gC/m²/day)",
#   "HARVESTextracted_litter_gCm2day" = "Harvest Litter (gC/m²/day)",
#   "HARVESTextracted_labile_gCm2day" = "Harvest Labile (gC/m²/day)",
#   "HARVESTextracted_som_gCm2day" = "Harvest SOM (gC/m²/day)",
#   "lai_m2m2" = "Leaf Area Index (LAI) (m²/m²)",
#   "dRootDepth_m" = "Root Depth Change (m)"
# )
# 
# 
# 
# ## looking for the min and max values globally and temporally for each variable
# var_range_list <- list()
# 
# for (varname in names(rast_list)) {
#   var_stack <- rast_list[[varname]]  # Already a SpatRaster
# 
#   # Get global min and max
#   global_min_list <- global(var_stack, fun = "min", na.rm = TRUE)
#   global_min <- global_min_list$min[which.min(global_min_list$min)]
#   global_max_list <- global(var_stack, fun = "max", na.rm = TRUE)
#   global_max <- global_max_list$max[which.max(global_max_list$max)]
# 
#   var_range_list[[varname]] <- c(global_min, global_max)
# }
# 
# output_dir <- "data/processed_rasters"
# dir.create(output_dir, showWarnings = FALSE)
# 
# for (var_name in names(rast_list)) {
#   cat("Saving:", var_name, "\n")
# 
#   rast_stack <- rast_list[[var_name]]  # Should be a SpatRaster with multiple layers (time steps)
# 
#   # Define output path
#   out_path <- file.path(output_dir, paste0(var_name, ".tif"))
# 
#   # Save with compression
#   writeRaster(
#     rast_stack,
#     filename = out_path,
#     overwrite = TRUE,
#     gdal = c("COMPRESS=LZW")
#   )
# }
# 
# saveRDS(list(rast_list = rast_list,
#              var_labels = var_labels,
#              display_labels = display_labels,
#              var_names = vars_to_extract,
#              var_range_list = var_range_list), "data/processed_data.Rds")
# 
# 
