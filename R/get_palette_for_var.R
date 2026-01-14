get_palette_for_var <- function(var_name, rast_list, var_groups, group_palettes, slice_rast = NULL) {
  # Sanity checks
  if (is.null(var_name) || !(var_name %in% names(rast_list))) {
    stop("Variable name ", var_name, " is not found in rast_list.")
  }
  if (is.null(var_groups[[var_name]])) {
    stop("Variable ", var_name, " does not have a group defined in var_groups.")
  }
  
  group <- var_groups[[var_name]]
  pal_name <- group_palettes[[group]]
  
  if (is.null(pal_name)) {
    stop("No palette defined for group ", group)
  }
  
  # Compute global min/max across all time
  var_stack <- rast_list[[var_name]]
  if (is.null(var_stack)) {
    stop("Raster stack for variable ", var_name, " is NULL.")
  }
  if (length(var_stack) == 0) {
    stop("Raster stack for variable ", var_name, " has zero layers.")
  }
  
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
    } else {
    #cols <- colorRampPalette(RColorBrewer::brewer.pal(9, pal_name))(100)
    cols <- scico(100, palette = pal_name)
    }
  }
  
  # Special case for fire variables: return a custom function that maps 0 to black
  if (group == "Fire") {
    pal_func <- colorNumeric(cols, domain = domain, na.color = "transparent")
    return(function(x) {
      out <- pal_func(x)
      out[x == 0] <- "#000000"
      out
    })
  }
  
  colorNumeric(palette = cols, domain = domain, na.color = "transparent")
}