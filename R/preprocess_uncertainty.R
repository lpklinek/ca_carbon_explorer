# Build uncertainty-bound rasters for the California Carbon Explorer.
# Leaves data/processed_rasters/ (medians) untouched. Run once, locally.
#
# Writes:  data/unc/<level>/lo/<var>.tif, data/unc/<level>/hi/<var>.tif   (32-bit float, LZW)
# Updates: data/processed_data.Rds  (adds unc_levels, unc_range_list; drops the unusable rast_list)

library(ncdf4)
library(terra)
setwd('/Users/lklinek/Desktop/Dissertation/CARDAMOM_shiny/CARDAMOM_output_shiny')
nc_path <- "/Users/lklinek/Desktop/CAforest_dalec4_soildailynonfire0.125deg_stock_flux.nc"
nc <- nc_open(nc_path)

# Read this: it may state exactly how the quantiles were computed.
cat(paste(ncvar_get(nc, "readme"), collapse = "\n"), "\n")

dat  <- readRDS("data/processed_data.Rds")
vars <- dat$var_names

# Intervals to export. Edit freely; add "95" = c(0.025, 0.975) if wanted.
# (Each level adds roughly 2 x 72 small tifs; check total size before deploying.)
unc_levels <- list(
  "90" = list(label = "90% interval (5th-95th percentile)",  short = "90% interval", lo_q = 0.05, hi_q = 0.95),
  "50" = list(label = "50% interval (25th-75th percentile)", short = "50% interval", lo_q = 0.25, hi_q = 0.75)
)

quantiles <- ncvar_get(nc, "quantile")
q_idx <- function(q) {
  i <- which(abs(quantiles - q) < 1e-9)
  stopifnot(length(i) == 1)
  i
}

lon <- ncvar_get(nc, "longitude")
lat <- ncvar_get(nc, "latitude")
i_loc <- ncvar_get(nc, "i_location")
j_loc <- ncvar_get(nc, "j_location")
valid_idx <- which(!is.na(i_loc) & !is.na(j_loc))
time_steps <- 108

# Identical to the original processing (rotate, set extent, flip v then h).
make_stack <- function(data_q) {
  layers <- lapply(seq_len(time_steps), function(t) {
    mat <- matrix(NA, nrow = 83, ncol = 77)
    mat[cbind(i_loc[valid_idx], j_loc[valid_idx])] <- data_q[valid_idx, t]
    r <- rast(t(mat)[, nrow(mat):1])
    ext(r) <- ext(min(lon), max(lon), min(lat), max(lat))
    crs(r) <- "EPSG:4326"
    r
  })
  s <- rast(layers)
  names(s) <- format(seq(as.Date("2014-01-01"), by = "month", length.out = time_steps), "%Y-%m")
  s <- flip(s, direction = "v")
  flip(s, direction = "h")
}

# Sanity check: rebuilding the median must reproduce your existing tif.
chk <- make_stack(ncvar_get(nc, vars[1])[, q_idx(0.5), ])
ref <- rast(file.path("data/processed_rasters", paste0(vars[1], ".tif")))
stopifnot(isTRUE(all.equal(as.numeric(values(chk)), as.numeric(values(ref)), tolerance = 1e-4)))
message("Alignment check passed for ", vars[1])

for (lvl in names(unc_levels)) for (side in c("lo", "hi")) {
  dir.create(file.path("data/unc", lvl, side), recursive = TRUE, showWarnings = FALSE)
}
unc_range_list <- setNames(lapply(unc_levels, function(x) list()), names(unc_levels))

for (v in vars) {
  message("Processing: ", v)
  arr <- ncvar_get(nc, v)                    # [site, quantile, time]
  med <- arr[, q_idx(0.5), ]
  for (lvl in names(unc_levels)) {
    cfg <- unc_levels[[lvl]]
    lo <- arr[, q_idx(cfg$lo_q), ]
    hi <- arr[, q_idx(cfg$hi_q), ]
    
    viol <- mean(lo > med + 1e-9 | hi < med - 1e-9, na.rm = TRUE)
    if (is.finite(viol) && viol > 0.001) warning(v, " [", lvl, "]: ", round(100 * viol, 2), "% of values have median outside the bounds")
    
    # legend range for the "uncertainty width" map layer: 0 to the 99th percentile of widths
    unc_range_list[[lvl]][[v]] <- c(0, as.numeric(quantile(hi - lo, 0.99, na.rm = TRUE)))
    
    for (side in c("lo", "hi")) {
      writeRaster(make_stack(if (side == "lo") lo else hi),
                  file.path("data/unc", lvl, side, paste0(v, ".tif")),
                  overwrite = TRUE, datatype = "FLT4S", gdal = c("COMPRESS=LZW"))
    }
  }
}

file.copy("data/processed_data.Rds", "data/processed_data_backup.Rds", overwrite = FALSE)
dat$rast_list      <- NULL   # SpatRaster objects don't survive saveRDS; the app reads the tifs
dat$unc_levels     <- unc_levels
dat$unc_range_list <- unc_range_list
saveRDS(dat, "data/processed_data.Rds")

nc_close(nc)
message("Done. Total size of data/unc: ",
        round(sum(file.size(list.files("data/unc", recursive = TRUE, full.names = TRUE))) / 1e6), " MB")