# Dev-time builder for the package's built-in boundary files.
# Sources (both public domain):
#   * US states / counties -- US Census Bureau cartographic boundary files,
#     1:20,000,000 (the smallest published), a US Government work (17 USC 105).
#   * World countries -- Natural Earth 1:110m admin-0, explicitly public domain.
# sf is used HERE ONLY (dev time) to read shapefiles; the package itself never
# depends on it. Output: compact gzipped GeoJSON with a handful of properties
# and coordinates rounded to 4 dp (~11 m), plenty for choropleth shading.
suppressPackageStartupMessages({library(sf); library(jsonlite)})

WD  <- "/private/tmp/claude-501/-Users-mikemikemike-Desktop-FoxPlots/f38da0bd-6c5c-479b-a81d-719b912a2cf1/scratchpad/geo"
OUT <- "/Users/mikemikemike/Desktop/FoxPlots/inst/geo"
dir.create(OUT, showWarnings = FALSE, recursive = TRUE)
setwd(WD)

fetch_zip <- function(url, stem) {
  z <- file.path(WD, paste0(stem, ".zip"))
  if (!file.exists(z)) download.file(url, z, quiet = TRUE)
  d <- file.path(WD, stem)
  if (!dir.exists(d)) unzip(z, exdir = d)
  list.files(d, pattern = "\\.shp$", full.names = TRUE)[1]
}

# Write an sf object as gzipped GeoJSON carrying only `keep` properties.
write_geo <- function(x, keep, file, digits = 4) {
  x <- x[, keep]
  x <- st_zm(x, drop = TRUE)
  tmp <- tempfile(fileext = ".geojson")
  suppressWarnings(st_write(x, tmp, driver = "GeoJSON", quiet = TRUE,
                            layer_options = paste0("COORDINATE_PRECISION=", digits)))
  txt <- paste(readLines(tmp, warn = FALSE), collapse = "\n")
  con <- gzfile(file.path(OUT, file), "wt")
  writeLines(txt, con)
  close(con)
  cat(sprintf("%-26s %5d features  %7.1f KB (gz)\n", file, nrow(x),
              file.size(file.path(OUT, file)) / 1024))
}

## --- US states -------------------------------------------------------------
st_shp <- fetch_zip(
  "https://www2.census.gov/geo/tiger/GENZ2023/shp/cb_2023_us_state_20m.zip",
  "cb_state")
states <- st_read(st_shp, quiet = TRUE)
states$state  <- states$NAME
states$abbr   <- states$STUSPS
states$fips   <- states$STATEFP
write_geo(states, c("state", "abbr", "fips"), "us_states.geojson.gz")

## --- US counties -----------------------------------------------------------
co_shp <- fetch_zip(
  "https://www2.census.gov/geo/tiger/GENZ2023/shp/cb_2023_us_county_20m.zip",
  "cb_county")
counties <- st_read(co_shp, quiet = TRUE)
lut <- stats::setNames(as.character(states$NAME), as.character(states$STATEFP))
counties$county <- counties$NAME            # bare name, e.g. "Roanoke"
counties$state  <- unname(lut[as.character(counties$STATEFP)])
# Two levels of collision to survive: county names repeat across states (31
# Washingtons), and within Virginia/Missouri/Maryland an independent CITY can
# share its name with a neighbouring county ("Roanoke County" vs "Roanoke
# city"). NAMELSAD carries the Census type suffix that separates them, so the
# unique label is NAMELSAD + state. Bare `county` stays for users matching
# plain names inside one state (with the state filter on), and `fips` is the
# always-unique key.
counties$county_full  <- counties$NAMELSAD
counties$county_state <- paste0(counties$NAMELSAD, ", ", counties$state)
counties$fips <- counties$GEOID
counties <- counties[!is.na(counties$state), ]
stopifnot(!anyDuplicated(counties$county_state), !anyDuplicated(counties$fips))
write_geo(counties, c("county", "county_full", "state", "county_state", "fips"),
          "us_counties.geojson.gz")

## --- World countries -------------------------------------------------------
ne <- st_read(file.path(WD, "ne_countries.geojson"), quiet = TRUE)
ne$country <- ne$NAME_LONG
ne$iso_a3  <- ne$ADM0_A3
ne$iso_a2  <- ne$ISO_A2_EH
ne$continent <- ne$CONTINENT
write_geo(ne, c("country", "iso_a3", "iso_a2", "continent"),
          "world_countries.geojson.gz")

cat("\ntotal inst/geo:",
    round(sum(file.size(list.files(OUT, full.names = TRUE))) / 1024, 1), "KB\n")
