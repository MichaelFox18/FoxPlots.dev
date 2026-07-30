# ============================================================
# helpers_examples.R -- the built-in example datasets
# ============================================================
# ONE source of truth: key -> {label, tags, build}. The Import module derives
# BOTH the menu label and the data from the same lookup, so a launcher can no
# longer advertise a key it cannot load (before 0.11.0 the label vector and the
# data list were two unconnected arguments kept in sync by hand -- a mismatch
# loaded nothing while still announcing "Loaded example"). Same shape as
# MAP_BUILTIN_BOUNDARIES in helpers_map.R.
#
# `build` is a CLOSURE, never data: the registry costs a list of functions at
# package load, and a dataset is materialized only when the user clicks
# "Load example". Builders that SLICE a bigger frame must reset rownames -- a
# subset of a data frame keeps its source row labels, and those leak into DT,
# the exporters and column_profile(). Builders must never return list columns
# (DT / writexl / the profile all assume atomic vectors).
#
# Labels are plain ASCII: they are echoed into man/ by foxplots_examples().

# A NAMED function, not an inline closure, on purpose: R CMD check's
# "dependencies in R code" pass walks namespace-level function bodies but not
# functions stored inside a top-level list, so without at least one plain
# `datasets::` call the declared Import gets a "not imported from" NOTE.
example_mtcars <- function() {
  d <- as.data.frame(datasets::mtcars)
  d$car <- rownames(d)
  rownames(d) <- NULL
  d
}

EXAMPLE_SETS <- list(

  # ---- teaching classics: a number across groups ---------------------------
  iris = list(
    label = "iris (150 flowers, 3 species)",
    tags  = c("compare", "regression", "summarize", "visualize"),
    build = function() datasets::iris),

  mtcars = list(
    label = "mtcars (32 cars, numeric + coded factors)",
    tags  = c("compare", "regression", "summarize", "reshape", "combine"),
    build = example_mtcars),

  toothgrowth = list(
    label = "ToothGrowth (60 guinea pigs, 2 x 3 factorial)",
    tags  = c("compare", "regression", "summarize"),
    # dose stays numeric on purpose -> the Change Variable Types demo
    build = function() datasets::ToothGrowth),

  plantgrowth = list(
    label = "PlantGrowth (30 plants, 3 treatments)",
    tags  = c("compare", "summarize"),
    build = function() datasets::PlantGrowth),

  insectsprays = list(
    label = "InsectSprays (72 counts, 6 sprays)",
    tags  = c("compare", "glmm", "summarize"),
    build = function() datasets::InsectSprays),

  chickwts = list(
    label = "chickwts (71 chicks, 6 feeds)",
    tags  = c("compare", "summarize"),
    build = function() datasets::chickwts),

  warpbreaks = list(
    label = "warpbreaks (54 looms, wool x tension)",
    tags  = c("compare", "glmm", "summarize"),
    build = function() datasets::warpbreaks),

  npk = list(
    label = "npk (24 plots, classic RCBD)",
    tags  = c("lmer", "compare"),
    build = function() datasets::npk),

  # ---- two categorical variables (chi-square) ------------------------------
  titanic = list(
    label = "Titanic (2,201 people, 4 categories)",
    tags  = c("compare", "summarize", "regression", "glmm"),
    build = function() {
      t <- as.data.frame(datasets::Titanic)          # 32 aggregated rows
      d <- t[rep(seq_len(nrow(t)), t$Freq), 1:4]     # one row per person
      rownames(d) <- NULL
      d$survived01 <- as.integer(d$Survived == "Yes")  # 0/1 for logistic and
      d                                                # the Binary GLMM tab
    }),

  mpg = list(
    label = "mpg (234 cars: class, drv, cty, hwy)",
    tags  = c("compare", "regression", "visualize", "summarize"),
    build = function() as.data.frame(ggplot2::mpg)),

  starwars = list(
    label = "starwars (87 characters, lots of gaps)",
    tags  = c("compare", "summarize", "clean"),
    build = function() {
      d <- as.data.frame(dplyr::starwars)
      # films / vehicles / starships are LIST columns.
      d[, !vapply(d, is.list, logical(1)), drop = FALSE]
    }),

  # ---- repeated measures / grouping structure ------------------------------
  chickweight = list(
    label = "ChickWeight (578 weights, 50 chicks, 4 diets)",
    tags  = c("lmer", "visualize", "summarize"),
    build = function() {
      d <- as.data.frame(datasets::ChickWeight)
      # Chick is an ORDERED factor with 50 levels; ordered factors sort and
      # contrast-code differently everywhere downstream. Flatten both.
      d$Chick <- as.character(d$Chick)
      d$Diet  <- as.character(d$Diet)
      d
    }),

  co2 = list(
    label = "CO2 (84 readings, 12 plants, 2 x 2 design)",
    tags  = c("lmer", "visualize"),
    build = function() {
      d <- as.data.frame(datasets::CO2)
      d$Plant <- as.character(d$Plant)               # ordered factor -> text
      d
    }),

  # ---- dates and time series ----------------------------------------------
  daily_weather = list(
    label = "Daily weather (3 stations x 365 days)",
    tags  = c("visualize", "summarize", "compare", "dates"),
    build = function() make_timeseries_example_data()),

  economics = list(
    label = "US economics (574 months, 1967-2015)",
    tags  = c("visualize", "dates"),
    build = function() as.data.frame(ggplot2::economics)),

  airquality = list(
    label = "airquality (153 days, real gaps)",
    tags  = c("clean", "compare", "regression", "visualize", "dates"),
    build = function() {
      d <- datasets::airquality                      # May-Sep 1973
      d$date  <- as.Date(sprintf("1973-%02d-%02d", d$Month, d$Day))
      d$month <- factor(month.abb[d$Month], levels = month.abb)
      d[, c("date", "month", "Ozone", "Solar.R", "Wind", "Temp")]
    }),

  # ---- deliberately dirty --------------------------------------------------
  messy_survey = list(
    label = "Messy field survey (every Data Health issue)",
    tags  = c("clean", "compare", "reshape"),
    build = function() make_messy_example_data()),

  # ---- wide / long shapes for Reshape --------------------------------------
  relig_income = list(
    label = "relig_income (wide: one column per income band)",
    tags  = c("reshape"),
    build = function() as.data.frame(tidyr::relig_income)),

  fish_encounters = list(
    label = "fish_encounters (long: station x fish)",
    tags  = c("reshape"),
    build = function() as.data.frame(tidyr::fish_encounters)),

  billboard = list(
    label = "billboard (wide: 76 week columns + a date)",
    tags  = c("reshape", "clean", "dates"),
    build = function() as.data.frame(tidyr::billboard)),

  us_rent_income = list(
    label = "us_rent_income (long: 2 measures per state)",
    tags  = c("reshape"),
    build = function() as.data.frame(tidyr::us_rent_income)),

  us_rent_wide = list(
    label = "US rent + income by state (map-ready)",
    tags  = c("map", "reshape"),
    build = function() as.data.frame(tidyr::pivot_wider(
      tidyr::us_rent_income, id_cols = c("GEOID", "NAME"),
      names_from = "variable", values_from = "estimate"))),

  txhousing = list(
    label = "Texas housing (6 cities x 187 months)",
    tags  = c("reshape", "summarize", "clean", "visualize"),
    build = function() {
      d <- as.data.frame(ggplot2::txhousing)
      # A fixed 6-city slice: 1,122 rows instead of 8,602, deterministic with
      # no RNG, and it keeps the real missing-value pattern.
      d <- d[d$city %in% c("Austin", "Dallas", "El Paso", "Fort Worth",
                           "Houston", "San Antonio"), ]
      rownames(d) <- NULL
      d
    }),

  # ---- wide-range numeric --------------------------------------------------
  diamonds = list(
    label = "diamonds (5,394 gems, 1-in-10 sample)",
    tags  = c("visualize", "regression", "summarize"),
    build = function() {
      d <- as.data.frame(ggplot2::diamonds)
      # Systematic every-10th row: deterministic (no set.seed / snapshot_rng
      # dance), keeps all 5 cut and 8 clarity levels and the full price range.
      # Same idiom as helpers_model.R's diagnostic thinning.
      d <- d[seq(1, nrow(d), by = 10), ]
      rownames(d) <- NULL
      # cut / color / clarity are ORDERED factors -> plain (keep level order).
      d[] <- lapply(d, function(x)
        if (is.ordered(x)) factor(x, ordered = FALSE) else x)
      d
    }),

  # ---- maps ---------------------------------------------------------------
  sites = list(
    label = "Florida research sites (120 points)",
    tags  = c("map", "compare", "summarize"),
    build = function() make_map_example_data()),

  quakes = list(
    label = "Fiji earthquakes (1,000 points)",
    tags  = c("map", "visualize"),
    build = function() as.data.frame(datasets::quakes)),

  storms = list(
    label = "Atlantic storm tracks (2021-2022 fixes)",
    tags  = c("map", "visualize"),
    build = function() {
      d <- as.data.frame(dplyr::storms)
      d <- d[d$year >= 2021, ]                       # fixed cut, no RNG
      rownames(d) <- NULL
      d
    }),

  # ---- mixed models --------------------------------------------------------
  rcbd = list(
    label = "RCBD example (3-factor, 108 plots)",
    tags  = c("lmer", "compare", "regression"),
    build = function() make_example_data()),

  glmm = list(
    label = "Field example (counts, proportions, binary, grouped)",
    tags  = c("glmm"),
    build = function() make_glmm_example_data()),

  # ---- two-table (Combine) -------------------------------------------------
  band_members = list(
    label = "band_members (name, band)",
    tags  = c("combine"),
    build = function() as.data.frame(dplyr::band_members)),

  band_instruments = list(
    label = "band_instruments (name, plays)",
    tags  = c("combine"),
    build = function() as.data.frame(dplyr::band_instruments)),

  mtcars_top = list(
    label = "mtcars (rows 1-16)",
    tags  = c("combine"),
    build = function() utils::head(EXAMPLE_SETS$mtcars$build(), 16)),

  mtcars_bottom = list(
    label = "mtcars (rows 17-32)",
    tags  = c("combine"),
    build = function() {
      d <- utils::tail(EXAMPLE_SETS$mtcars$build(), 16)
      rownames(d) <- NULL                              # tail() keeps "17".."32"
      d
    })
)

# Each app's Import menu, in menu order (the first key is the default
# selection). EXPLICIT lists, not tag queries: tags carry no order, and a tag
# query would mean adding one dataset silently rewrites four apps' menus (and
# could drop a 5,000-row set into an app that was never sized for it). Tags
# stay as metadata for foxplots_examples() and for spotting coverage gaps.
EXAMPLES_DEFAULT <- c("mtcars", "relig_income", "fish_encounters")

EXAMPLES_EXPLORER <- c("mtcars", "mpg", "iris", "toothgrowth", "airquality",
                       "daily_weather", "economics", "chickweight", "titanic",
                       "diamonds", "txhousing", "relig_income",
                       "fish_encounters", "sites", "messy_survey")

EXAMPLES_RESHAPE <- c("relig_income", "billboard", "fish_encounters",
                      "us_rent_income", "txhousing", "mtcars", "messy_survey")

EXAMPLES_COMBINE <- c("band_members", "band_instruments",
                      "mtcars_top", "mtcars_bottom")

EXAMPLES_COMPARE <- c("iris", "toothgrowth", "plantgrowth", "insectsprays",
                      "warpbreaks", "chickwts", "mpg", "titanic",
                      "airquality", "daily_weather", "mtcars", "starwars",
                      "messy_survey")

EXAMPLES_REGRESSION <- c("mpg", "iris", "mtcars", "titanic", "diamonds",
                         "airquality", "toothgrowth", "rcbd")

EXAMPLES_LMER <- c("rcbd", "chickweight", "co2", "npk")

EXAMPLES_GLMM <- c("glmm", "insectsprays", "warpbreaks", "titanic")

EXAMPLES_MAP <- c("sites", "quakes", "storms", "us_rent_wide")

# Turn whatever a caller passed into a key -> spec list. Accepts:
#   * NULL                -> EXAMPLES_DEFAULT
#   * a character vector  -> registry keys (the launcher path)
#   * a named list        -> ad-hoc data frames or zero-arg builders
#                            (test fixtures; keeps testServer callers working)
# An unknown key stops HERE, at UI-build / server-init time, so a typo fails
# in test-launchers.R's "every launcher builds" pass instead of producing a
# menu entry that loads nothing.
as_example_spec <- function(examples = NULL) {
  if (is.null(examples)) examples <- EXAMPLES_DEFAULT
  if (is.character(examples)) {
    unknown <- setdiff(examples, names(EXAMPLE_SETS))
    if (length(unknown))
      stop("Unknown example key(s): ", paste(unknown, collapse = ", "),
           ". See foxplots_examples().", call. = FALSE)
    return(EXAMPLE_SETS[examples])
  }
  if (is.list(examples) && length(examples) &&
      !is.null(names(examples)) && all(nzchar(names(examples)))) {
    keys <- names(examples)
    return(lapply(stats::setNames(keys, keys), function(k) {
      v <- examples[[k]]
      list(label = k, tags = character(0),
           build = if (is.function(v)) v else function() v)
    }))
  }
  stop("`examples` must be example keys or a named list of data frames.",
       call. = FALSE)
}

# label -> key, exactly the selectInput shape. Mirrors map_boundary_choices().
example_menu <- function(spec) {
  stats::setNames(names(spec), vapply(spec, `[[`, character(1), "label"))
}

# Build ONE example, or stop() with a message the module can show. This is the
# guard for the old bug: examples[[input$example]] used to return NULL and the
# app still announced "Loaded example: <key>".
example_build <- function(spec, key) {
  entry <- spec[[key]]
  if (is.null(entry))
    stop("No example named \"", key, "\".", call. = FALSE)
  d <- entry$build()
  if (!is.data.frame(d) || !nrow(d) || !ncol(d))
    stop("The example \"", key, "\" did not produce a usable table.",
         call. = FALSE)
  d
}

#' Catalogue of the built-in example datasets
#'
#' Every dataset the apps offer on their Import tab, with the key you pass to
#' [foxplots_example()] and the menu label the apps show. Building the
#' catalogue does not build any data.
#'
#' @return A data frame with columns key, label and tags.
#' @examples
#' head(foxplots_examples())
#' @export
foxplots_examples <- function() {
  data.frame(
    key   = names(EXAMPLE_SETS),
    label = vapply(EXAMPLE_SETS, `[[`, character(1), "label"),
    tags  = vapply(EXAMPLE_SETS,
                   function(e) paste(e$tags, collapse = ", "), character(1)),
    row.names = NULL, stringsAsFactors = FALSE)
}

#' Load one built-in example dataset
#'
#' @param key One of `foxplots_examples()$key`.
#' @return A data frame.
#' @examples
#' str(foxplots_example("toothgrowth"))
#' @export
foxplots_example <- function(key) {
  stopifnot(is.character(key), length(key) == 1L)
  example_build(as_example_spec(key), key)
}

# ---------------------------------------------------------------------------
#  The two generated sets that no stock frame covers
# ---------------------------------------------------------------------------

#' Generate the built-in daily time-series example dataset
#'
#' A seeded year of daily observations at three research stations, used as the
#' demo for date axes: 365 distinct dates (enough to crowd an x axis), a
#' 3-level station factor, and three measures with a clear seasonal signal --
#' temperature (sinusoidal), rainfall (right-skewed with a wet season and many
#' zero days), and NDVI greenness (seasonal plus a slow upward trend). The
#' date is carried BOTH as a real Date and as ISO text, so the Data Health
#' "dates stored as text" fix has something to convert.
#'
#' @return A data frame with date, date_text, station, month, temp_c, rain_mm
#'   and ndvi (1,095 rows).
#' @examples
#' d <- make_timeseries_example_data()
#' nrow(d)
#' @export
make_timeseries_example_data <- function() {
  restore_rng <- snapshot_rng()   # fixed seed must NOT hijack the session RNG
  on.exit(restore_rng(), add = TRUE)
  set.seed(20240101)
  dates    <- seq(as.Date("2023-01-01"), as.Date("2023-12-31"), by = "day")
  stations <- c("Gainesville", "Immokalee", "Quincy")
  d <- expand.grid(date = dates, station = stations,
                   KEEP.OUT.ATTRS = FALSE, stringsAsFactors = FALSE)
  d <- d[order(d$station, d$date), ]
  rownames(d) <- NULL
  n   <- nrow(d)
  doy <- as.integer(format(d$date, "%j"))
  # Seasonal phase: peak mid-July, trough mid-January.
  season  <- sin(2 * pi * (doy - 105) / 365)
  lat_off <- c(Gainesville = 0, Immokalee = 4.5, Quincy = -1.5)[d$station]

  d$temp_c <- round(22 + 6.5 * season + unname(lat_off) +
                      stats::rnorm(n, 0, 1.8), 1)

  # Rainfall: right-skewed, wetter in summer, with a realistic run of dry days
  # -- a good log-scale / histogram demo and a non-normal Compare Groups case.
  wet <- stats::plogis(4 * (season - 0.15))
  d$rain_mm <- round(stats::rgamma(n, shape = 0.55, scale = 6 + 26 * wet), 1)
  d$rain_mm[stats::rbinom(n, 1, 0.45 - 0.25 * wet) == 1] <- 0

  # NDVI: bounded 0-1 greenness, lagged behind temperature, slight trend.
  d$ndvi <- round(pmin(pmax(0.52 + 0.16 * sin(2 * pi * (doy - 135) / 365) +
                              0.00018 * doy + stats::rnorm(n, 0, 0.025),
                            0.05), 0.95), 3)

  # A 12-level, correctly ORDERED group-by column (a plain factor, not an
  # ordered one -- ordered factors contrast-code differently in the models).
  d$month     <- factor(month.abb[as.integer(format(d$date, "%m"))],
                        levels = month.abb)
  d$date_text <- format(d$date, "%Y-%m-%d")
  d[, c("date", "date_text", "station", "month", "temp_c", "rain_mm", "ndvi")]
}

#' Generate the built-in messy example dataset
#'
#' A seeded, deliberately dirty field survey used as the Data Health demo:
#' every issue detect_issues() knows how to find is present at least once --
#' a blank column header, padded category values, "N/A" placeholders, currency
#' stored as text, ISO dates stored as text, an entirely blank column, an
#' entirely blank row, two exact duplicate rows, and three extreme outliers.
#' Two problems are included that the engine deliberately does NOT fix: real
#' missing values in moisture_pct, and case drift in crop (fixable via
#' Reshape or a filter, and visible in the Column profile).
#'
#' @return A data frame with 123 rows and 9 columns (one header is blank);
#'   applying every Data Health fix leaves 120 rows.
#' @examples
#' d <- make_messy_example_data()
#' nrow(d)
#' @export
make_messy_example_data <- function() {
  restore_rng <- snapshot_rng()   # fixed seed must NOT hijack the session RNG
  on.exit(restore_rng(), add = TRUE)
  set.seed(1234)
  n <- 120L
  # Whitespace: the SAME three treatments, padded four different ways.
  treat <- sprintf(sample(c("%s", " %s", "%s ", "  %s"), n, replace = TRUE),
                   rep(c("Control", "Low N", "High N"), length.out = n))
  # Case drift: no clean_spec fixes this, on purpose.
  crop  <- sample(c("Peanut", "peanut", "PEANUT", "Sweet corn", "sweet corn"),
                  n, replace = TRUE)
  yield <- round(stats::rnorm(n, 52, 6), 1)
  yield[c(7, 41, 98)] <- c(210, 245, 3)              # far outside 3 x IQR
  moist <- round(stats::runif(n, 12, 30), 1)
  moist[c(3, 19, 55, 77, 101, 110)] <- NA_real_      # honest missing values
  price <- sprintf("$%s", formatC(round(stats::runif(n, 800, 9000)),
                                  big.mark = ",", format = "d"))
  price[c(5, 22)] <- "N/A"                           # placeholder marker

  d <- data.frame(
    plot_id      = sprintf("%05d", seq_len(n)),      # leading zeros -> text
    site         = sample(c("North", "South", "East"), n, replace = TRUE),
    treatment    = treat,
    crop         = crop,
    sample_date  = format(as.Date("2024-03-01") + sample(0:180, n, TRUE),
                          "%Y-%m-%d"),
    yield_kg     = yield,
    moisture_pct = moist,
    price_usd    = price,
    notes        = rep("", n),                       # entirely blank column
    stringsAsFactors = FALSE)

  d <- rbind(d, d[c(2, 30), ])                       # 2 exact duplicate rows
  blank <- d[1, ]; blank[] <- NA
  for (nm in names(blank))
    if (is.character(blank[[nm]])) blank[[nm]] <- ""
  d <- rbind(d, blank)                               # 1 entirely blank row
  rownames(d) <- NULL
  # A blank header, renamed LAST: the `names` fix runs first in clean_specs()
  # order, so the by-name fixes below it reach this column once names are made
  # unique. Ship a blank header, never a DUPLICATE one -- df[["dup"]] silently
  # resolves to the first match, so a duplicate would escape by-name fixes.
  names(d)[names(d) == "notes"] <- ""
  d
}
