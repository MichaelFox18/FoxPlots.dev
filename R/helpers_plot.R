# ============================================================
# helpers_plot.R -- chart building, suitability hints & code gen
# ============================================================
# Pure plotting logic lifted from the original Data Explorer: one
# build_full_plot() used by every plot slot, the chart-suitability
# hints, the palette scales, and a code generator that emits a runnable
# ggplot2 snippet for the same settings. mod_visualize.R is a thin
# (namespaced) wrapper over these.
#
# REQUIRES ggplot2 to be attached by the caller (the apps do). dplyr is
# namespace-qualified, so only ggplot2 needs to be on the search path.
# UF_BLUE / UF_ORANGE come from components.R.

# Above this many rows the Visualize tab renders static (fast) plots instead of
# interactive plotly ones -- ggplotly() gets slow well before a few thousand
# marks and can make the single-threaded app appear frozen.
BIG_ROWS   <- 1000
PIE_MAX    <- 12   # pie slices beyond this are grouped into "Other"
BREWER_MAX <- 8    # ColorBrewer Set1/Set2 run out past ~8 levels -> viridis
BAR_MAX    <- 30   # bars beyond this are grouped into "Other"
# Discrete "Color / group by" levels beyond this are dropped. Cost is LINEAR in
# the level count -- measured on 11,000 rows: 30 levels 0.4s, 500 levels 4.9s,
# 11,000 levels 111.7s, which freezes the single-threaded app for two minutes.
# Suppressing the legend saves nothing (it is per-level geom/scale work, not
# legend construction), so the only fix is to stop grouping. 50 keeps ordinary
# categorical work (a 40-county scatter costs 0.3s) and cuts the pathological
# case. Continuous numeric colors are NOT capped -- they use a single gradient
# scale and stay fast at any cardinality.
GROUP_MAX  <- 50

# Maps the 0.5-5 size slider onto a sensible 0.2-0.9 bar width.
bar_width <- function(size) 0.2 + (size - 0.5) / 4.5 * 0.7

# Picks an aggregation function for value bar charts.
agg_fun <- function(agg) switch(agg %||% "sum",
  mean   = function(z) mean(z, na.rm = TRUE),
  median = function(z) stats::median(z, na.rm = TRUE),
  function(z) sum(z, na.rm = TRUE))

theme_call <- function(name, base_size = 13) {
  fn <- switch(name %||% "minimal",
    minimal = theme_minimal, classic = theme_classic,
    light   = theme_light,   bw      = theme_bw,
    dark    = theme_dark,    theme_minimal)
  fn(base_size = base_size)
}

# Column-type predicates.
is_discrete_col <- function(x) is.character(x) || is.factor(x) || is.logical(x)
is_date_col     <- function(x) inherits(x, c("Date", "POSIXct", "POSIXt"))

# A discrete x-axis with many or long labels gets its ticks angled so they stay
# legible instead of overlapping into mush.
needs_x_rotation <- function(df, pt, xv) {
  if (is.null(xv) || pt == "pie" || !xv %in% names(df)) return(FALSE)
  x <- df[[xv]]
  if (!(pt %in% c("bar", "boxplot") || is_discrete_col(x))) return(FALSE)
  uvals <- unique(as.character(x))
  length(uvals) > 8 || max(nchar(uvals), 0L) > 10
}

# Keep the top `n_keep` categories (by count, or by summed weight `w` when a Y
# variable is present) and roll everything else into a single "Other" bar.
lump_bar_x <- function(df, xv, w, n_keep) {
  x   <- as.character(df[[xv]])
  wt  <- if (is.null(w)) rep(1, length(x)) else w
  tot <- sort(tapply(wt, x, function(z) sum(z, na.rm = TRUE)), decreasing = TRUE)
  keep <- names(tot)[seq_len(min(n_keep, length(tot)))]
  x[!x %in% keep] <- "Other"
  df[[xv]] <- factor(x, levels = unique(c(keep, "Other")))
  df
}

# Returns an HTML warning when the chosen variable doesn't suit the chosen chart
# type, or NULL when the pairing is fine. Shown inline under the variable
# pickers so students learn *why* a chart looks off.
chart_hint <- function(df, p) {
  if (is.null(df) || is.null(p$type)) return(NULL)
  if (identical(p$type, "heatmap")) {
    nums <- names(df)[vapply(df, is.numeric, logical(1))]
    sel  <- intersect(p$corr_vars %||% character(0), nums)
    if (length(nums) < 2)
      return("A correlation heatmap needs at least <b>2 numeric columns</b> in the data.")
    if (length(sel) < 2)
      return("Pick at least <b>2 numeric variables</b> to build the correlation heatmap.")
    return(NULL)
  }
  if (is.null(p$x) || !nzchar(p$x)) return(NULL)
  if (!p$x %in% names(df)) return(NULL)
  pt <- p$type
  xv <- p$x
  x  <- df[[xv]]
  n_x    <- dplyr::n_distinct(x, na.rm = TRUE)
  cont_x <- is.numeric(x) && !is_date_col(x) && n_x > 10

  if (pt %in% c("scatter", "line") && is_discrete_col(x))
    return(sprintf("<b>%s</b> is categorical. %s charts read best with a numeric or date X &mdash; a <b>box plot</b> or <b>bar chart</b> may show this better.",
                   xv, tools::toTitleCase(pt)))
  if (pt == "bar" && cont_x)
    return(sprintf("<b>%s</b> looks continuous (%s distinct values), so a bar chart draws many thin bars. A <b>histogram</b> is usually the better choice for a numeric variable.",
                   xv, format(n_x, big.mark = ",")))
  if (pt %in% c("boxplot", "violin", "meanerror") && cont_x)
    return(sprintf("<b>%s</b> looks continuous, so you'll get one group per value. This chart groups a numeric Y by a <b>categorical</b> X.", xv))
  if (pt == "density" && is_discrete_col(x))
    return(sprintf("<b>%s</b> is categorical. A density plot needs a <b>numeric</b> X &mdash; for categories a <b>bar chart</b> reads better.", xv))
  if (pt == "hexbin" && is_discrete_col(x))
    return(sprintf("<b>%s</b> is categorical. Hexbin bins a dense <b>numeric</b> X against a numeric Y &mdash; try a <b>bar chart</b> for categories.", xv))
  if (pt == "pie" && cont_x)
    return(sprintf("<b>%s</b> looks continuous, which makes an unreadable pie. Pie charts need a <b>categorical</b> variable with a handful of values.", xv))
  # Colouring by a high-cardinality column asks for one geom and one legend key
  # per level, which freezes the app long before it finishes. build_full_plot()
  # drops the grouping at GROUP_MAX; say so, and name the way out.
  cv <- if (!is.null(p$color) && nzchar(p$color) && p$color != "__none__" &&
            p$color %in% names(df)) p$color else NULL
  if (!is.null(cv) && !pt %in% c("pie", "hexbin", "heatmap") &&
      !is.numeric(df[[cv]])) {
    # na.rm = FALSE deliberately, matching build_full_plot and generate_code:
    # ggplot draws NA as its own level with its own legend key (verified), so
    # a missing value really does cost one of the GROUP_MAX slots. Counting it
    # here too keeps all three in step -- with na.rm = TRUE a column of exactly
    # 50 real levels plus any NA lost its colouring with no explanation.
    n_cv <- dplyr::n_distinct(df[[cv]])
    if (n_cv > GROUP_MAX)
      # Do NOT suggest faceting here: the facet picker only offers columns with
      # 30 or fewer levels (mod_visualize's cols_cat), so any column that trips
      # this cap is not offered there either -- advice the user cannot follow.
      return(sprintf("<b>%s</b> has %s different values &mdash; too many to colour by (limit %d), so the colouring is turned off. Pick a column with fewer categories, or use <b>Filter rows</b> on the Import tab to narrow the data first.",
                     cv, format(n_cv, big.mark = ","), GROUP_MAX))
  }
  barlim <- p$cat_limit %||% BAR_MAX
  if (pt == "bar" && is_discrete_col(x) && n_x > barlim)
    return(sprintf("<b>%s</b> has %s categories; only the largest %d are shown (the rest grouped as &ldquo;Other&rdquo;). Use the &ldquo;Maximum bars&rdquo; slider to show more or fewer.",
                   xv, format(n_x, big.mark = ","), barlim))
  NULL
}

# --- Palettes ---------------------------------------------------------------

PALETTES <- c("Automatic" = "auto", "UF Brand" = "uf", "Viridis" = "viridis",
              "Colorblind-safe" = "cb", "ColorBrewer Set1" = "set1",
              "ColorBrewer Set2" = "set2", "Greyscale" = "greys")

okabe_ito  <- function(n) rep_len(
  c("#E69F00", "#56B4E9", "#009E73", "#F0E442",
    "#0072B2", "#D55E00", "#CC79A7", "#000000"), n)

uf_discrete <- function(n) {
  base <- c(UF_BLUE, UF_ORANGE, "#2ca25f", "#8856a7",
            "#e6550d", "#3182bd", "#31a354", "#756bb1")
  if (n <= length(base)) base[seq_len(n)]
  else grDevices::colorRampPalette(c(UF_BLUE, UF_ORANGE))(n)
}

# color+fill scales for the chosen group palette (both aesthetics returned).
group_scales <- function(df, cv, palette) {
  is_cont <- is.numeric(df[[cv]]) && dplyr::n_distinct(df[[cv]]) > 10
  n       <- dplyr::n_distinct(df[[cv]])
  if (palette == "auto" || is.null(palette)) {
    if (is_cont)          return(list(scale_color_viridis_c(), scale_fill_viridis_c()))
    if (n > BREWER_MAX)   return(list(scale_color_viridis_d(), scale_fill_viridis_d()))
    return(list(scale_color_brewer(palette = "Set1"), scale_fill_brewer(palette = "Set1")))
  }
  if (is_cont) {
    if (palette == "uf")
      return(list(scale_color_gradient(low = UF_BLUE, high = UF_ORANGE),
                  scale_fill_gradient(low = UF_BLUE, high = UF_ORANGE)))
    return(list(scale_color_viridis_c(), scale_fill_viridis_c()))
  }
  if (palette %in% c("set1", "set2") && n > BREWER_MAX)
    return(list(scale_color_viridis_d(), scale_fill_viridis_d()))
  switch(palette,
    uf      = list(scale_color_manual(values = uf_discrete(n)),
                   scale_fill_manual(values  = uf_discrete(n))),
    viridis = list(scale_color_viridis_d(), scale_fill_viridis_d()),
    cb      = list(scale_color_manual(values = okabe_ito(n)),
                   scale_fill_manual(values  = okabe_ito(n))),
    set1    = list(scale_color_brewer(palette = "Set1"), scale_fill_brewer(palette = "Set1")),
    set2    = list(scale_color_brewer(palette = "Set2"), scale_fill_brewer(palette = "Set2")),
    greys   = list(scale_color_grey(start = 0.2, end = 0.75),
                   scale_fill_grey(start = 0.2, end = 0.75)),
    list(scale_color_brewer(palette = "Set1"), scale_fill_brewer(palette = "Set1")))
}

# Single fill scale for pie slices.
pie_fill_scale <- function(palette, n, name) {
  if (palette %in% c("auto", "") || is.null(palette))
    return(if (n <= BREWER_MAX) scale_fill_brewer(palette = "Set2", name = name)
           else                 scale_fill_viridis_d(name = name))
  if (palette %in% c("set1", "set2") && n > BREWER_MAX)
    return(scale_fill_viridis_d(name = name))
  switch(palette,
    uf      = scale_fill_manual(values = uf_discrete(n), name = name),
    viridis = scale_fill_viridis_d(name = name),
    cb      = scale_fill_manual(values = okabe_ito(n), name = name),
    set1    = scale_fill_brewer(palette = "Set1", name = name),
    set2    = scale_fill_brewer(palette = "Set2", name = name),
    greys   = scale_fill_grey(start = 0.2, end = 0.75, name = name),
    scale_fill_brewer(palette = "Set2", name = name))
}

# Code-snippet form of group_scales() for a single aesthetic ("color"/"fill").
palette_code <- function(palette, aes_fn, is_cont, n) {
  s <- function(suffix, args = "") sprintf("scale_%s_%s(%s)", aes_fn, suffix, args)
  vals <- function(cols) sprintf('values = c(%s)', paste(sprintf('"%s"', cols), collapse = ", "))
  if (palette == "auto" || is.null(palette)) {
    if (is_cont)        return(s("viridis_c"))
    if (n > BREWER_MAX) return(s("viridis_d"))
    return(s("brewer", 'palette = "Set1"'))
  }
  if (is_cont) {
    if (palette == "uf") return(s("gradient", sprintf('low = "%s", high = "%s"', UF_BLUE, UF_ORANGE)))
    return(s("viridis_c"))
  }
  if (palette %in% c("set1", "set2") && n > BREWER_MAX) return(s("viridis_d"))
  switch(palette,
    uf      = s("manual", vals(uf_discrete(n))),
    viridis = s("viridis_d"),
    cb      = s("manual", vals(okabe_ito(n))),
    set1    = s("brewer", 'palette = "Set1"'),
    set2    = s("brewer", 'palette = "Set2"'),
    greys   = s("grey", "start = 0.2, end = 0.75"),
    s("brewer", 'palette = "Set1"'))
}

# Builds the "y = a + b-x, R2 = ..." annotation for a fitted scatter/line.
trend_label_text <- function(df, xv, yv, meth, deg) {
  d <- data.frame(x = df[[xv]], y = df[[yv]])
  d <- d[stats::complete.cases(d), , drop = FALSE]
  if (!is.numeric(d$x) || !is.numeric(d$y) || nrow(d) < 3) return(NULL)
  if (meth == "loess") {
    fit <- tryCatch(stats::loess(y ~ x, data = d), error = function(e) NULL)
    if (is.null(fit)) return(NULL)
    r2 <- 1 - sum(stats::residuals(fit)^2) / sum((d$y - mean(d$y))^2)
    return(sprintf("loess fit,  R\u00b2 = %.3f", r2))
  }
  fit <- if (meth == "poly")
           tryCatch(stats::lm(y ~ poly(x, deg, raw = TRUE), data = d), error = function(e) NULL)
         else
           tryCatch(stats::lm(y ~ x, data = d), error = function(e) NULL)
  if (is.null(fit)) return(NULL)
  r2 <- summary(fit)$r.squared
  if (meth == "poly") return(sprintf("polynomial (degree %d),  R\u00b2 = %.3f", deg, r2))
  co <- stats::coef(fit)
  sprintf("y = %.3g %+.3g\u00b7x,  R\u00b2 = %.3f", co[1], co[2], r2)
}

# Correlation heatmap over the user-chosen numeric columns. The selection is
# authoritative -- with fewer than two columns chosen we render nothing (rather
# than defaulting to every numeric column), so a wide dataset starts blank and
# fills in as the user picks, instead of dumping an unreadable wall of tiles.
build_corr_heatmap <- function(df, p) {
  allnum <- names(df)[vapply(df, is.numeric, logical(1))]
  num    <- intersect(p$corr_vars %||% character(0), allnum)
  if (length(num) < 2) return(NULL)
  method <- p$corr_method %||% "pearson"
  cm <- suppressWarnings(stats::cor(df[num], use = "pairwise.complete.obs", method = method))
  dd <- as.data.frame(as.table(cm), stringsAsFactors = TRUE)
  names(dd) <- c("Var1", "Var2", "value")
  dd$Var2 <- factor(dd$Var2, levels = rev(levels(dd$Var2)))
  dd$lab  <- ifelse(is.na(dd$value), "", sprintf("%.2f", dd$value))
  title <- if (!is.null(p$title) && nzchar(trimws(p$title))) p$title
           else sprintf("Correlation Heatmap (%s)", tools::toTitleCase(method))
  g <- ggplot(dd, aes(.data[["Var1"]], .data[["Var2"]], fill = .data[["value"]])) +
    geom_tile(color = "white", linewidth = 0.4) +
    scale_fill_gradient2(low = UF_BLUE, mid = "white", high = UF_ORANGE,
                         midpoint = 0, limits = c(-1, 1), name = "r") +
    coord_fixed() +
    labs(title = title, x = NULL, y = NULL) +
    theme_minimal(base_size = 13) +
    theme(plot.title  = element_text(hjust = 0.5, face = "bold", size = 14),
          axis.text.x = element_text(angle = 45, hjust = 1),
          panel.grid  = element_blank(),
          legend.position = p$legend_pos %||% "right")
  if (isTRUE(p$corr_label) && length(num) <= 15)
    g <- g + geom_text(aes(label = .data[["lab"]]), size = 3, color = "gray20")
  g
}

# Means-with-CI plot for the one-way (Compare Groups) tab. Points at each group
# mean, error bars over the confidence interval, and -- when a connecting-letters
# frame is supplied -- the Tukey letter groupings above each bar. A pure function
# of the precomputed means/cld frames (needs ggplot2 attached, like the rest of
# this file), so helpers_compare.R stays ggplot-free.
build_means_letters_plot <- function(means, cld = NULL, ylab = NULL) {
  if (is.null(means) || !nrow(means)) return(NULL)
  m <- means
  m$Group <- factor(as.character(m$Group),
                    levels = as.character(m$Group)[order(m$Mean)])
  has_ci <- all(c("CI_low", "CI_high") %in% names(m)) &&
            any(is.finite(m$CI_low) & is.finite(m$CI_high))
  m$.ytext <- if (has_ci) m$CI_high else m$Mean
  show_letters <- !is.null(cld) && "Letters" %in% names(cld) && nrow(cld)
  if (show_letters)
    m$.lab <- cld$Letters[match(as.character(m$Group), as.character(cld$Group))]
  p <- ggplot(m, aes(x = .data[["Group"]], y = .data[["Mean"]])) +
    geom_point(size = 3, colour = UF_BLUE)
  if (has_ci)
    p <- p + geom_errorbar(aes(ymin = .data[["CI_low"]], ymax = .data[["CI_high"]]),
                           width = 0.15, colour = UF_BLUE)
  if (show_letters)
    p <- p + geom_text(aes(y = .data[[".ytext"]], label = .data[[".lab"]]),
                       vjust = -0.8, fontface = "bold", na.rm = TRUE)
  p + labs(x = NULL, y = ylab %||% "Mean", title = "Group means with 95% CI") +
    theme_minimal(base_size = 13) +
    theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
          panel.grid.minor = element_blank(),
          axis.text.x = element_text(angle = 45, hjust = 1),
          legend.position = "none")
}

# --- Plot builder -----------------------------------------------------------
# p is a plain list of settings:
#   type, x, y, color, title, xlab, ylab, theme, color_hex, size, bins,
#   bar_agg, cat_limit, reg_overlay, reg_type, reg_deg, reg_ci, reg_col,
#   trend_label, palette, alpha, jitter, logscale, facet, legend_pos,
#   gridlines, flip, corr_vars, corr_method, corr_label
build_full_plot <- function(df, p) {
  if (is.null(df) || is.null(p$type)) return(NULL)
  if (identical(p$type, "heatmap")) return(build_corr_heatmap(df, p))
  if (is.null(p$x) || !nzchar(p$x)) return(NULL)
  if (!p$x %in% names(df)) return(NULL)

  pt <- p$type
  xv <- p$x
  yv <- if (!is.null(p$y) && nzchar(p$y) && p$y != "__count__") p$y else NULL
  cv <- if (!is.null(p$color) && nzchar(p$color) && p$color != "__none__") p$color else NULL

  if (pt %in% c("scatter", "line", "boxplot", "violin", "meanerror", "hexbin") &&
      is.null(yv)) return(NULL)
  if (!is.null(yv) && !yv %in% names(df)) return(NULL)
  if (pt %in% c("histogram", "density") && !is.numeric(df[[xv]])) return(NULL)
  if (pt == "hexbin" && (!is.numeric(df[[xv]]) || !is.numeric(df[[yv]]))) return(NULL)
  # hexbin is Suggests-only and geom_hex() errors only at print time, so bail
  # HERE and every caller (render, export, report) degrades gracefully.
  if (pt == "hexbin" && !requireNamespace("hexbin", quietly = TRUE)) return(NULL)

  if (!is.null(cv) && cv %in% names(df)) {
    if (is.numeric(df[[cv]]) && dplyr::n_distinct(df[[cv]]) <= 10)
      df[[cv]] <- as.factor(df[[cv]])
    # These types group by category; a still-continuous numeric group (>10
    # distinct) can't define boxes/violins/bars, so drop it.
    if (pt %in% c("histogram", "bar", "boxplot", "violin", "meanerror", "density") &&
        is.numeric(df[[cv]]))
      cv <- NULL
    # A DISCRETE group past GROUP_MAX draws one geom (and one legend key) per
    # level -- an ID column would ask for thousands. Checked after the as.factor
    # coercion above, and gated on !is.numeric so a continuous numeric colour
    # (one gradient scale, fast at any size) is never caught. chart_hint() says
    # so on screen, and generate_code() mirrors the drop.
    if (!is.null(cv) && !is.numeric(df[[cv]]) &&
        dplyr::n_distinct(df[[cv]]) > GROUP_MAX)
      cv <- NULL
  } else {
    cv <- NULL
  }
  # Hexbin colours by count, not by a grouping variable.
  if (pt == "hexbin") cv <- NULL

  # "Size by" turns a scatter into a bubble chart: point size maps to a numeric
  # variable. Only honoured for scatter, and only for a real numeric column.
  sizev <- if (identical(pt, "scatter") && !is.null(p$size_by) &&
               nzchar(p$size_by) && p$size_by != "__none__" &&
               p$size_by %in% names(df) && is.numeric(df[[p$size_by]]))
             p$size_by else NULL

  size  <- p[["size"]] %||% 2   # [[ ]] not $ -- $ would partial-match p$size_by
  col   <- p$color_hex %||% UF_BLUE
  bins  <- p$bins %||% 30
  alpha <- p$alpha %||% 0.8
  legend_pos <- p$legend_pos %||% "right"
  facet_v <- if (!is.null(p$facet) && nzchar(p$facet) &&
                 p$facet != "__none__" && p$facet %in% names(df)) p$facet else NULL
  # >30 panels are unreadable, so the facet is dropped -- decided HERE, before
  # the chart branches, because the aggregating types (line, bar-with-Y) must
  # group by the facet column only when it will actually be drawn.
  if (!is.null(facet_v) && dplyr::n_distinct(df[[facet_v]]) > 30) facet_v <- NULL

  title <- if (!is.null(p$title) && nzchar(trimws(p$title))) p$title else NULL
  xlab  <- label_or(p$xlab %||% "", xv)
  ylab  <- if (!is.null(yv)) label_or(p$ylab %||% "", yv) else NULL
  subtitle <- NULL

  # ycol lets the line chart fit the smooth on its aggregated ".value" column
  # while the scatter fits on the raw y.
  smooth_layer <- function(ycol = yv) {
    if (!isTRUE(p$reg_overlay)) return(NULL)
    meth <- p$reg_type %||% "lm"
    fml  <- if (meth == "poly")
              stats::as.formula(paste0("y ~ poly(x, ", p$reg_deg %||% 2, ")"))
            else y ~ x
    # With a color/group variable set, fit one line PER GROUP (coloured to match
    # the points); otherwise a single fit in the chosen regression colour.
    if (!is.null(cv)) {
      geom_smooth(
        mapping   = aes(x = .data[[xv]], y = .data[[ycol]], color = .data[[cv]]),
        method    = if (meth == "poly") "lm" else meth,
        formula   = fml, se = isTRUE(p$reg_ci), linewidth = 1.1
      )
    } else {
      geom_smooth(
        mapping   = aes(x = .data[[xv]], y = .data[[ycol]]),
        method    = if (meth == "poly") "lm" else meth,
        formula   = fml, se = isTRUE(p$reg_ci),
        color     = p$reg_col %||% UF_ORANGE, linewidth = 1.1
      )
    }
  }

  p_obj <- NULL

  if (pt == "scatter") {
    aes_m <- aes(x = .data[[xv]], y = .data[[yv]])
    if (!is.null(cv))    aes_m$colour <- aes(colour = .data[[cv]])$colour
    if (!is.null(sizev)) aes_m$size   <- aes(size = .data[[sizev]])$size
    pt_geom <- if (isTRUE(p$jitter)) geom_jitter else geom_point
    # Fix colour only without a colour var, and fix size only without a size var
    # (otherwise those come from the mapped aesthetics).
    geom_args <- list(alpha = alpha)
    if (is.null(cv))    geom_args$color <- col
    if (is.null(sizev)) geom_args$size  <- size
    p_obj <- ggplot(df, aes_m) + do.call(pt_geom, geom_args)
    if (!is.null(sizev))
      p_obj <- p_obj + scale_size_continuous(range = c(1.5, 8), name = sizev)
    p_obj <- p_obj + smooth_layer()

  } else if (pt == "line") {
    # A line connects SUMMARY STATS: aggregate Y by X (within each group) with
    # the chosen function, then connect. For a continuous X with one row each
    # this is the identity (a plain line); for repeated/categorical X it
    # connects the per-category mean/median/sum instead of scribbling raw rows.
    grp  <- unique(c(xv, cv, facet_v))   # facet_v too, or facet_wrap() can't find it
    aggf <- agg_fun(p$line_agg %||% "mean")
    pdat <- df |>
      dplyr::group_by(dplyr::across(dplyr::all_of(grp))) |>
      dplyr::summarise(.value = aggf(.data[[yv]]), .groups = "drop")
    pdat <- pdat[order(pdat[[xv]]), , drop = FALSE]
    aes_m <- if (!is.null(cv))
               aes(x = .data[[xv]], y = .data[[".value"]],
                   color = .data[[cv]], group = .data[[cv]])
             else
               aes(x = .data[[xv]], y = .data[[".value"]], group = 1)
    p_obj <- ggplot(pdat, aes_m)
    p_obj <- if (is.null(cv))
               p_obj + geom_line(linewidth = size * 0.4, color = col) +
                       geom_point(size = size * 0.7,     color = col)
             else
               p_obj + geom_line(linewidth = size * 0.4) +
                       geom_point(size = size * 0.7)
    p_obj <- p_obj + smooth_layer(".value")
    ylab <- label_or(p$ylab %||% "",
                     paste0(tools::toTitleCase(p$line_agg %||% "mean"), " of ", yv))

  } else if (pt == "bar") {
    has_y <- !is.null(yv)
    bw    <- bar_width(size)
    barmax <- p$cat_limit %||% BAR_MAX
    if (is_discrete_col(df[[xv]]) && dplyr::n_distinct(df[[xv]]) > barmax) {
      df <- lump_bar_x(df, xv, if (has_y) df[[yv]] else NULL, barmax)
      subtitle <- sprintf("Showing the %d largest categories; the rest are grouped as \u201cOther\u201d.", barmax)
    }
    if (has_y) {
      grp <- unique(c(xv, cv, facet_v))  # facet_v too, or facet_wrap() can't find it
      pdat <- df |>
        dplyr::group_by(dplyr::across(dplyr::all_of(grp))) |>
        dplyr::summarise(.value = agg_fun(p$bar_agg)(.data[[yv]]), .groups = "drop")
      aes_m <- if (!is.null(cv))
                 aes(x = .data[[xv]], y = .data[[".value"]], fill = .data[[cv]])
               else
                 aes(x = .data[[xv]], y = .data[[".value"]])
      p_obj <- ggplot(pdat, aes_m)
      p_obj <- if (is.null(cv))
                 p_obj + geom_col(fill = col, width = bw, alpha = alpha)
               else
                 p_obj + geom_col(width = bw, alpha = alpha,
                                  position = position_dodge(width = 0.9))
      # Optional connecting line over the bar tops (supervisor: "connect summary
      # stats"). Matches the bars' dodge so grouped lines sit on their bars.
      if (isTRUE(p$bar_line)) {
        if (is.null(cv)) {
          p_obj <- p_obj +
            geom_line(aes(group = 1), linewidth = 0.9, color = "#333333") +
            geom_point(size = 1.8, color = "#333333")
        } else {
          dpos <- position_dodge(width = 0.9)
          p_obj <- p_obj +
            geom_line(aes(group = .data[[cv]]), position = dpos,
                      linewidth = 0.9, color = "#333333") +
            geom_point(aes(group = .data[[cv]]), position = dpos,
                       size = 1.8, color = "#333333")
        }
      }
      ylab <- label_or(p$ylab %||% "",
                       paste0(tools::toTitleCase(p$bar_agg %||% "sum"), " of ", yv))
    } else {
      aes_m <- if (!is.null(cv)) aes(x = .data[[xv]], fill = .data[[cv]])
               else               aes(x = .data[[xv]])
      p_obj <- ggplot(df, aes_m)
      p_obj <- if (is.null(cv))
                 p_obj + geom_bar(stat = "count", fill = col, width = bw, alpha = alpha)
               else
                 p_obj + geom_bar(stat = "count", width = bw, alpha = alpha, position = "dodge")
      ylab <- "Count"
    }

  } else if (pt == "histogram") {
    if (!is.null(cv)) {
      p_obj <- ggplot(df, aes(x = .data[[xv]], fill = .data[[cv]])) +
               geom_histogram(bins = bins, color = "white", alpha = alpha, position = "dodge")
    } else {
      p_obj <- ggplot(df, aes(x = .data[[xv]])) +
               geom_histogram(bins = bins, color = "white", fill = col, alpha = alpha)
    }
    ylab <- "Count"

  } else if (pt == "boxplot") {
    # A boxplot's X is categorical: one box per distinct X. A numeric X left as
    # continuous draws a single mis-sized box (the "cut-off" look), so coerce it
    # to a factor -- each value gets its own box.
    if (is.numeric(df[[xv]])) df[[xv]] <- as.factor(df[[xv]])
    aes_m <- if (!is.null(cv)) aes(x = .data[[xv]], y = .data[[yv]], fill = .data[[cv]])
             else               aes(x = .data[[xv]], y = .data[[yv]])
    p_obj <- ggplot(df, aes_m)
    p_obj <- if (is.null(cv))
               p_obj + geom_boxplot(fill = col, alpha = alpha,
                                    outlier.size = size * 0.7, outlier.alpha = 0.6)
             else
               # dodge2 so a grouped boxplot draws side-by-side boxes per x
               # category; preserve="single" keeps equal widths when a group is
               # missing at some x.
               p_obj + geom_boxplot(alpha = alpha, outlier.size = size * 0.7,
                                    outlier.alpha = 0.6,
                                    position = position_dodge2(preserve = "single"))

  } else if (pt == "violin") {
    # Like a boxplot, but the width shows where the data bunches up. Categorical
    # X (coerce a numeric X so each value gets its own violin); quartile lines
    # inside echo the box.
    if (is.numeric(df[[xv]])) df[[xv]] <- as.factor(df[[xv]])
    aes_m <- if (!is.null(cv)) aes(x = .data[[xv]], y = .data[[yv]], fill = .data[[cv]])
             else               aes(x = .data[[xv]], y = .data[[yv]])
    p_obj <- ggplot(df, aes_m)
    p_obj <- if (is.null(cv))
               p_obj + geom_violin(fill = col, alpha = alpha, trim = FALSE)
             else
               p_obj + geom_violin(alpha = alpha, trim = FALSE,
                                   position = position_dodge(width = 0.9))

  } else if (pt == "density") {
    # Smoothed distribution of a numeric X; overlay one curve per group.
    if (!is.null(cv))
      p_obj <- ggplot(df, aes(x = .data[[xv]], fill = .data[[cv]], color = .data[[cv]])) +
               geom_density(alpha = alpha * 0.6)
    else
      p_obj <- ggplot(df, aes(x = .data[[xv]])) +
               geom_density(fill = col, color = col, alpha = alpha)
    ylab <- "Density"

  } else if (pt == "meanerror") {
    # Group means with error bars (SE or SD), points, and a connecting line.
    # stat_summary computes the summaries straight from the raw Y -- no manual
    # aggregation needed.
    if (is.numeric(df[[xv]])) df[[xv]] <- as.factor(df[[xv]])
    # SE via ggplot2's mean_se; SD computed in base R (mean_sdl would pull in Hmisc).
    fdata <- if (identical(p$err_type %||% "se", "sd"))
               function(z) { z <- z[!is.na(z)]; m <- mean(z); s <- stats::sd(z)
                             data.frame(y = m, ymin = m - s, ymax = m + s) }
             else ggplot2::mean_se
    aes_m <- if (!is.null(cv))
               aes(x = .data[[xv]], y = .data[[yv]], color = .data[[cv]], group = .data[[cv]])
             else aes(x = .data[[xv]], y = .data[[yv]], group = 1)
    p_obj <- ggplot(df, aes_m)
    if (is.null(cv)) {
      p_obj <- p_obj +
        stat_summary(fun = mean, geom = "line", color = col, linewidth = size * 0.4) +
        stat_summary(fun.data = fdata, geom = "errorbar", width = 0.2, color = col) +
        stat_summary(fun = mean, geom = "point", size = size * 1.6, color = col)
    } else {
      dpos <- position_dodge(width = 0.4)
      p_obj <- p_obj +
        stat_summary(fun = mean, geom = "line", position = dpos, linewidth = size * 0.4) +
        stat_summary(fun.data = fdata, geom = "errorbar", width = 0.2, position = dpos) +
        stat_summary(fun = mean, geom = "point", size = size * 1.6, position = dpos)
    }
    ylab <- label_or(p$ylab %||% "", paste0("Mean of ", yv,
                     sprintf(" (\u00b1%s)", toupper(p$err_type %||% "se"))))

  } else if (pt == "hexbin") {
    # 2D density for a dense scatter: bin (x, y) into hexagons coloured by count.
    p_obj <- ggplot(df, aes(x = .data[[xv]], y = .data[[yv]])) +
      geom_hex(bins = bins) +
      scale_fill_gradient(low = "#dce6f5", high = UF_BLUE, name = "Count")

  } else if (pt == "pie") {
    use_count <- is.null(yv)
    pie_df <- if (use_count) {
      dplyr::count(df, .data[[xv]], name = "val_")
    } else {
      df |> dplyr::group_by(.data[[xv]]) |>
            dplyr::summarise(val_ = sum(.data[[yv]], na.rm = TRUE), .groups = "drop")
    }
    names(pie_df)[1] <- "cat_"
    pie_df$cat_ <- as.character(pie_df$cat_)

    pielim <- p$cat_limit %||% PIE_MAX
    lumped <- nrow(pie_df) > pielim
    if (lumped) {
      pie_df <- pie_df[order(pie_df$val_, decreasing = TRUE), ]
      keep   <- pie_df[seq_len(pielim - 1), ]
      other  <- data.frame(cat_ = "Other", val_ = sum(pie_df$val_[-seq_len(pielim - 1)]))
      pie_df <- rbind(keep, other)
    }
    pie_df$cat_ <- factor(pie_df$cat_, levels = pie_df$cat_)
    pct <- pie_df$val_ / sum(pie_df$val_) * 100
    pie_df$label_ <- ifelse(pct >= 5, paste0(pie_df$cat_, "\n", round(pct, 1), "%"), "")

    n_slices   <- nrow(pie_df)
    fill_scale <- pie_fill_scale(p$palette %||% "auto", n_slices, xv)
    subtitle <- if (lumped)
      paste0("Showing the ", pielim - 1, " largest categories; the rest are grouped as \u201cOther\u201d.")
    else NULL

    return(
      ggplot(pie_df, aes(x = "", y = .data[["val_"]], fill = .data[["cat_"]])) +
        geom_col(width = 1, color = "white", linewidth = 0.5) +
        coord_polar("y", start = 0) +
        geom_text(aes(label = .data[["label_"]]), position = position_stack(vjust = 0.5),
                  size = 3.5, color = "white", fontface = "bold") +
        fill_scale +
        labs(title = title, subtitle = subtitle) +
        theme_void(base_size = 13) +
        theme(plot.title    = element_text(hjust = 0.5, face = "bold", size = 14),
              plot.subtitle = element_text(hjust = 0.5, size = 10, color = "#666"),
              legend.position = legend_pos, legend.title = element_text(size = 11))
    )
  }

  if (is.null(p_obj)) return(NULL)

  if (!is.null(cv))
    for (s in group_scales(df, cv, p$palette %||% "auto")) p_obj <- p_obj + s

  if (isTRUE(p$reg_overlay) && isTRUE(p$trend_label) &&
      pt %in% c("scatter", "line") && !is.null(yv)) {
    lab <- trend_label_text(df, xv, yv, p$reg_type %||% "lm", p$reg_deg %||% 2)
    if (!is.null(lab))
      p_obj <- p_obj + annotate("text", x = -Inf, y = Inf, label = lab,
                                hjust = -0.05, vjust = 1.5, size = 4,
                                color = "#333333", fontface = "italic")
  }

  # Axis transform: "none", or <log|sqrt><x|y|both>. Applied only to continuous
  # axes (x only where x is numeric).
  ls <- p$logscale %||% "none"
  if (ls != "none") {
    is_sqrt <- startsWith(ls, "sqrt")
    axis    <- sub("^(log|sqrt)", "", ls)
    if (axis %in% c("x", "both") && is.numeric(df[[xv]]) &&
        pt %in% c("scatter", "line", "histogram", "density", "hexbin"))
      p_obj <- p_obj + (if (is_sqrt) scale_x_sqrt() else scale_x_log10())
    if (axis %in% c("y", "both") &&
        pt %in% c("scatter", "line", "bar", "histogram", "boxplot",
                  "violin", "meanerror", "hexbin"))
      p_obj <- p_obj + (if (is_sqrt) scale_y_sqrt() else scale_y_log10())
  }

  faceted <- !is.null(facet_v)   # the >30-panel cap already applied up top
  if (faceted)
    p_obj <- p_obj + facet_wrap(vars(.data[[facet_v]]))

  if (isTRUE(p$flip)) p_obj <- p_obj + coord_flip()

  uses_color <- pt %in% c("scatter", "line", "meanerror")
  base_theme <- theme_call(p$theme, 13) +
    theme(
      plot.title      = element_text(hjust = 0.5, face = "bold", size = 14,
                                     margin = margin(b = 10)),
      plot.subtitle   = element_text(hjust = 0.5, size = 10, color = "#666"),
      axis.title      = element_text(size = 12),
      legend.title    = element_text(size = 11),
      legend.position = legend_pos
    )
  if (!isTRUE(p$gridlines %||% TRUE))
    base_theme <- base_theme + theme(panel.grid = element_blank())
  if (needs_x_rotation(df, pt, xv) && !isTRUE(p$flip))
    base_theme <- base_theme + theme(axis.text.x = element_text(angle = 40, hjust = 1))
  # Small multiples get breathing room: space between panels, compact bold strip
  # labels on a light band, so faceted charts stay legible when packed together.
  if (faceted)
    base_theme <- base_theme + theme(
      panel.spacing    = grid::unit(0.9, "lines"),
      strip.text       = element_text(size = 9, face = "bold",
                                      margin = margin(3, 3, 3, 3)),
      strip.background = element_rect(fill = "#eef1f5", color = NA))

  # Hexbin's fill is the bin count, not a grouping variable.
  fill_lab <- if (pt == "hexbin") "Count" else if (!uses_color) cv else NULL
  p_obj + base_theme + labs(
    title = title, subtitle = subtitle, x = xlab, y = ylab,
    color = if (uses_color) cv else NULL,
    fill  = fill_lab
  )
}

# --- Code generator ---------------------------------------------------------

bq <- function(n) {
  if (is.null(n) || !nzchar(n)) return(n)
  if (make.names(n) == n) n else sprintf('`%s`', n)
}
qq <- function(s) sprintf('"%s"', gsub('"', '\\\\"', s))

generate_corr_code <- function(df, p) {
  allnum <- names(df)[vapply(df, is.numeric, logical(1))]
  num    <- intersect(p$corr_vars %||% character(0), allnum)
  if (length(num) < 2)
    return("# Pick at least two numeric variables to build the correlation heatmap.")
  method <- p$corr_method %||% "pearson"
  title  <- if (!is.null(p$title) && nzchar(trimws(p$title))) p$title
            else sprintf("Correlation Heatmap (%s)", tools::toTitleCase(method))
  pre <- c("num <- df[sapply(df, is.numeric)]",
           sprintf("num <- num[, c(%s)]",
                   paste(vapply(num, qq, character(1)), collapse = ", ")))
  pre <- c(pre,
           sprintf('cm  <- cor(num, use = "pairwise.complete.obs", method = "%s")', method),
           "dd  <- as.data.frame(as.table(cm))")
  lab <- if (isTRUE(p$corr_label))
           '\n  geom_text(aes(label = sprintf("%.2f", Freq)), size = 3, color = "gray20") +' else ""
  code <- paste0(
    "ggplot(dd, aes(Var1, Var2, fill = Freq)) +",
    '\n  geom_tile(color = "white") +', lab,
    sprintf('\n  scale_fill_gradient2(low = "%s", mid = "white", high = "%s", midpoint = 0, limits = c(-1, 1), name = "r") +',
            UF_BLUE, UF_ORANGE),
    '\n  coord_fixed() +',
    '\n  theme_minimal() +',
    '\n  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +',
    sprintf("\n  labs(title = %s, x = NULL, y = NULL)", qq(title)))
  assemble_code(pre, code, FALSE)
}

generate_code <- function(df, p) {
  if (is.null(df) || is.null(p$type)) return("# Choose a chart type to generate R code.")
  if (identical(p$type, "heatmap")) return(generate_corr_code(df, p))
  if (is.null(p$x) || !nzchar(p$x))
    return("# Choose a chart type and an X variable to generate R code.")

  pt <- p$type
  xv <- p$x
  yv <- if (!is.null(p$y) && nzchar(p$y) && p$y != "__count__") p$y else NULL
  cv <- if (!is.null(p$color) && nzchar(p$color) && p$color != "__none__") p$color else NULL
  facet_v <- if (!is.null(p$facet) && nzchar(p$facet) &&
                 p$facet != "__none__" && p$facet %in% names(df)) p$facet else NULL
  # Same >30-panel cap as build_full_plot, so the emitted code reproduces the
  # chart the user actually saw (no facet on screen -> no facet in the code).
  if (!is.null(facet_v) && dplyr::n_distinct(df[[facet_v]]) > 30) facet_v <- NULL
  sizev <- if (identical(pt, "scatter") && !is.null(p$size_by) &&
               nzchar(p$size_by) && p$size_by != "__none__" &&
               p$size_by %in% names(df) && is.numeric(df[[p$size_by]]))
             p$size_by else NULL

  if (pt %in% c("scatter", "line", "boxplot", "violin", "meanerror", "hexbin") &&
      is.null(yv))
    return("# Select a Y variable to generate code for this chart type.")

  pre <- character(0)
  if (!is.null(cv) && cv %in% names(df)) {
    if (is.numeric(df[[cv]]) && dplyr::n_distinct(df[[cv]]) <= 10)
      pre <- c(pre, sprintf('df[["%s"]] <- as.factor(df[["%s"]])', cv, cv))
    if (pt %in% c("histogram", "bar", "boxplot", "violin", "meanerror", "density") &&
        is.numeric(df[[cv]]) && dplyr::n_distinct(df[[cv]]) > 10) cv <- NULL
    # Same GROUP_MAX cap as build_full_plot, so the emitted code reproduces the
    # chart the user actually saw (no colour on screen -> no colour in the code,
    # and no 11,000-element values = c(...) literal in the snippet).
    if (!is.null(cv) && !is.numeric(df[[cv]]) &&
        dplyr::n_distinct(df[[cv]]) > GROUP_MAX) cv <- NULL
  } else {
    cv <- NULL
  }
  if (pt == "hexbin") cv <- NULL

  # These types use a categorical X -- coerce a numeric X so each value is a group.
  if (pt %in% c("boxplot", "violin", "meanerror") && is.numeric(df[[xv]]))
    pre <- c(pre, sprintf('df[["%s"]] <- as.factor(df[["%s"]])', xv, xv))
  if (pt == "hexbin")
    pre <- c(pre, "# Note: geom_hex() needs the 'hexbin' package installed.")

  barmax <- p$cat_limit %||% BAR_MAX
  if (pt == "bar" && is_discrete_col(df[[xv]]) && dplyr::n_distinct(df[[xv]]) > barmax)
    pre <- c(pre, sprintf("# The app showed only the top %d categories of '%s'; this code plots them all.",
                          barmax, xv),
                  sprintf("# To match it, lump the rest: df[[\"%s\"]] <- forcats::fct_lump_n(df[[\"%s\"]], %d)",
                          xv, xv, barmax))

  size  <- p[["size"]] %||% 2   # [[ ]] not $ -- $ would partial-match p$size_by
  col   <- p$color_hex %||% UF_BLUE
  bins  <- p$bins %||% 30
  agg   <- if (pt == "line") p$line_agg %||% "mean" else p$bar_agg %||% "sum"
  alpha <- round(p$alpha %||% 0.8, 2)
  theme_str <- switch(p$theme %||% "minimal",
    minimal = "theme_minimal()", classic = "theme_classic()",
    light   = "theme_light()",   bw      = "theme_bw()",
    dark    = "theme_dark()",    "theme_minimal()")

  title <- if (!is.null(p$title) && nzchar(trimws(p$title))) p$title else NULL
  xlab  <- label_or(p$xlab %||% "", xv)
  ylab  <- if (!is.null(yv)) label_or(p$ylab %||% "", yv) else NULL

  uses_color  <- pt %in% c("scatter", "line", "meanerror")
  needs_dplyr <- (pt %in% c("bar", "line") && !is.null(yv)) || pt == "pie"

  scale_line <- NULL
  if (!is.null(cv))
    scale_line <- palette_code(
      p$palette %||% "auto",
      if (uses_color) "color" else "fill",
      is.numeric(df[[cv]]) && dplyr::n_distinct(df[[cv]]) > 10,
      dplyr::n_distinct(df[[cv]]))

  smooth_line <- NULL
  if (isTRUE(p$reg_overlay) && pt %in% c("scatter", "line")) {
    meth <- p$reg_type %||% "lm"
    se   <- if (isTRUE(p$reg_ci)) "TRUE" else "FALSE"
    rcol <- p$reg_col %||% UF_ORANGE
    frm  <- if (meth == "poly")
              sprintf('method = "lm", formula = y ~ poly(x, %s)', p$reg_deg %||% 2)
            else sprintf('method = "%s", formula = y ~ x', meth)
    # Grouped fit (one line per group) when a colour var is set; else single fit.
    smooth_line <- if (!is.null(cv))
      sprintf('geom_smooth(aes(color = %s), %s, se = %s, linewidth = 1.1)',
              bq(cv), frm, se)
    else
      sprintf('geom_smooth(%s, se = %s, color = %s, linewidth = 1.1)',
              frm, se, qq(rcol))
  }

  labs_parts <- character(0)
  if (!is.null(title)) labs_parts <- c(labs_parts, sprintf("title = %s", qq(title)))

  data_obj <- "df"
  if (pt == "pie") {
    if (is.null(yv)) {
      pre <- c(pre, sprintf('plot_df <- dplyr::count(df, %s, name = "val")', bq(xv)))
    } else {
      pre <- c(pre, sprintf('plot_df <- dplyr::summarise(dplyr::group_by(df, %s), val = sum(%s, na.rm = TRUE), .groups = "drop")',
                            bq(xv), bq(yv)))
    }
    pre <- c(pre, sprintf('plot_df[["%s"]] <- as.factor(plot_df[["%s"]])', xv, xv))
    pal_pie <- p$palette %||% "auto"
    nslice  <- dplyr::n_distinct(df[[xv]])
    pie_fill <- if (pal_pie %in% c("auto", "")) {
                  if (nslice <= BREWER_MAX) 'scale_fill_brewer(palette = "Set2")'
                  else                       'scale_fill_viridis_d()'
                } else palette_code(pal_pie, "fill", FALSE, nslice)
    lp <- p$legend_pos %||% "right"
    code <- paste0(
      sprintf('ggplot(plot_df, aes(x = "", y = val, fill = %s)) +', bq(xv)),
      '\n  geom_col(width = 1, color = "white") +',
      '\n  coord_polar("y") +',
      '\n  ', pie_fill, ' +',
      '\n  theme_void()',
      if (!identical(lp, "right")) sprintf(' +\n  theme(legend.position = "%s")', lp) else '',
      if (!is.null(title)) paste0(' +\n  labs(title = ', qq(title), ')') else ''
    )
    return(assemble_code(pre, code, needs_dplyr))
  }

  aes_inner <- sprintf("x = %s", bq(xv))
  if (pt %in% c("bar", "line") && !is.null(yv)) aes_inner <- paste0(aes_inner, ", y = .value")
  else if (!is.null(yv))                        aes_inner <- paste0(aes_inner, sprintf(", y = %s", bq(yv)))
  if (!is.null(cv)) aes_inner <- paste0(
    aes_inner, sprintf(", %s = %s", if (uses_color) "color" else "fill", bq(cv)))
  if (!is.null(sizev)) aes_inner <- paste0(aes_inner, sprintf(", size = %s", bq(sizev)))
  if (pt %in% c("line", "meanerror") && is.null(cv)) aes_inner <- paste0(aes_inner, ", group = 1")
  if (pt %in% c("line", "meanerror") && !is.null(cv)) aes_inner <- paste0(aes_inner, sprintf(", group = %s", bq(cv)))

  if (pt %in% c("bar", "line") && !is.null(yv)) {
    # facet_v joins the grouping so the emitted facet_wrap() finds its column.
    grp_cols <- paste(unique(c(bq(xv), if (!is.null(cv)) bq(cv),
                               if (!is.null(facet_v)) bq(facet_v))), collapse = ", ")
    aggfn <- switch(agg, mean = "mean", median = "median", "sum")
    pre <- c(pre, sprintf(
      'plot_df <- dplyr::summarise(dplyr::group_by(df, %s), .value = %s(%s, na.rm = TRUE), .groups = "drop")',
      grp_cols, aggfn, bq(yv)))
    if (pt == "line")
      pre <- c(pre, sprintf('plot_df <- plot_df[order(plot_df[["%s"]]), ]', xv))
    data_obj <- "plot_df"
    ylab <- label_or(p$ylab %||% "", paste0(tools::toTitleCase(agg), " of ", yv))
  }

  pt_fn <- if (isTRUE(p$jitter)) "geom_jitter" else "geom_point"
  geom_lines <- switch(pt,
    scatter = {
      parts <- character(0)
      if (is.null(sizev)) parts <- c(parts, sprintf("size = %s", size))
      parts <- c(parts, sprintf("alpha = %s", alpha))
      if (is.null(cv)) parts <- c(parts, sprintf("color = %s", qq(col)))
      sprintf("%s(%s)", pt_fn, paste(parts, collapse = ", "))
    },
    line    = if (is.null(cv))
                sprintf('geom_line(linewidth = %s, color = %s) +\n  geom_point(size = %s, color = %s)',
                        size * 0.4, qq(col), size * 0.7, qq(col))
              else
                sprintf('geom_line(linewidth = %s) +\n  geom_point(size = %s)',
                        size * 0.4, size * 0.7),
    bar     = if (!is.null(yv)) {
                if (is.null(cv))
                  sprintf('geom_col(fill = %s, width = %s, alpha = %s)', qq(col), round(bar_width(size), 3), alpha)
                else
                  sprintf('geom_col(width = %s, alpha = %s, position = "dodge")', round(bar_width(size), 3), alpha)
              } else {
                if (is.null(cv))
                  sprintf('geom_bar(fill = %s, width = %s, alpha = %s)', qq(col), round(bar_width(size), 3), alpha)
                else
                  sprintf('geom_bar(width = %s, alpha = %s, position = "dodge")', round(bar_width(size), 3), alpha)
              },
    histogram = if (is.null(cv))
                  sprintf('geom_histogram(bins = %s, color = "white", fill = %s, alpha = %s)', bins, qq(col), alpha)
                else
                  sprintf('geom_histogram(bins = %s, color = "white", alpha = %s, position = "dodge")', bins, alpha),
    boxplot = if (is.null(cv))
                sprintf('geom_boxplot(fill = %s, alpha = %s)', qq(col), alpha)
              else
                sprintf('geom_boxplot(alpha = %s)', alpha),
    violin  = if (is.null(cv))
                sprintf('geom_violin(fill = %s, alpha = %s, trim = FALSE)', qq(col), alpha)
              else
                sprintf('geom_violin(alpha = %s, trim = FALSE, position = position_dodge(width = 0.9))', alpha),
    density = if (is.null(cv))
                sprintf('geom_density(fill = %s, color = %s, alpha = %s)', qq(col), qq(col), alpha)
              else
                sprintf('geom_density(alpha = %s)', round((p$alpha %||% 0.8) * 0.6, 2)),
    meanerror = {
      errfun <- if (identical(p$err_type %||% "se", "sd"))
                  'fun.data = function(z) {z <- z[!is.na(z)]; m <- mean(z); s <- sd(z); data.frame(y = m, ymin = m - s, ymax = m + s)}'
                else 'fun.data = mean_se'
      if (is.null(cv))
        sprintf(paste0('stat_summary(fun = mean, geom = "line", color = %s) +\n  ',
                       'stat_summary(%s, geom = "errorbar", width = 0.2, color = %s) +\n  ',
                       'stat_summary(fun = mean, geom = "point", size = %s, color = %s)'),
                qq(col), errfun, qq(col), round(size * 1.6, 2), qq(col))
      else
        sprintf(paste0('stat_summary(fun = mean, geom = "line", position = position_dodge(width = 0.4)) +\n  ',
                       'stat_summary(%s, geom = "errorbar", width = 0.2, position = position_dodge(width = 0.4)) +\n  ',
                       'stat_summary(fun = mean, geom = "point", size = %s, position = position_dodge(width = 0.4))'),
                errfun, round(size * 1.6, 2))
    },
    hexbin = sprintf('geom_hex(bins = %s) +\n  scale_fill_gradient(low = "#dce6f5", high = %s, name = "Count")',
                     bins, qq(UF_BLUE))
  )

  if (pt %in% c("bar", "histogram") && is.null(yv)) ylab <- "Count"
  if (pt == "histogram") ylab <- "Count"
  if (pt == "density")   ylab <- "Density"
  if (pt == "meanerror")
    ylab <- label_or(p$ylab %||% "",
                     paste0("Mean of ", yv, sprintf(" (\u00b1%s)", toupper(p$err_type %||% "se"))))

  labs_parts <- c(labs_parts, sprintf("x = %s", qq(xlab)))
  if (!is.null(ylab)) labs_parts <- c(labs_parts, sprintf("y = %s", qq(ylab)))
  if (!is.null(cv))
    labs_parts <- c(labs_parts, sprintf("%s = %s", if (uses_color) "color" else "fill", qq(cv)))

  lines <- c(sprintf("ggplot(%s, aes(%s))", data_obj, aes_inner))
  lines <- c(lines, geom_lines)
  # Optional connecting line over the bar tops.
  if (pt == "bar" && !is.null(yv) && isTRUE(p$bar_line)) {
    if (is.null(cv))
      lines <- c(lines, 'geom_line(aes(group = 1), linewidth = 0.9, color = "#333333")',
                        'geom_point(size = 1.8, color = "#333333")')
    else
      lines <- c(lines,
        sprintf('geom_line(aes(group = %s), position = position_dodge(width = 0.9), linewidth = 0.9, color = "#333333")', bq(cv)),
        sprintf('geom_point(aes(group = %s), position = position_dodge(width = 0.9), size = 1.8, color = "#333333")', bq(cv)))
  }
  if (!is.null(smooth_line)) lines <- c(lines, smooth_line)
  if (!is.null(scale_line))  lines <- c(lines, scale_line)
  if (!is.null(sizev))
    lines <- c(lines, sprintf('scale_size_continuous(range = c(1.5, 8), name = %s)', qq(sizev)))

  if (isTRUE(p$reg_overlay) && isTRUE(p$trend_label) &&
      pt %in% c("scatter", "line") && !is.null(yv)) {
    tl <- trend_label_text(df, xv, yv, p$reg_type %||% "lm", p$reg_deg %||% 2)
    if (!is.null(tl))
      lines <- c(lines, sprintf(
        'annotate("text", x = -Inf, y = Inf, label = %s, hjust = -0.05, vjust = 1.5, size = 4, color = "#333333", fontface = "italic")',
        qq(tl)))
  }

  ls <- p$logscale %||% "none"
  if (ls != "none") {
    is_sqrt <- startsWith(ls, "sqrt")
    axis    <- sub("^(log|sqrt)", "", ls)
    if (axis %in% c("x", "both") && is.numeric(df[[xv]]) &&
        pt %in% c("scatter", "line", "histogram", "density", "hexbin"))
      lines <- c(lines, if (is_sqrt) "scale_x_sqrt()" else "scale_x_log10()")
    if (axis %in% c("y", "both") &&
        pt %in% c("scatter", "line", "bar", "histogram", "boxplot",
                  "violin", "meanerror", "hexbin"))
      lines <- c(lines, if (is_sqrt) "scale_y_sqrt()" else "scale_y_log10()")
  }

  if (!is.null(facet_v)) lines <- c(lines, sprintf("facet_wrap(vars(%s))", bq(facet_v)))
  if (isTRUE(p$flip))    lines <- c(lines, "coord_flip()")

  lines <- c(lines, theme_str)

  theme_args <- character(0)
  lp <- p$legend_pos %||% "right"
  if (!identical(lp, "right"))            theme_args <- c(theme_args, sprintf('legend.position = "%s"', lp))
  if (!isTRUE(p$gridlines %||% TRUE))     theme_args <- c(theme_args, "panel.grid = element_blank()")
  if (needs_x_rotation(df, pt, xv) && !isTRUE(p$flip))
    theme_args <- c(theme_args, "axis.text.x = element_text(angle = 40, hjust = 1)")
  if (length(theme_args))
    lines <- c(lines, sprintf("theme(%s)", paste(theme_args, collapse = ", ")))

  lines <- c(lines, sprintf("labs(%s)", paste(labs_parts, collapse = ", ")))

  code <- paste0(lines[1], " +\n  ",
                 paste(lines[-1], collapse = " +\n  "))
  assemble_code(pre, code, needs_dplyr)
}

assemble_code <- function(pre, code, needs_dplyr) {
  head_lines <- c(
    "library(ggplot2)",
    if (needs_dplyr) "library(dplyr)",
    "",
    "# Replace `df` with your own data frame, e.g.:",
    '# df <- read.csv("your_data.csv")',
    ""
  )
  if (length(pre)) pre <- c(pre, "")
  paste(c(head_lines, pre, code), collapse = "\n")
}

# --- Multi-plot drawing / export (base grid -- no extra packages) ------------

# Arrange a list of ggplots into a 1- or 2-column grid on the current device.
draw_plot_grid <- function(plots) {
  n <- length(plots)
  if (n == 0) {
    graphics::plot.new()
    graphics::text(0.5, 0.5, "No plots to show.\nConfigure plots in the Visualize tab.",
                   cex = 1.3, col = "gray40")
    return(invisible(NULL))
  }
  ncols <- if (n == 1) 1L else 2L
  nrows <- ceiling(n / ncols)
  grid::grid.newpage()
  grid::pushViewport(grid::viewport(layout = grid::grid.layout(nrows, ncols)))
  for (i in seq_along(plots)) {
    r  <- ceiling(i / ncols)
    cc <- ((i - 1L) %% ncols) + 1L
    print(plots[[i]], vp = grid::viewport(layout.pos.row = r, layout.pos.col = cc))
  }
  invisible(NULL)
}

# Render a list of ggplots to an image file (png / pdf / svg) as a grid.
#
# Device choice is deliberate. NEVER gate this on capabilities("cairo"): that
# reports BUILD-time support and returns TRUE even where cairo.so fails to load
# at runtime (macOS without XQuartz is the common case). cairo_pdf()/svg() then
# emit a warning only, open NO device, and the caller silently receives a path
# that was never written -- while ggplot's print falls through to the default
# device and drops a stray Rplots.pdf in the working directory.
#
# Base pdf() needs no cairo, so PDF works everywhere. SVG has no base-R
# equivalent, so it is probed and reported instead of failing silently.
render_plots_to_file <- function(plots, file, fmt, w_each, h_each, dpi) {
  n     <- max(1L, length(plots))
  ncols <- if (n <= 1) 1L else 2L
  nrows <- ceiling(n / ncols)
  W <- w_each * ncols
  H <- h_each * nrows
  before <- grDevices::dev.cur()
  switch(fmt,
    png = grDevices::png(file, width = W, height = H, units = "in", res = dpi),
    pdf = grDevices::pdf(file, width = W, height = H),
    svg = suppressWarnings(grDevices::svg(file, width = W, height = H)),
    grDevices::png(file, width = W, height = H, units = "in", res = dpi))
  if (identical(grDevices::dev.cur(), before)) {
    stop(if (identical(fmt, "svg"))
           paste0("SVG export needs cairo support, which isn't available on ",
                  "this system. Choose PNG or PDF instead.")
         else
           sprintf("Could not open a %s graphics device on this system.",
                   toupper(fmt)),
         call. = FALSE)
  }
  # Safety net only: cleared by the explicit dev.off() on the success path.
  on.exit(if (!identical(grDevices::dev.cur(), before)) grDevices::dev.off(),
          add = TRUE)
  draw_plot_grid(plots)
  grDevices::dev.off()
  # The device has to be closed before the file is complete on disk, so this
  # is the earliest honest point to confirm the export actually produced bytes.
  if (!file.exists(file) || file.size(file) == 0) {
    stop(sprintf("The %s export produced an empty file.", toupper(fmt)),
         call. = FALSE)
  }
  invisible(file)
}

# --- plotly post-processing (interactive view only) -------------------------
# ggplotly() doesn't carry over a few things ggplot got right; these patch the
# converted plotly object so the interactive preview matches the static export.

# Translate a ggplot legend.position into a plotly layout `legend` list. plotly
# ignores ggplot's top/bottom placement (it always draws the legend on the
# right), so we reposition it explicitly. Returns NULL when the default (right)
# is correct or the legend is hidden ("none" is already honoured by ggplotly).
plotly_legend_layout <- function(pos) {
  switch(pos %||% "right",
    bottom = list(orientation = "h", x = 0.5, xanchor = "center",
                  y = -0.2, yanchor = "top"),
    top    = list(orientation = "h", x = 0.5, xanchor = "center",
                  y = 1.1,  yanchor = "bottom"),
    NULL)
}

# ggplotly() names a dodged/grouped trace "(level,1)" -- the trailing ",1" is the
# panel index, which then leaks into the legend (e.g. a fill of cyl shows
# "(4,1)", "(6,1)", "(8,1)" instead of "4", "6", "8"). Strip it back to just the
# level, leaving already-clean trace names untouched.
clean_plotly_trace_names <- function(ply) {
  if (is.null(ply$x$data)) return(ply)
  ply$x$data <- lapply(ply$x$data, function(tr) {
    nm <- tr$name
    if (!is.null(nm) && length(nm) == 1L && grepl("^\\(.*,\\s*-?\\d+\\)$", nm))
      tr$name <- sub("^\\((.*),\\s*-?\\d+\\)$", "\\1", nm)
    tr
  })
  ply
}
