# ============================================================
# helpers_plot.R — chart building, suitability hints & code gen
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
# interactive plotly ones — ggplotly() gets slow well before a few thousand
# marks and can make the single-threaded app appear frozen.
BIG_ROWS   <- 1000
PIE_MAX    <- 12   # pie slices beyond this are grouped into "Other"
BREWER_MAX <- 8    # ColorBrewer Set1/Set2 run out past ~8 levels -> viridis
BAR_MAX    <- 30   # bars beyond this are grouped into "Other"

# Maps the 0.5–5 size slider onto a sensible 0.2–0.9 bar width.
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
    sel  <- p$corr_vars
    k    <- if (!is.null(sel) && length(sel) >= 2) length(intersect(sel, nums)) else length(nums)
    if (k < 2) return("A correlation heatmap needs at least <b>2 numeric columns</b> selected.")
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
  if (pt == "boxplot" && cont_x)
    return(sprintf("<b>%s</b> looks continuous, so you'll get one box per value. Box plots group a numeric Y by a <b>categorical</b> X.", xv))
  if (pt == "pie" && cont_x)
    return(sprintf("<b>%s</b> looks continuous, which makes an unreadable pie. Pie charts need a <b>categorical</b> variable with a handful of values.", xv))
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

# Builds the "y = a + b·x, R² = ..." annotation for a fitted scatter/line.
trend_label_text <- function(df, xv, yv, meth, deg) {
  d <- data.frame(x = df[[xv]], y = df[[yv]])
  d <- d[stats::complete.cases(d), , drop = FALSE]
  if (!is.numeric(d$x) || !is.numeric(d$y) || nrow(d) < 3) return(NULL)
  if (meth == "loess") {
    fit <- tryCatch(stats::loess(y ~ x, data = d), error = function(e) NULL)
    if (is.null(fit)) return(NULL)
    r2 <- 1 - sum(stats::residuals(fit)^2) / sum((d$y - mean(d$y))^2)
    return(sprintf("loess fit,  R² = %.3f", r2))
  }
  fit <- if (meth == "poly")
           tryCatch(stats::lm(y ~ poly(x, deg, raw = TRUE), data = d), error = function(e) NULL)
         else
           tryCatch(stats::lm(y ~ x, data = d), error = function(e) NULL)
  if (is.null(fit)) return(NULL)
  r2 <- summary(fit)$r.squared
  if (meth == "poly") return(sprintf("polynomial (degree %d),  R² = %.3f", deg, r2))
  co <- stats::coef(fit)
  sprintf("y = %.3g %+.3g·x,  R² = %.3f", co[1], co[2], r2)
}

# Correlation heatmap over the numeric columns (or a chosen subset).
build_corr_heatmap <- function(df, p) {
  num <- names(df)[vapply(df, is.numeric, logical(1))]
  sel <- p$corr_vars
  if (!is.null(sel) && length(sel) >= 2) num <- intersect(sel, num)
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

  if (pt %in% c("scatter", "line", "boxplot") && is.null(yv)) return(NULL)
  if (!is.null(yv) && !yv %in% names(df)) return(NULL)
  if (pt == "histogram" && !is.numeric(df[[xv]])) return(NULL)

  if (!is.null(cv) && cv %in% names(df)) {
    if (is.numeric(df[[cv]]) && dplyr::n_distinct(df[[cv]]) <= 10)
      df[[cv]] <- as.factor(df[[cv]])
    # bar / histogram / boxplot group by category; a still-continuous numeric
    # group (>10 distinct) can't define boxes/bars, so drop it.
    if (pt %in% c("histogram", "bar", "boxplot") && is.numeric(df[[cv]]))
      cv <- NULL
  } else {
    cv <- NULL
  }

  size  <- p$size %||% 2
  col   <- p$color_hex %||% UF_BLUE
  bins  <- p$bins %||% 30
  alpha <- p$alpha %||% 0.8
  legend_pos <- p$legend_pos %||% "right"
  facet_v <- if (!is.null(p$facet) && nzchar(p$facet) &&
                 p$facet != "__none__" && p$facet %in% names(df)) p$facet else NULL

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
    aes_m <- if (!is.null(cv)) aes(x = .data[[xv]], y = .data[[yv]], color = .data[[cv]])
             else               aes(x = .data[[xv]], y = .data[[yv]])
    pt_geom <- if (isTRUE(p$jitter)) geom_jitter else geom_point
    p_obj <- ggplot(df, aes_m)
    p_obj <- if (is.null(cv))
               p_obj + pt_geom(size = size, alpha = alpha, color = col)
             else
               p_obj + pt_geom(size = size, alpha = alpha)
    p_obj <- p_obj + smooth_layer()

  } else if (pt == "line") {
    # A line connects SUMMARY STATS: aggregate Y by X (within each group) with
    # the chosen function, then connect. For a continuous X with one row each
    # this is the identity (a plain line); for repeated/categorical X it
    # connects the per-category mean/median/sum instead of scribbling raw rows.
    grp  <- c(xv, cv)
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
      subtitle <- sprintf("Showing the %d largest categories; the rest are grouped as “Other”.", barmax)
    }
    if (has_y) {
      grp <- c(xv, cv)
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
    # to a factor — each value gets its own box.
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
      paste0("Showing the ", pielim - 1, " largest categories; the rest are grouped as “Other”.")
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
        pt %in% c("scatter", "line", "histogram"))
      p_obj <- p_obj + (if (is_sqrt) scale_x_sqrt() else scale_x_log10())
    if (axis %in% c("y", "both") &&
        pt %in% c("scatter", "line", "bar", "histogram", "boxplot"))
      p_obj <- p_obj + (if (is_sqrt) scale_y_sqrt() else scale_y_log10())
  }

  faceted <- !is.null(facet_v) && dplyr::n_distinct(df[[facet_v]]) <= 30
  if (faceted)
    p_obj <- p_obj + facet_wrap(vars(.data[[facet_v]]))

  if (isTRUE(p$flip)) p_obj <- p_obj + coord_flip()

  uses_color <- pt %in% c("scatter", "line")
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

  p_obj + base_theme + labs(
    title = title, subtitle = subtitle, x = xlab, y = ylab,
    color = if (uses_color) cv else NULL,
    fill  = if (!uses_color) cv else NULL
  )
}

# --- Code generator ---------------------------------------------------------

bq <- function(n) {
  if (is.null(n) || !nzchar(n)) return(n)
  if (make.names(n) == n) n else sprintf('`%s`', n)
}
qq <- function(s) sprintf('"%s"', gsub('"', '\\\\"', s))

generate_corr_code <- function(df, p) {
  num <- names(df)[vapply(df, is.numeric, logical(1))]
  sel <- p$corr_vars
  if (!is.null(sel) && length(sel) >= 2) num <- intersect(sel, num)
  if (length(num) < 2)
    return("# A correlation heatmap needs at least two numeric columns.")
  method <- p$corr_method %||% "pearson"
  title  <- if (!is.null(p$title) && nzchar(trimws(p$title))) p$title
            else sprintf("Correlation Heatmap (%s)", tools::toTitleCase(method))
  pre <- "num <- df[sapply(df, is.numeric)]"
  if (!is.null(sel) && length(sel) >= 2)
    pre <- c(pre, sprintf("num <- num[, c(%s)]",
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

  if (pt %in% c("scatter", "line", "boxplot") && is.null(yv))
    return("# Select a Y variable to generate code for this chart type.")

  pre <- character(0)
  if (!is.null(cv) && cv %in% names(df)) {
    if (is.numeric(df[[cv]]) && dplyr::n_distinct(df[[cv]]) <= 10)
      pre <- c(pre, sprintf('df[["%s"]] <- as.factor(df[["%s"]])', cv, cv))
    if (pt %in% c("histogram", "bar") && is.numeric(df[[cv]]) &&
        dplyr::n_distinct(df[[cv]]) > 10) cv <- NULL
  } else {
    cv <- NULL
  }

  # Boxplot X is categorical — coerce a numeric X so each value gets its own box.
  if (pt == "boxplot" && is.numeric(df[[xv]]))
    pre <- c(pre, sprintf('df[["%s"]] <- as.factor(df[["%s"]])', xv, xv))

  barmax <- p$cat_limit %||% BAR_MAX
  if (pt == "bar" && is_discrete_col(df[[xv]]) && dplyr::n_distinct(df[[xv]]) > barmax)
    pre <- c(pre, sprintf("# The app showed only the top %d categories of '%s'; this code plots them all.",
                          barmax, xv),
                  sprintf("# To match it, lump the rest: df[[\"%s\"]] <- forcats::fct_lump_n(df[[\"%s\"]], %d)",
                          xv, xv, barmax))

  size  <- p$size %||% 2
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

  uses_color  <- pt %in% c("scatter", "line")
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
  if (pt == "line" && is.null(cv)) aes_inner <- paste0(aes_inner, ", group = 1")
  if (pt == "line" && !is.null(cv)) aes_inner <- paste0(aes_inner, sprintf(", group = %s", bq(cv)))

  if (pt %in% c("bar", "line") && !is.null(yv)) {
    grp_cols <- paste(c(bq(xv), if (!is.null(cv)) bq(cv)), collapse = ", ")
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
    scatter = if (is.null(cv))
                sprintf('%s(size = %s, alpha = %s, color = %s)', pt_fn, size, alpha, qq(col))
              else
                sprintf('%s(size = %s, alpha = %s)', pt_fn, size, alpha),
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
                sprintf('geom_boxplot(alpha = %s)', alpha)
  )

  if (pt %in% c("bar", "histogram") && is.null(yv)) ylab <- "Count"
  if (pt == "histogram") ylab <- "Count"

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
        pt %in% c("scatter", "line", "histogram"))
      lines <- c(lines, if (is_sqrt) "scale_x_sqrt()" else "scale_x_log10()")
    if (axis %in% c("y", "both") &&
        pt %in% c("scatter", "line", "bar", "histogram", "boxplot"))
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

# --- Multi-plot drawing / export (base grid — no extra packages) ------------

# Arrange a list of ggplots into a 1- or 2-column grid on the current device.
draw_plot_grid <- function(plots) {
  n <- length(plots)
  if (n == 0) {
    plot.new()
    text(0.5, 0.5, "No plots to show.\nConfigure plots in the Visualize tab.",
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
render_plots_to_file <- function(plots, file, fmt, w_each, h_each, dpi) {
  n     <- max(1L, length(plots))
  ncols <- if (n <= 1) 1L else 2L
  nrows <- ceiling(n / ncols)
  W <- w_each * ncols
  H <- h_each * nrows
  switch(fmt,
    png = grDevices::png(file, width = W, height = H, units = "in", res = dpi),
    pdf = grDevices::cairo_pdf(file, width = W, height = H),
    svg = grDevices::svg(file, width = W, height = H),
    grDevices::png(file, width = W, height = H, units = "in", res = dpi))
  on.exit(grDevices::dev.off())
  draw_plot_grid(plots)
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

# ggplotly() names a dodged/grouped trace "(level,1)" — the trailing ",1" is the
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
