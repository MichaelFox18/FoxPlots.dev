# ============================================================
# helpers_model.R — linear / multiple / polynomial regression
# ============================================================
# Pure model helpers lifted from the original Data Explorer: fit a model,
# pull a plain-English interpretation out of its summary, and build the two
# diagnostic ggplots. mod_regression.R is a thin wrapper over these.
#
# fit_model() and model_interpretation() use only base/stats. The gg helpers
# REQUIRE ggplot2 to be attached (the apps do) and use UF_BLUE/UF_ORANGE
# (components.R) and BIG_ROWS (helpers_plot.R).

#' Fit a linear / multiple / polynomial regression.
#'
#' @param df A data frame.
#' @param response Name of the numeric response (Y) column.
#' @param predictors Character vector of predictor (X) columns. Polynomial uses
#'   only the first.
#' @param type One of "linear", "multiple", "polynomial".
#' @param degree Polynomial degree (used when type = "polynomial").
#' @return An lm object.
#' @export
fit_model <- function(df, response, predictors,
                      type = c("linear", "multiple", "polynomial"),
                      degree = 2) {
  type <- match.arg(type)
  stopifnot(is.data.frame(df), is.character(response), length(response) == 1L,
            is.character(predictors), length(predictors) >= 1L)
  missing <- setdiff(c(response, predictors), names(df))
  if (length(missing)) {
    stop("Columns not found in data: ", paste(missing, collapse = ", "))
  }
  rhs <- if (type == "polynomial")
           sprintf("poly(%s, %d, raw = TRUE)", predictors[1], as.integer(degree))
         else
           paste(predictors, collapse = " + ")
  stats::lm(stats::as.formula(paste(response, "~", rhs)), data = df)
}

#' Pull the headline numbers and significance out of a fitted model.
#'
#' @param model An lm object.
#' @return A list: r2, adj_r2, overall_p (model F-test p), and the names of the
#'   significant / non-significant predictors at alpha = 0.05.
#' @export
model_interpretation <- function(model) {
  s     <- summary(model)
  fstat <- s$fstatistic
  overall_p <- if (!is.null(fstat))
    unname(stats::pf(fstat[1], fstat[2], fstat[3], lower.tail = FALSE))
  else NA_real_
  coef_df   <- as.data.frame(s$coefficients)
  pred_rows <- coef_df[rownames(coef_df) != "(Intercept)", , drop = FALSE]
  pvals     <- pred_rows[, 4]
  list(
    r2             = round(s$r.squared, 3),
    adj_r2         = round(s$adj.r.squared, 3),
    overall_p      = overall_p,
    significant    = rownames(pred_rows)[pvals <  0.05],
    nonsignificant = rownames(pred_rows)[pvals >= 0.05]
  )
}

# Diagnostic-plot data is thinned to BIG_ROWS points (deterministically, so the
# view doesn't jump on re-render) to keep ggplotly snappy; the model is still
# fit on every row.
thin_rows <- function(d) {
  if (nrow(d) <= BIG_ROWS) return(d)
  d[unique(round(seq(1, nrow(d), length.out = BIG_ROWS))), , drop = FALSE]
}
thin_note <- function(n) {
  if (n > BIG_ROWS)
    sprintf("Showing ~%s of %s points for responsiveness",
            format(BIG_ROWS, big.mark = ","), format(n, big.mark = ","))
  else NULL
}

#' Fitted-vs-actual diagnostic ggplot. Requires ggplot2 attached.
#' @noRd
reg_fitted_gg <- function(model) {
  d <- data.frame(actual = model$model[[1]], fitted = stats::fitted(model))
  note <- thin_note(nrow(d)); d <- thin_rows(d)
  ggplot(d, aes(x = .data[["actual"]], y = .data[["fitted"]])) +
    geom_point(color = UF_BLUE, size = 2.5, alpha = 0.7) +
    geom_abline(color = UF_ORANGE, linetype = "dashed", linewidth = 1) +
    theme_minimal(base_size = 12) +
    labs(title = "Fitted vs Actual", subtitle = note, x = "Actual", y = "Fitted") +
    theme(plot.title = element_text(hjust = 0.5, face = "bold"),
          plot.subtitle = element_text(hjust = 0.5, size = 9, color = "#777"))
}

#' Residuals-vs-fitted diagnostic ggplot. Requires ggplot2 attached.
#' @noRd
reg_resid_gg <- function(model) {
  d <- data.frame(fitted = stats::fitted(model), resid = stats::residuals(model))
  note <- thin_note(nrow(d)); d <- thin_rows(d)
  ggplot(d, aes(x = .data[["fitted"]], y = .data[["resid"]])) +
    geom_point(color = UF_BLUE, size = 2.5, alpha = 0.7) +
    geom_hline(yintercept = 0, color = UF_ORANGE, linetype = "dashed", linewidth = 1) +
    theme_minimal(base_size = 12) +
    labs(title = "Residuals vs Fitted", subtitle = note,
         x = "Fitted Values", y = "Residuals") +
    theme(plot.title = element_text(hjust = 0.5, face = "bold"),
          plot.subtitle = element_text(hjust = 0.5, size = 9, color = "#777"))
}
