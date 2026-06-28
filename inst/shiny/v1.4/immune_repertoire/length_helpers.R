## ---- CDR3 length helpers ---------------------------------------------- ##
## scRepertoire::clonalLength has no facet argument: with group.by it overlays
## every group as coloured bars in ONE panel. To show one plot per group we take
## its exportTable (columns: length, CT, values — `values` is the group label),
## aggregate to per-length clonotype counts, and redraw with facet_wrap so each
## group gets its own panel sharing a common length axis.
ir_length_facet_plot <- function(export_tbl, scale = FALSE) {
  stopifnot(all(c("length", "values") %in% colnames(export_tbl)))

  df <- data.frame(
    length = as.integer(export_tbl$length),
    group = as.character(export_tbl$values),
    stringsAsFactors = FALSE
  )
  df <- df[!is.na(df$length) & !is.na(df$group), , drop = FALSE]

  # Per (group, length) clonotype count, then per-group proportion when scaling.
  counts <- as.data.frame(
    table(group = df$group, length = df$length),
    stringsAsFactors = FALSE
  )
  counts$length <- as.integer(counts$length)
  if (isTRUE(scale)) {
    totals <- tapply(counts$Freq, counts$group, sum)
    denom <- totals[counts$group]
    counts$value <- ifelse(denom > 0, counts$Freq / denom, 0)
    y_lab <- "Proportion of CDR3"
  } else {
    counts$value <- counts$Freq
    y_lab <- "Number of CDR3"
  }

  ggplot2::ggplot(
    counts,
    ggplot2::aes(x = .data$length, y = .data$value, fill = .data$group)
  ) +
    ggplot2::geom_col(width = 0.9, show.legend = FALSE) +
    ggplot2::facet_wrap(~ .data$group) +
    ggplot2::labs(x = "Length", y = y_lab) +
    ggplot2::theme_classic()
}
