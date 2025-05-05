map_dim <- function(data, dim_col) {
  mapview(data,
          zcol = dim_col,
          col.regions = c("blue", "white", "red"),  # négatif ↔ positif
          at = c(min(data[[dim_col]], na.rm = TRUE),
                 0,
                 max(data[[dim_col]], na.rm = TRUE)),
          layer.name = dim_col)
}

