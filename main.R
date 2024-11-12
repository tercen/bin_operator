suppressPackageStartupMessages({
  library(tercenApi)
  library(tercen)
  library(data.table)
  library(dtplyr)
  library(dplyr, warn.conflicts = FALSE)
})

bin_data <- function(values, method, n_bins) {

  if (method == "Equal Width") {
    breaks <- seq(min(values), max(values), length.out = n_bins + 1)
  } else if (method == "Quantiles") {
    breaks <- quantile(values, probs = seq(0, 1, length.out = n_bins + 1))
  } else if (method == "Pretty") {
    breaks <- pretty(values, n = n_bins)
  }

  binned_col <- cut(values, breaks, include.lowest = TRUE)
  
  return(binned_col)
}

bin_data_2d <- function(df, method, n_bins_x, n_bins_y, group = FALSE) {
  x_values <- df$.x
  y_values <- df$.y
  
  if (method == "Equal Width") {
    breaks_x <- seq(min(x_values), max(x_values), length.out = n_bins_x + 1)
    breaks_y <- seq(min(y_values), max(y_values), length.out = n_bins_y + 1)
  } else if (method == "Quantiles") {
    breaks_x <- quantile(x_values, probs = seq(0, 1, length.out = n_bins_x + 1))
    breaks_y <- quantile(y_values, probs = seq(0, 1, length.out = n_bins_y + 1))
  } else if (method == "Pretty") {
    breaks_x <- pretty(x_values, n = n_bins_x)
    breaks_y <- pretty(y_values, n = n_bins_y)
  }
  
  binned_x <- cut(x_values, breaks_x, include.lowest = TRUE)
  binned_y <- cut(y_values, breaks_y, include.lowest = TRUE)
  
  binned_2d <- interaction(binned_x, binned_y, drop = TRUE)
  
  x_df <- tibble(x_bin = levels(binned_x)) %>% mutate(x_bin_id = seq_len(nrow(.)))
  y_df <- tibble(y_bin = levels(binned_y)) %>% mutate(y_bin_id = seq_len(nrow(.)))
  xy_df <- tibble(xy_bin = levels(binned_2d)) %>% mutate(xy_bin_id = seq_len(nrow(.)))

  if(!group) df$.colorLevels <- 0L
  
  obs_df <- tibble(
    .ci = df$.ci,
    .ri = df$.ri,
    .x_bin_id = as.numeric(binned_x),
    .y_bin_id = as.numeric(binned_y),
    .xy_bin_id = as.numeric(binned_2d),
    .colorLevels = df$.colorLevels
  ) %>%
    ctx$addNamespace()
  
  x_df <- left_join(
    x_df, 
    obs_df %>%
      group_by(.x_bin_id, .colorLevels) %>%
      summarise(x_count = as.numeric(n())) %>%
      mutate(x_prop = x_count / sum(x_count)),
    by = c("x_bin_id" = ".x_bin_id")
  ) %>%
    ctx$addNamespace()
  
  y_df <- left_join(
    y_df, 
    obs_df %>%
      group_by(.y_bin_id, .colorLevels) %>%
      summarise(y_count = as.numeric(n())) %>%
      mutate(y_prop = y_count / sum(y_count)),
    by = c("y_bin_id" = ".y_bin_id")
  ) %>%
    ctx$addNamespace()
  
  xy_df <- left_join(
    xy_df, 
    obs_df %>%
      group_by(.xy_bin_id, .colorLevels) %>%
      summarise(xy_count = as.numeric(n())) %>%
      mutate(xy_prop = xy_count / sum(xy_count)),
    by = c("xy_bin_id" = ".xy_bin_id")
  ) %>%
    ctx$addNamespace()

  if(!group) {
    obs_df <- select(obs_df, -.colorLevels)
    x_df <- select(x_df, -.colorLevels)
    y_df <- select(y_df, -.colorLevels)
    xy_df <- select(xy_df, -.colorLevels)
  } else {
    x_df <- rename(x_df, .x_colorLevels = .colorLevels)
    y_df <- rename(y_df, .y_colorLevels = .colorLevels)
    xy_df <- rename(xy_df, .xy_colorLevels = .colorLevels)
  }
  
  return(list(obs_df = obs_df, x_df = x_df, y_df = y_df, xy_df = xy_df))
}

ctx = tercenCtx()
method <- ctx$op.value("method", as.character, "Equal Width")
n_bins <- ctx$op.value("n_bins", as.double, 10)

if(ctx$hasNumericXAxis) {
  to_select <- c(".y", ".x", ".ci", ".ri")
  
  if(length(ctx$colors) > 0) {
    
    out_list <- ctx$select(c(to_select, ".colorLevels")) %>%
      bin_data_2d(method = method, n_bins_x = n_bins, n_bins_y = n_bins, group = TRUE)
    
    # use color factors instead of colorLevels
    col_relation <- ctx$select(c(ctx$colors, ".colorLevels")) %>% 
      distinct() %>%
      as_relation()
    
    out_rel <- out_list$obs_df %>% 
      as_relation() %>%
      left_join_relation(ctx$crelation, ".ci", ctx$crelation$rids) %>%
      left_join_relation(ctx$rrelation, ".ri", ctx$rrelation$rids) %>%
      left_join_relation(col_relation, ".colorLevels", ".colorLevels") %>%
      left_join_relation(as_relation(out_list$x_df),  list(".colorLevels", ".x_bin_id"), list(".x_colorLevels", paste0(ctx$namespace, ".x_bin_id"))) %>%
      left_join_relation(as_relation(out_list$y_df),  list(".colorLevels", ".y_bin_id"), list(".y_colorLevels", paste0(ctx$namespace, ".y_bin_id"))) %>%
      left_join_relation(as_relation(out_list$xy_df), list(".colorLevels", ".xy_bin_id"), list(".xy_colorLevels", paste0(ctx$namespace, ".xy_bin_id"))) %>%
      as_join_operator(c(ctx$cnames, ctx$rnames, ctx$colors), c(ctx$cnames, ctx$rnames, ctx$colors))

  } else {
    
    out_list <- ctx$select(to_select) %>%
      bin_data_2d(method = method, n_bins_x = n_bins, n_bins_y = n_bins)
    
    out_rel <- out_list$obs_df %>% 
      as_relation() %>%
      left_join_relation(ctx$crelation, ".ci", ctx$crelation$rids) %>%
      left_join_relation(ctx$rrelation, ".ri", ctx$rrelation$rids) %>%
      left_join_relation(as_relation(out_list$x_df), ".x_bin_id", paste0(ctx$namespace, ".x_bin_id")) %>%
      left_join_relation(as_relation(out_list$y_df), ".y_bin_id", paste0(ctx$namespace, ".y_bin_id")) %>%
      left_join_relation(as_relation(out_list$xy_df), ".xy_bin_id", paste0(ctx$namespace, ".xy_bin_id")) %>%
      as_join_operator(c(ctx$cnames, ctx$rnames), c(ctx$cnames, ctx$rnames))
    
  }
  
  save_relation(out_rel, ctx)
  
    
} else {
  to_select <- c(".y", ".ci", ".ri")
  ctx$select(to_select) %>%
    lazy_dt() %>%
    mutate(bin = bin_data(.y, method = method, n_bins = n_bins)) %>%
    mutate(bin_label = as.character(bin), bin_id = as.numeric(bin)) %>%
    select(-bin, -.y) %>%
    as_tibble() %>%
    ctx$addNamespace() %>%
    ctx$save()
  
}




