suppressPackageStartupMessages({
  library(tercenApi)
  library(tercen)
  library(data.table)
  library(dtplyr)
  library(dplyr, warn.conflicts = FALSE)
})

# Define the bin_data function
bin_data <- function(values, method, n_bins) {

  # Bin the data based on the selected method and number of bins
  if (method == "Equal Width") {
    breaks <- seq(min(values), max(values), length.out = n_bins + 1)
  } else if (method == "Quantiles") {
    breaks <- quantile(values, probs = seq(0, 1, length.out = n_bins + 1))
  } else if (method == "Pretty") {
    breaks <- pretty(values, n = n_bins)
  }

  # Use the cut function to bin the data and create a new column
  binned_col <- cut(values, breaks, include.lowest = TRUE)

  # Return the updated data frame
  return(binned_col)
}


ctx = tercenCtx()
method <- ctx$op.value("method", as.character, "Equal Width")
n_bins <- ctx$op.value("n_bins", as.double, 10)

ctx %>% 
  select(.ci, .ri, .y) %>%
  lazy_dt() %>%
  mutate(bin = bin_data(.y, method = method, n_bins = n_bins)) %>%
  mutate(bin_label = as.character(bin), bin_id = as.numeric(bin)) %>%
  select(-bin, -.y) %>%
  as_tibble() %>%
  ctx$addNamespace() %>%
  ctx$save()



