# Bin

##### Description

The `bin operator` assigns values to bins, based on intervals.

##### Usage

Input projection|.
---|---
`y-axis`        | numeric, measurement, per cell

Input parameters|.
---|---
`method`        | any of Equal Width, Quantiles or Pretty
`n_bins`        | number of bins (for Equal Width and Quantiles methods), or number of _desired_ bins (for Pretty method)

Output relations|.
---|---
`bin_label`| bin label
`bin_id`   | bin ID

##### Details

The computation is based on the cut function from R. Breaks are defined according to the
number of desired bins and computation method specified in the operator settings:

- Equal Width: _n_bins_ intervals of equal size are defined,
- Quantiles: _n_bins_ intervals based on .y distribution quantiles are defined,
- Pretty: approximately _n_bins_ intervals are defined using the `pretty()` R function.

