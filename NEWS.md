# ckbplotr 0.3.1

* New argument stroke in make_shape_plot and make_forest_plot.
* New argument nullval in make_forest_plot, which adds a vertical reference line at this value. (By default a line is still added at 1 if using log scale.)
* New argument minse in make_shape_plot and make_forest_plot which sets the minimum standard error to use when scaling point size. This allows scaling to be made consistent between plots.
* Argument col.keep available in make_forest_plot() to keep columns in the returned data frame.
* Using the addtext argument of make_forest_plot(), you now need to include an equals or less than sign with the p-value.

# ckbplotr 0.3.0

* New arguments for setting aesthetics overall (by value) or per-point (by specifying a column name).
* Size of text and lines etc. can be controlled by base_size and base_line_size.
* Fitted lines can be added to shape plots with the lines argument.
* Log scale on plots can be controlled with the logscale argument.
* Added vignettes and improved documentation.
* Other small fixes.

# ckbplotr 0.2.0

* Update version number before changes that are not backwards compatible may be introduced.
* Added a `NEWS.md` file to track changes to the package.
