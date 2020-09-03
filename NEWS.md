
* Name of arguments in make_forest_plot(), make_forest_data(), and make_jasper_forest_plot() have changed. The old names should still work for now and a message is displayed to tell you the new names. Hopefully the new names are more meaningful.
* The code returned by make_forest_plot() will now run entirely on its own with editing. (i.e. It includes code to prepare data for plotting, so you do not need to run make_forest_plot() for it to work.)
* Use of coord_flip() has been removed from make_forest_plot(), and x and y have been switched where needed.
* The x positions for axis labels, text columns, and panel headings in make_forest_plot() are rounded to six decimal places.
* Point estimates and CIs outside the axis limits will not be plotted with make_forest_plot().

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
