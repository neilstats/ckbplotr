# ckbplotr 0.6.2.9000

* Removed make_jasper_forest_plot function.

# ckbplotr 0.6.2

* Allow use of vectors for col.left and col.right in the addaes and addarg arguments. (So that different aesthetics and arguments can be added for each column.)
* Add digits argument to make_forest_data() and make_forest_plot().
* make_forest_plot() will report the results of automatic horizontal column positioning and spacing.
* Various minor fixes.

# ckbplotr 0.6.1
* Fixes

# ckbplotr 0.6.0

* New spacing and column positioning arguments added to make_forest_plot(). By default, these will be calculated automatically.
* New argument addcode in make_forest_plot() which inserts code into the generated plot code.
* New arguments addaes and addarg in make_shape_plot() and make_forest_plot() which can be used to specify additional aesthetics and arguments for some ggplot layers.
* New argument col.right.parse in make_forest_plot() which controls if columns are parsed into expressions when plotted.
* New argument `row.labels.levels` in make_forest_plot() and make_forest_data() to specify which columns of row.labels data frame to use. And better handling of missing values.
* make_forest_plot() no longer returns data. But the plot data is available at .\$plot\$data
* New argument envir added to make_shape_plot (allows the user to specify the environment for evaluating the plot code).
* ggtext::element_markdown() now used for y-axis labels in make_forest_plot()
* fix_panel_width() replaced by fix_panel()
* Improved handling of unicode characters.
* Updated documentation.
* Fixes and internal code improvements.

# ckbplotr 0.5.0

* In make_forest_plot(), confidence intervals narrower than plotted points can be a different colour (and plotted before/after the points) by using the panel.width argument.
* A new function fix_panel_width() can then be used to fix the width of panels in a forest plot.
* A new argument envir in make_forest_plot() allows the user to specify the environment for evaluating the plotcode. (Helpful if using make_forest_plot() inside another function.)

# ckbplotr 0.4.1

* Fixed make_jasper_forest_plot.

# ckbplotr 0.4.0

* Name of arguments in make_forest_plot(), make_forest_data(), and make_jasper_forest_plot() have changed. The old names should still work for now and a message is displayed to tell you the new names. Hopefully the new names are more meaningful.
* The code returned by make_forest_plot() will now run entirely on its own with editing. (i.e. It includes code to prepare data for plotting, so you do not need to run make_forest_plot() for it to work.)
* Use of coord_flip() has been removed from make_forest_plot(), and x and y have been switched where needed.
* The x positions for axis labels, text columns, and panel headings in make_forest_plot() are rounded to six decimal places.
* Point estimates and CIs outside the axis limits will not be plotted with make_forest_plot().

# ckbplotr 0.3.2

* Arguments col.right.headings and col.left.headings in make_forest_plot will now accept lists, so that different headings can be used between plots.

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
