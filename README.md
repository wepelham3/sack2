# sack2

A collection of miscellaneous useful functions used across projects.

---------------

### General functions:

- `vals` to summarize frequency of values in a vector
- `n_unique` to count number of unique values in a vector
- `%notin%` to test whether an element is NOT in a set
- `describe` to tidily summarize a vector
- `describe_df` to tidily summarize a dataframe
- `describe_NA` to tidily summarize missing data
- `count_NA_patterns` to return a count of each missingness pattern in data
- `cor0` to tidily summarize pairwise correlations
- `table0` to call `table()` with `NA`s included
- `cross_table` to call `gmodels::CrossTable()` with all proportions suppressed
- `pval_to_symbol` to convert numeric p values to corresponding significance symbols
- `replace_NAs_with_medians` to replace `NA`s with medians throughout dataframe
- `round0` to round all numeric columns in a dataframe
- `fnr_names` to find and replace patterns in names of a dataframe

### Workflow functions:

- `set_up_new_repo` to create basic file structure to start new analysis
- `clear_output_dir` to delete all files in `output/`
- `bp` to make a noise
- `tc` to write to the clipboard
- `mk` to source the `RUNME.R` or `MAKEFILE.R` in directory
- `notify_cell` to send a text to a cell phone
- `fnr_wd` to find-and-replace in all files in a directory
- `source_lines` to source specific lines in a file
- `source_all` to source all files in a directory
- `lp` to print tibble with all rows shown
- `save_session_info` to save a `.txt` file with session info
- `wpsave` to save `ggplot` as both `.png` and `.pdf` and load in viewer
- `write_wp` to save dataframe as both `.csv` and `.xlsx`

### Functions to tidy (or tidily produce) things:

- `tidy_MIcombine` to tidily pool analysis results when using `mitools`
- `tidy_confusionMatrix` to tidy a confusion matrix from `caret`
- `get_varImp` to get a tidy summary of variable importance from `caret::train`
- `get_train_perf` to get a tidy summary of training performance from `caret::train`

### Functions for conducting multiple imputation in `mice`:

- `inspect_pred.matrix` to check the pattern of which variables predict which in `mice` model
- `fit_mice_for_diag` to iteratively find # iterations in `mice` model necessary to get all `Rhat` < 1.05
- `plot_mice_convergence` to produce convergence plots for `mice` model
- `plot_mice_distrib` to produce raw vs. imputed distribution plots for `mice` model
- `compare_mice_distrib` to compare descriptives on raw vs. imputed distributions for `mice` model

### Functions for working with SPSS `.sav` files:

- `inspect_sav` to get metadata (e.g., variable labels) from a `.sav` file
- `add_sav_labels` to add variable labels based on a `.sav` file or labelled R data
