# odytools 0.8.0.9000

-   Added `ody_rc_add_label`. It labels the variables of a data frame according to the metadata of a REDCap project.
-   The resulting object of `ody_rc_import` now has the class *odytools_redcap*. Speciall methods are dispatched for this class from the following generic funtions:
    -   `base::print`
    -   `dplyr::select`
-   Added `ody_plot_violindotplot` which plots a half violin, a jitter dot plot and boxplot in the same graph. It also performs comparisons between groups.

# odytools 0.8.0

-   Added `ody_extract_vhlab_pdf` to extract data from VH Lab PDFs.
-   Now, when a project is started (both plain and REDCap-based), a template "memento" markdown file is added to docs.
-   Improved `ody_gpt`. Now it is based on `tidychatmodels` package and you can call it using the add-in OdyGPT.
-   Improved `ody_qlq_c30_v3`:
    -   The functions checks if the items have the expected range of values.
    -   New criteria for the calculation of scales when some items are missing.
    -   Added the capability for delta calculation.
-   Added `ody_gt2image` to convert a GT table into an image.
-   When accessing a project (both plain and REDCap-based), the existence and update status of the renv.lock file and a git repo are now checked.
-   When rendering a report via the render chunk, the lockfile is now updated if the report is successfully rendered.
-   When a project is started (both plain and REDCap-based), a template .gitignore file is now added.
-   In the REDCap Data Viewer, the *Event* dropdown list now also shows the Arm (if any) the event belongs to.
-   `ody_rc_import` now includes the attributes:
    -   *meddra_fields*: Vector of the field names coded with MedDRA.
    -   *meddra_codes*: A tibble with the MedDRA codes used by all *meddra_fields*.
-   `ody_rc_select` now adds the attributes *medra_fields* and *meddra_codes* to the output in case any of the selected variables are coded with MedDRA.
-   Added `ody_rc_translate_meddra` to translate MedDRA codes to their descriptions.

# odytools 0.7.5

-   The `ody_rc_import` function now also creates the *form_complete* variable, which indicates the form's status (Incomplete, Unverified, or Complete).
-   Added the *form* argument to `ody_rc_import` to allow for the download of a specific form, as opposed to downloading the entire database.
-   Added `ody_rc_add_site.` Appends patient site to any REDCap-derived table.
-   Minor corrections and improvements:
    -   Added "html-table-processing: none" in the YML header of the analysis templates.
    -   New "render" chunk in the analysis template based on withr.
    -   Now `ody_glue2lang` evaluates as expeted when used inside another function.
    -   Now datasets refresh silently.

# odytools 0.7.4

-   Added a data merger in the REDCap Data Viewer to show in the same table variables belonging to different forms.
-   Added `ody_rc_add_import_date` to add the import date of the redcap_data object to a file name.
-   Now `ody_rc_select` also accepts the variables in a single character vector.
-   Added `ody_qlq_c30_v3` to calculate the scores of the scales of EORTC QLQ-C30 version 3.0.

# odytools 0.7.3

-   Added `ody_glue2lang`, a helping function to glue and transform into evaluable expressions.
-   Added `ody_rc_spread` that takes a classic project (with no events) and spreads it into a tibble with one row per subject. This is useful for creating Excel exports.
-   The new REDCap imports, obtained with the add-in, will no longer be backed up by default. Instead, we've introduced a new add-in that permits the user to save the current redcap_data and datasets objects into a single RData file. This file will be stored in the data/imports directory.

# odytools 0.7.2

-   Improvements to the REDCap Data Viewer:
    -   Data can now be filtered by site.
    -   Instead of displaying the raw name, selectors now present the label name of sites, events, and forms.
    -   To ensure data availability, validators are added (if no data, a human friendly message is displayed).
    -   While internal calculations are being executed, spinners are displayed to provide a visual cue of ongoing processes.
-   `ody_rc_import` improvements:
    -   Nest projects even with no repeating forms.
    -   Get the Data Access Groups information, if any, an creates the following attributes:
        -   *dag*: Tibble with the names and id info of all groups.
        -   *subjects_dag*: Tibble with all the recorded subjects and the group they belong to.
-   Added `ody_compare_1_vs_others` to compare a group with the rest of the groups in each variable of a data frame.
-   Very first version of `ody_gpt`.
-   Introduced a formal method for hardcoding values.
-   Internal improvements of *Update odytools*.

# odytools 0.7.1

-   Now, `ody_rc_import` does not try to nest a project with no repeating istruments even if nest = TRUE.
-   Now, `ody_summarise_df` plots the density curves following the same order as shown in the table and if the test fails when comparing groups, the empty results table is not shown.
-   Minor modification of the *quality* folder structure. Now, it only contains the folders *verification* and *tables.*
-   Added the argument *export* to `ody_add_to_datasets`. If *export* = TRUE, the dataset is exported as csv to quality/tables.
-   Added *Update odytools* to add-ins to install the last master or development version of odytools.

# odytools 0.7.0

-   Now, project-specific functions of a REDCap project are assumed to be stored in the scripts of the folder functions.
-   Added `ody_rc_timetravel` to load previous imports.
-   Now, tokens are stored in .Renviron, so you only have to provide it when starting a new project or after regenerating your token.
-   Added `ody_extract_km` to get the median and confidence intervals of survfit objects.
-   Added `ody_proj_init` to set the basicc structure of a non-REDCap project.
-   Now, `ody_add_version` looks for already rendered outputs in the specified path instead than only in the current wd (the default path).
-   Added *Create Lockfile* addin. It creates and saves a lockfile of the current project, so if any thing crashes after an update, the project can we isolated creating its own library according to the lockfile.

# odytools 0.6.2

-   Added *add_overall* argument to `ody_summarise_df`

# odytools 0.6.1

-   Added `ody_rc_filter_subject`, a function to filter a REDCap import according to a provided vector of subject's Ids.

# odytools 0.6.0

-   Added `ody_add_to_datasets,` a function to create datsets.
-   Added `ody_rc_current`, a function to print the current project and import of the loaded REDCap database.
-   First "REDCap structure" implementation. Added several addins to manage a REDCap database in RStudio.
    -   *Where am I?*: Print the name and import date of the current REDCap project if any. (it just calls ody_rc_current.
    -   *Start/update a REDCap project*: Start, or update if already started, a REDCap project in RStudio.
    -   *Refresh datasets*: Refresh the datasets content.
    -   *REDCap viewer*: Explore the data of a REDCap project. (it calls ody_rc_view)
    -   *View metadata*: Show the metadata of the current project on the Viewer.
    -   *View datasets description*: Show the descriptions of the datasets on the Viewer.
    -   *Add analysis template*: Add a new Quarto template in analysis/.

# odytools 0.5.2

-   Improved `ody_rc_select`. Now it also works with character vectors and by defaulit it returns a list if the selected variables do not belong to the same form.

# odytools 0.5.1

-   Added a descriptive tab in `ody_rc_view`.

# odytools 0.5.0

-   Drastic improvement of `ody_summarise_df`. Now it reports a reactable.

# odytools 0.4.3

-   `ody_pdx_model_sensitivity` improved. Added a model validation section.

# odytools 0.4.2

-   Added a Completeness tab to `ody_rc_view`.

# odytools 0.4.1

-   Improved `ody_rc_view.` It does not need a pre-downloded redcap_data object anymore. Now, if it is not present, the function downloads it by itself (if a token is provided). Download buttons have been added as well as a tab with metadata of the selected variables. This function can now be called from a RStudio addin.

# odytools 0.4.0

-   Added AE related functions.
    -   `ody_make_ae_tbl`: It makes a "max grade" count of patients.
    -   `ody_make_ae_gt`: It inputs the output of ody_make_ae_tbl and makes a gt display table.

# odytools 0.3.2

-   Added other_utils functions:
    -   `ody_add_version`: This function creates a file name with a date version at the end. It takes care no repeated file names exist in the same folder.
    -   `ody_change_names`: It changes the names of a data frame according to the names provided by a second data frame.

# odytools 0.3.1

-   Added `ody_rc_completeness`. This functions verifies the completeness of a REDCap derived dataframe. The conditions list is based on the branching logic of the project.
-   Added the *.include_aux* (TRUE/FALSE) argument to `ody_rc_select`. If TRUE, the function also returns the auxiliar logic variables of a multiple selection variable.

# odytools 0.3.0

-   Added REDCap functions:
    -   `ody_rc_import`: Imports a project with a user token.
    -   `ody_rc_view`: Interactive viewer to explore the imported project.
    -   `ody_rc_select`: Helper function to select forms or variables.
    -   `ody_rc_format`: Helper function to format variables according to their metadata definition.

# odytools 0.2.2

-   Now `ody_pdx_model_sensitivity` also works with a 3 levels treatment factor.

# odytools 0.2.1

-   Added entry controls to `ody_pdx_model_sensitivity`.

# odytools 0.2.0

-   Added data quality functions `ody_verify_completeness` and `ody_verify_conformance`.
-   Added `ody_render_quality_report` to render a html with the result of `ody_verify_completeness` and `ody_verify_conformance`.

# odytools 0.1.0

-   Added `ody_summarise_df`
-   Added `ody_define_timepoints`
-   Added `ody_pdx_model_sensitivity`
