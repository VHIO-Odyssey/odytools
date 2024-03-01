# odytools 0.7.2.9000

-   The new REDCap imports obtained with the add-in are no longer stored by default. Instead, a pop-up window will ask if you wish to retain it. If your answer is yes, both the import and the datasets will be stored in a single, dated RData file in data/imports.

# odytools 0.7.2

-   Improvements to the REDCap Data Viewer:
    -   Data can now be filtered by site.
    -   Instead of displaying the raw name, selectors now present the label name of sites, events, and forms.
    -   To ensure data availability, validators are added (if no data, a human friendly message is displayed).
    -   While internal calculations are being executed, spinners are displayed to provide a visual cue of ongoing processes.
-   **ody_rc_import** improvements:
    -   Nest projects even with no repeating forms.
    -   Get the Data Access Groups information, if any, an creates the following attributes:
        -   **dag**: Tibble with the names and id info of all groups.
        -   **subjects_dag**: Tibble with all the recorded subjects and the group they belong to.
-   Added **ody_compare_1_vs_others** to compare a group with the rest of the groups in each variable of a data frame.
-   Very first version of **ody_gpt**.
-   Introduced a formal method for hardcoding values.
-   Internal improvements of **Update odytools**.

# odytools 0.7.1

-   Now, **ody_rc_import** does not try to nest a project with no repeating istruments even if nest = TRUE.
-   Now, **ody_summarise_df** plots the density curves following the same order as shown in the table and if the test fails when comparing groups, the empty results table is not shown.
-   Minor modification of the *quality* folder structure. Now, it only contains the folders *verification* and *tables.*
-   Added the argument *export* to **ody_add_to_datasets**. If *export* = TRUE, the dataset is exported as csv to quality/tables.
-   Added **Update odytools** to add-ins to install the last master or development version of odytools.

# odytools 0.7.0

-   Now, project-specific functions of a REDCap project are assumed to be stored in the scripts of the folder functions.
-   Added **ody_rc_timetravel** to load previous imports.
-   Now, tokens are stored in .Renviron, so you only have to provide it when starting a new project or after regenerating your token.
-   Added **ody_extract_km** to get the median and confidence intervals of survfit objects.
-   Added **ody_proj_init** to set the basicc structure of a non-REDCap project.
-   Now, **ody_add_version** looks for already rendered outputs in the specified path instead than only in the current wd (the default path).
-   Added **Create Lockfile** addin. It creates and saves a lockfile of the current project, so if any thing crashes after an update, the project can we isolated creating its own library according to the lockfile.

# odytools 0.6.2

-   Added **add_overall** argument to **ody_summarise_df**

# odytools 0.6.1

-   Added **ody_rc_filter_subject**, a function to filter a REDCap import according to a provided vector of subject's Ids.

# odytools 0.6.0

-   Added **ody_add_to_datasets,** a function to create datsets.

-   Added **ody_rc_current**, a function to print the current project and import of the loaded REDCap database.

-   First "REDCap structure" implementation. Added several addins to manage a REDCap database in RStudio.

    -   **Where am I?**: Print the name and import date of the current REDCap project if any. (it just calls ody_rc_current.

    -   **Start/update a REDCap project**: Start, or update if already started, a REDCap project in RStudio.

    -   **Refresh datasets**: Refresh the datasets content.

    -   **REDCap viewer**: Explore the data of a REDCap project. (it calls ody_rc_view)

    -   **View metadata**: Show the metadata of the current project on the Viewer.

    -   **View datasets description**: Show the descriptions of the datasets on the Viewer.

    -   **Add analysis template**: Add a new Quarto template in analysis/.

# odytools 0.5.2

-   Improved **ody_rc_select**. Now it also works with character vectors and by defaulit it returns a list if the selected variables do not belong to the same form.

# odytools 0.5.1

-   Added a descriptive tab in **ody_rc_view**.

# odytools 0.5.0

-   Drastic improvement of **ody_summarise_df**. Now it reports a reactable.

# odytools 0.4.3

-   **ody_pdx_model_sensitivity** improved. Added a model validation section.

# odytools 0.4.2

-   Added a Completeness tab to **ody_rc_view**.

# odytools 0.4.1

-   Improved **ody_rc_view.** It does not need a pre-downloded redcap_data object anymore. Now, if it is not present, the function downloads it by itself (if a token is provided). Download buttons have been added as well as a tab with metadata of the selected variables. This function can now be called from a RStudio addin.

# odytools 0.4.0

-   Added AE related functions.

    -   **ody_make_ae_tbl**: It makes a "max grade" count of patients.
    -   **ody_make_ae_gt**: It inputs the output of ody_make_ae_tbl and makes a gt display table.

# odytools 0.3.2

-   Added other_utils functions:

    -   **ody_add_version**: This function creates a file name with a date version at the end. It takes care no repeated file names exist in the same folder.

    -   **ody_change_names**: It changes the names of a data frame according to the names provided by a second data frame.

# odytools 0.3.1

-   Added **ody_rc_completeness**. This functions verifies the completeness of a REDCap derived dataframe. The conditions list is based on the branching logic of the project.
-   Added the *.include_aux* (TRUE/FALSE) argument to **ody_rc_select**. If TRUE, the function also returns the auxiliar logic variables of a multiple selection variable.

# odytools 0.3.0

-   Added REDCap functions:
    -   **ody_rc_import**: Imports a project with a user token.
    -   **ody_rc_view**: Interactive viewer to explore the imported project.
    -   **ody_rc_select**: Helper function to select forms or variables.
    -   **ody_rc_format**: Helper function to format variables according to their metadata definition.

# odytools 0.2.2

-   Now **ody_pdx_model_sensitivity** also works with a 3 levels treatment factor.

# odytools 0.2.1

-   Added entry controls to **ody_pdx_model_sensitivity**.

# odytools 0.2.0

-   Added data quality functions **ody_verify_completeness** and **ody_verify_conformance**.
-   Added **ody_render_quality_report** to render a html with the result of **ody_verify_completeness** and **ody_verify_conformance**.

# odytools 0.1.0

-   Added **ody_summarise_df**
-   Added **ody_define_timepoints**
-   Added **ody_pdx_model_sensitivity**
