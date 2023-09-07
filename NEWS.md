# odytools 0.1.0

-   Added **ody_summarise_df**
-   Added **ody_define_timepoints**
-   Added **ody_pdx_model_sensitivity**

# odytools 0.2.0

-   Added data quality functions **ody_verify_completeness** and **ody_verify_conformance**.
-   Added **ody_render_quality_report** to render a html with the result of **ody_verify_completeness** and **ody_verify_conformance**.

# odytools 0.2.1

-   Added entry controls to **ody_pdx_model_sensitivity**.

# odytools 0.2.2

-   Now **ody_pdx_model_sensitivity** also works with a 3 levels treatment factor.

# odytools 0.3.0

-   Added RedCap functions:
    -   **ody_rc_import**: Imports a project with a user token.
    -   **ody_rc_view**: Interactive viewer to explore the imported project.
    -   **ody_rc_select**: Helper function to select forms or variables.
    -   **ody_rc_format**: Helper function to format variables according to their metadata definition.

# odytools 0.3.1

-   Added **ody_rc_completeness**. This functions verifies the completeness of a RedCap derived dataframe. The conditions list is based on the branching logic of the project.
-   Added the *.include_aux* (TRUE/FALSE) argument to **ody_rc_select**. If TRUE, the function also returns the auxiliar logic variables of a multiple selection variable.

# odytools 0.3.2

-   Added other_utils functions:

    -   **ody_add_version**: This function creates a file name with a date version at the end. It takes care no repeated file names exist in the same folder.

    -   **ody_change_names**: It changes the names of a data frame according to the names provided by a second data frame.

# odytools 0.4.0

-   Added AE related functions.

    -   **ody_make_ae_tbl**: It makes a "max grade" count of patients.
    -   **ody_make_ae_gt**: It inputs the output of ody_make_ae_tbl and makes a gt display table.

# odytools 0.4.1

-   Improved **ody_rc_view.** It does not need a pre-downloded redcap_data object anymore. Now, if it is not present, the function downloads it by itself (if a token is provided). Download buttons have been added as well as a tab with metadata of the selected variables. This function can now be called from a RStudio addin.

# odytools 0.4.2

-   Added a Completeness tab to **ody_rc_view**.

# odytools 0.4.3

-   **ody_pdx_model_sensitivity** improved. Added a model validation section.

# odytools 0.5.0

-   Drastic improvement of **ody_summarise_df**. Now it reports a reactable.
