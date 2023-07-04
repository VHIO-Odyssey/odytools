-   **data_quality:** Data quality reports.

    -   count_grouped_cases

    -   ody_verify_completeness

    -   ody_verify_conformance [count_grouped_cases]

    -   report_completeness

    -   report_conformance

    -   ody_render_quality_report [ody_verify_completeness, ody_verify_conformance, report_completeness, report_conformance]

-   **data_summary:** Functions to summarise dataframes.

    -   complete_list

    -   make_var_list

    -   summary_tibble

    -   summarise_continuos_var [summary_tibble]

    -   summarise_discrete_var

    -   ody_summarise_df [complete_list, make_var_list, summarise_continuos_var, summarise_discrete_var]

-   **data_wrangling:** Data wrangling functions.

    -   get_timepoint

    -   ody_define_timepoint [get_timepoint]

-   **pdx:** Functions for the Experimental Therapeutics Group.

    -   ody_ody_pdx_model_percentage_response (Deprecated)

    -   ody_pdx_model_sensitivity

-   **redcap:** Functions to export and work on redcap projects.

    -   extract_data

    -   import_rc [extract_data]

    -   label_rc_import

    -   nest_rc

    -   restore_attributes

    -   ody_rc_import [import_rc, label_rc_import, nest_rc, restore_attributes]

    -   select_rc_long

    -   select_rc_classic

    -   ody_rc_select [select_rc_long, select_rc_classic]

    -   ody_rc_view

    -   ody_rc_completeness [ody_verify_completeness]

-   **other_utils**: Miscelaneous utilities.

    -   ody_add_version
