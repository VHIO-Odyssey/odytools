## **data_quality**

Data quality reports.

-   count_grouped_cases

-   ody_verify_completeness

-   ody_verify_conformance [count_grouped_cases]

-   report_completeness

-   report_conformance

-   ody_render_quality_report [ody_verify_completeness, ody_verify_conformance, report_completeness, report_conformance]

## **data_summary**

Functions to summarise dataframes.

-   complete_list

-   make_var_list

-   summary_tibble

-   summarise_continuos_var [summary_tibble]

-   summarise_discrete_var

-   ody_summarise_df [complete_list, make_var_list, summarise_continuos_var, summarise_discrete_var, make_continuous_detail_tbl, make_discrete_detail_tbl]

## **data_wrangling**

Data wrangling functions.

-   get_timepoint

-   ody_define_timepoint [get_timepoint]

## **pdx**

Functions for the Experimental Therapeutics Group.

-   ody_ody_pdx_model_percentage_response (Deprecated)

-   ody_pdx_model_sensitivity

## **redcap**

Functions to export and work on redcap projects.

-   extract_data

-   import_rc [extract_data]

-   label_rc_import

-   nest_rc

-   restore_attributes

-   ody_rc_import [import_rc, label_rc_import, nest_rc, restore_attributes]

-   select_rc_long

-   select_rc_classic

-   ody_rc_select [select_rc_long, select_rc_classic]

-   ody_rc_format

-   ody_rc_view [restore_attributes, ody_rc_completeness, ody_rc_format, ody_summarise_df]

-   ody_rc_completeness [ody_verify_completeness, get_conditions_from_metadata]

## redcap_project

Functions to create and update RedCap projects in RStudio:

-   get_project_name

-   get_import_date

-   rc_init_dirs_files [get_project_name]

-   rc_store_data [get_project_name, ody_rc_import, get_import_date]

-   rc_store_datasets [get_project_name, get_import_date]

-   rc_init_update [rc_init_dirs_files, rc_store_data, rc_store_datasets, get_project_name] ADDIN

-   rc_refresh_datasets [rc_store_datasets, get_project_name] ADDIN

-   ody_add_to_datasets

-   ody_rc_current ADDIN

-   add_analysis_template ADDIN

-   myView

-   rc_view_metadata ADDIN

-   view_datasets ADDIN

## **other_utils**

Miscelaneous utilities.

-   ody_add_version

## **aes**

Adverse Events related functions.

-   count_ae_max_grade

-   make_ae_tbl [count_ae_max_grade]

-   ody_make_ae_tbl [make_ae_table]

-   add_pct

-   ody_make_ae_gt [add_pct]
