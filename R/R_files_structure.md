## **data_quality**

Data quality reports.

-   count_grouped_cases

-   ody_verify_completeness

-   ody_verify_conformance [count_grouped_cases]

-   report_completeness

-   report_conformance

-   ody_render_quality_report [ody_verify_completeness, ody_verify_conformance, report_completeness, report_conformance]

-   ody_filter_fails

-   ody_report_fails

## **data_summary**

Functions to summarise dataframes.

-   complete_list

-   make_var_list

-   summary_tibble

-   summarise_continuos_var [summary_tibble]

-   summarise_discrete_var

-   make_continuous_detail_tbl

-   make_discrete_detail_tbl

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

-   import_rc [extract_data, get_single_field, get_dag]

-   get_single_field

-   get_dag

-   process_raw_dic

-   label_rc_import [process_raw_dic]

-   nest_rc

-   restore_attributes

-   ody_rc_import [import_rc, label_rc_import, rc_clean_single_form, nest_rc, restore_attributes]

-   select_rc_long

-   select_rc_classic

-   ody_rc_select [select_rc_long, select_rc_classic]

-   ody_rc_select_form

-   ody_rc_filter_subject

-   ody_rc_format

-   ody_rc_translate_meddra

-   ody_rc_translate_atc

-   ody_rc_view [restore_attributes, ody_rc_completeness, ody_rc_format, ody_summarise_df]

-   ody_rc_completeness [ody_verify_completeness, get_conditions_from_metadata]

-   ody_rc_spread [spreader]

-   spreader

-   ody_rc_add_import_date

-   ody_rc_add_site

-   ody_rc_add_label

-   ody_rc_get_metadata

-   ody_rc_search_ttm [ody_rc_select, ody_rc_format, ody_rc_filter_subject, ody_rc_spread]

## redcap_methods

Methods exported to external generic functions

-   print

## redcap_project

Functions to create and update RedCap projects in RStudio:

-   get_project_name

-   get_import_date

-   rc_init_dirs_files [get_project_name]

-   rc_store_datasets [get_project_name, get_import_date]

-   rc_init_update [get_project_name, rc_init_dirs_files, ody_rc_import, get_import_date, hardcode_values, rc_store_datasets, ] ADDIN

-   rc_refresh_datasets [rc_store_datasets, get_project_name] ADDIN

-   ody_add_to_datasets

-   ody_rc_current ADDIN

-   add_analysis_template ADDIN

-   myView

-   rc_view_metadata ADDIN

-   view_datasets ADDIN

-   ody_rc_timetravel

-   hardcode_value_longproj

-   hardcode_value_clasproj

-   hardcode_values [hardcode_value_longproj, hardcode_value_clasproj]

## **other_utils**

Miscelaneous utilities.

-   ody_add_version
-   ody_save_path
-   ody_change_names
-   ody_options
-   ody_proj_init
-   save_lock ADDIN
-   update_odytools ADDIN
-   compare_1_vs_others
-   ody_compare_1_vs_others [compare_1_vs_others]
-   check_renvlock
-   ody_gt2image
-   ody_apply_on_pattern
-   ody_label_df
-   ody_timetravel
-   ody_exofilter

## **aes**

Adverse Events related functions.

-   count_ae_max_grade

-   make_ae_tbl [count_ae_max_grade]

-   ody_make_ae_tbl [make_ae_table]

-   add_pct

-   ody_make_ae_gt [add_pct]

## **survival**

Survival related functions.

-   ody_extract_km

## **gpt**

-   ody_gpt

## **scores_ct**

Calcutation of scores commonly used in clinical trials.

-   ody_qlq_c30

## **pdf_extraction**

Tools to extract data from some pdf reports.

## **graphics**

Functions to plot graphics commonly used in the reports

-   ody_plot_violindotbox
-   ody_add_tbl_violindotbox

## **style_methods**

Methods for the generic ody_style wich are styling methods for different classes.

-   ody_style (generic)
-   ody_style.default
-   ody_style.tbl_ae_focus
-   ody_style.gtsummary
