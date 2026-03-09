## **data_quality**

Data quality reports.

-   count_grouped_cases [dplyr, tibble, stringr, lubridate]
-   ody_verify_completeness [complete_list, dplyr, tibble, purrr, tidyselect, stringr]
-   ody_verify_conformance [count_grouped_cases, dplyr, tibble, purrr, stringr]
-   report_completeness [dplyr, tibble, reactable, reactablefmtr, htmltools, tidyr]
-   report_conformance [dplyr, reactable, reactablefmtr, htmltools]
-   ody_render_quality_report [ody_verify_completeness, ody_verify_conformance, report_completeness, report_conformance, lubridate, stringr, rlang, rmarkdown]
-   ody_filter_fails [dplyr]
-   ody_report_fails [tibble, gt]

## **data_summary**

Functions to summarise dataframes.

-   complete_list [tibble, dplyr, tidyr]
-   make_var_list [tibble, purrr, dplyr, tidyselect]
-   summary_tibble [tibble, lubridate]
-   summarise_continous_var [summary_tibble, tibble, tidyr, dplyr]
-   summarise_discrete_var [tibble, dplyr, tidyr]
-   make_discrete_detail_tbl [reactable, stringr, purrr, dplyr]
-   make_continuous_detail_table [reactable, ggplot2, ggridges, dplyr, forcats]
-   ody_summarise_df [complete_list, make_var_list, summarise_continous_var, summarise_discrete_var, make_discrete_detail_tbl, make_continuous_detail_table, ody_options, rlang, dplyr, tibble, purrr, tidyselect, reactable, reactablefmtr, htmltools, gtsummary, labelled]

## **data_wrangling**

Data wrangling functions.

-   get_timepoint [tibble, lubridate]
-   ody_define_timepoints [get_timepoint, purrr, dplyr, tidyr, tidyselect, lubridate]

## **pdx**

Functions for the Experimental Therapeutics Group.

-   ody_pdx_model_sensitivity [rlang, stringr, rmarkdown]

## **redcap**

Functions to export and work on redcap projects.

-   extract_data [httr, stringr]
-   import_rc [extract_data, get_single_field, get_dag, httr, readr, dplyr, tibble, tidyr, tidyselect, stringr, purrr, cli]
-   get_single_field [httr]
-   get_dag [httr]
-   process_raw_dic [stringr, purrr]
-   label_rc_import [process_raw_dic, cli, dplyr, tibble, purrr, stringr, labelled]
-   nest_rc [cli, dplyr, tibble, tidyr, purrr, stringr, tidyselect]
-   restore_attributes
-   rc_clean_single_form [dplyr, tidyselect]
-   ody_rc_import [import_rc, label_rc_import, rc_clean_single_form, nest_rc, restore_attributes, rstudioapi, cli, purrr, dplyr, stringr]
-   select_rc_long [dplyr, tidyr, tidyselect, stringr]
-   select_rc_classic [dplyr, tidyr, tidyselect, stringr]
-   ody_rc_simplify_selection [simplify_selection, purrr, dplyr]
-   simplify_selection [dplyr, tidyselect]
-   ody_rc_select [select_rc_long, select_rc_classic, rlang, purrr, dplyr, stringr]
-   ody_rc_select_form [rlang, dplyr, tidyr, tidyselect, stringr]
-   rc_select_viewer [select_rc_long, select_rc_classic, purrr, dplyr, stringr]
-   ody_rc_filter_subject [tidyr, dplyr]
-   ody_rc_format [dplyr, tidyselect, stringr, lubridate, labelled]
-   ody_rc_translate_meddra [tibble, dplyr]
-   ody_rc_translate_atc [tibble, dplyr]
-   wait_for_local_port [cli]
-   ody_rc_view [wait_for_local_port, rlang, stringr, callr, httpuv, rstudioapi]
-   filter_condition [dplyr]
-   get_conditions_from_metadata [filter_condition, dplyr, stringr, purrr, labelled]
-   ody_rc_completeness [ody_verify_completeness, get_conditions_from_metadata, report_completeness, rlang, dplyr, tidyselect, stringr, labelled]
-   ody_rc_spread [spreader, dplyr, tidyr, purrr, stringr]
-   spreader [ody_rc_format, dplyr, tidyr, purrr, stringr]
-   ody_rc_add_import_date [ody_rc_current, stringr, lubridate]
-   ody_rc_add_site [dplyr, tibble, stringr]
-   ody_rc_add_label [process_raw_dic, dplyr, labelled]
-   ody_rc_get_metadata [extract_data, httr]
-   ody_rc_search_ttm [ody_rc_select_form, ody_rc_format, ody_rc_filter_subject, ody_rc_spread, dplyr, stringr, tidyr]
-   ody_rc_check_metadata_availability [extract_data, httr, stringr]
-   ody_rc_arrange_master_therapy [dplyr, tidyr, stringr]
-   ody_rc_report_saps [get_single_field, dplyr, tidyr, tibble, stringr]

## **redcap_methods**

Methods exported to external generic functions

-   print.odytools_redcap [stringr, cli]
-   print.odytools_datasets_list [purrr, stringr, cli]
-   print.odytools_dataset [cli]

## **redcap_project**

Functions to create and update RedCap projects in RStudio:

-   get_project_name [here, stringr]
-   get_import_date [stringr, lubridate]
-   rc_init_dirs_files [get_project_name, here, stringr]
-   rc_make_datasets [get_project_name, get_import_date, here, purrr, rlang, dplyr, readr, stringr]
-   rc_make_datasets_no_redcap [here, purrr, rlang, dplyr]
-   rc_init_update [get_project_name, rc_init_dirs_files, ody_rc_import, hardcode_values, rc_make_datasets, rlang, here, stringr, rstudioapi, cli, readr] ADDIN
-   rc_refresh_datasets [rc_make_datasets, rc_make_datasets_no_redcap, get_project_name, here, rlang, cli] ADDIN
-   rc_back_up [get_project_name, get_import_date, here, dplyr, stringr]
-   ody_add_to_datasets
-   ody_rc_current [here, rlang, stringr, cli] ADDIN
-   add_analysis_template [get_project_name, here, stringr] ADDIN
-   myView
-   rc_view_metadata [here] ADDIN
-   view_datasets [dplyr, tibble, purrr] ADDIN
-   ody_rc_timetravel [here, stringr, cli]
-   hardcode_value_longproj [dplyr, stringr]
-   hardcode_value_clasproj [dplyr, stringr]
-   hardcode_values [hardcode_value_longproj, hardcode_value_clasproj, dplyr]
-   rc_export_data_dependencies [get_project_name, get_import_date, here, stringr, cli]

## **other_utils**

Miscelaneous utilities.

-   ody_add_version [lubridate, stringr]
-   ody_save_path [ody_add_version, stringr, here]
-   ody_change_names [dplyr, purrr]
-   ody_options
-   ody_proj_init [get_project_name, rlang, here, stringr]
-   save_lock [rlang, here, rstudioapi, renv] ADDIN
-   update_odytools [rstudioapi, devtools] ADDIN
-   compare_1_vs_others [tibble, dplyr]
-   ody_compare_1_vs_others [compare_1_vs_others, dplyr, purrr]
-   check_renvlock [here, git2r, lubridate, stringr, cli]
-   ody_gt2image [rlang, gt, magick]
-   ody_apply_on_pattern [dplyr, tidyselect, stringr]
-   ody_label_df [dplyr, tidyr, purrr, stringr, labelled]
-   ody_glue2lang [rlang, glue, stringr]
-   ody_timetravel [here, stringr, cli]
-   ody_exofilter [dplyr]
-   ody_read_data [stringr, here, rlang, readxl, vroom]
-   ody_repair_dates [rlang, dplyr, purrr, stringr, lubridate, janitor]
-   ody_write_xlsx [rlang, purrr, stringr, openxlsx2, here, glue]

## **aes**

Adverse Events related functions.

-   count_ae_max_grade [dplyr, tidyr]
-   make_ae_tbl [count_ae_max_grade, dplyr, tidyr, rlang, purrr, stringr]
-   ody_make_ae_tbl [make_ae_tbl, rlang, dplyr, purrr, stringr]
-   add_pct [dplyr, stringr]
-   ody_make_ae_gt [add_pct, purrr, dplyr, gt, stringr]

## **survival**

Survival related functions.

-   ody_extract_km [tibble, dplyr, gt]

## **gpt**

-   ody_gpt [rlang, rstudioapi, tidychatmodels, dplyr]
-   ody_correct [ody_gpt]

## **scores_ct**

Calcutation of scores commonly used in clinical trials.

-   check_values
-   qlq_c30_scores_transform
-   qlq_c30_scores_transform_1
-   qlq_c30_scores_scale [qlq_c30_scores_transform, qlq_c30_scores_transform_1]
-   qlq_c30_scores [qlq_c30_scores_scale, purrr, tibble, dplyr, stringr]
-   ody_qlq_c30_v3 [check_values, qlq_c30_scores, rlang, dplyr, tidyselect, purrr, labelled]

## **pdf_extraction**

Tools to extract data from some pdf reports.

-   make_analite_row [tibble, stringr]
-   correct_fail_data [dplyr, stringr, purrr]
-   ody_extract_vhlab_pdf [make_analite_row, correct_fail_data, rlang, pdftools, purrr, stringr, dplyr, lubridate, openxlsx]

## **graphics**

Functions to plot graphics commonly used in the reports

-   ody_plot_violindotbox [rlang, ggplot2, ggpubr, gghalves, dplyr, glue, gtsummary]
-   ody_add_tbl_violindotbox [ody_glue2lang, rlang, dplyr, tidyselect, gt, stringr, patchwork]

## **style_methods**

Methods for the generic ody_style wich are styling methods for different classes.

-   ody_style (generic)
-   ody_style.default
-   ody_style.gtsummary [rlang, gtsummary, gt]
-   ody_style.tbl_ae_focus [rlang, gtsummary, gtreg, dplyr, gt]
-   ody_style.ggsurvfit [rlang, ggsurvfit, ggplot2]

## **utils-pipe**

Pipe operator.

-   %>% [magrittr]
