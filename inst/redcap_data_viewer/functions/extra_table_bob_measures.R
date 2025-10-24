# Evaluaciones de tumor para pacientes del brazo 2C con tratamiento iniciado


extra_table_bob_measures <- function(redcap_data) {
  
  module <- 
    ody_rc_select_form(redcap_data, conclusion_of_mtb_portal) |> 
    ody_rc_format() |> 
    filter(!is.na(mtb_registration_module)) |> 
    rowwise() |> 
    mutate(
      arm = c_across(matches("^mtb_arms")) |> na.omit()
    ) |> 
    select(
      record_id, 
      module = mtb_registration_module, 
      arm
    )
  
  c1d1 <- 
    ody_rc_select(redcap_data, t_date_m1_1, t_date_m2_1, amivant_date_d1) |> 
    map(
      ~ filter(., redcap_instance_number == "1") |>  
        select(record_id, c1d1_date = last_col())
    ) |> 
    list_rbind() |> 
    ody_rc_format()
  
  module_c1d1 <- right_join(module, c1d1, by = "record_id")
  
  
  tumor_evaluations <- 
    ody_rc_select(
      redcap_data, 
      tm_timepoint, 
      tm_cycle_num,
      tm_date_measu,
      tm_tl_measu_sum,
      tm_response_tl,
      tm_response_ntl,
      tm_nl_stat,
      tm_response_overall
    ) |> 
    ody_rc_format() |> 
    select(
      -redcap_event_name,
      -redcap_form_name,
      -redcap_instance_type,
      -redcap_instance_number
    )
  
  end_of_treatment <-
    ody_rc_select(redcap_data, eot_reason) |> 
    ody_rc_format() |>
    select(record_id, eot_reason)
  
  
  left_join(module_c1d1, tumor_evaluations, by = "record_id") |> 
    left_join(end_of_treatment, by = "record_id") |>
    mutate(
      weeks_since_c1d1 = time_length(
        tm_date_measu - c1d1_date, unit = "weeks"
      ) |> round(1),
      .after = tm_date_measu
    )
  
}

