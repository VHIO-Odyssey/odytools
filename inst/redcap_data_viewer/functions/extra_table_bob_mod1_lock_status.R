
extra_table_bob_mod1_lock_status <- function(data_app, token) {
  
  sdv_events_and_forms <- 
    tibble(
      redcap_event_name = c(
        "preregistration_arm_1",
        "registration_arm_1",
        "ibasket_module_1_a_arm_1",
        "baseline_arm_1",
        "treatment_arm_1",
        "common_forms_arm_1",
        "end_of_treatment_arm_1",
        "followup_arm_1"
      ),
      instrument = list(
        c("preregistration"),
        c("registration"),
        c(
          "inclusion_and_exclusion_criteria_ibasket_modu_b2f9",
          "inclusion_and_exclusion_criteria_ibasket_modu_a5ac",
          "inclusion_and_exclusion_criteria_ibasket_module_1",
          "inclusion_and_exclusion_criteria_ibasket_modu_d7c8"
        ),
        c(
          "medical_history",
          "previous_malignancies",
          "prior_systemic_therapy",
          "urinalysis",
          "baseline_signs_and_symptoms",
          "clinical_assessment",
          "laboratory"
        ),
        c("treatment_module_1_atezolizumab"),
        c("concomitant_medication", "adverse_events", "tumor_measurements"),
        c("end_of_treatment"),
        c("survival_status")
      )
    ) |> 
    unnest(instrument) |> 
    mutate(
      sdv_required = "1"
    )
  
  mod1_pacs <- 
    ody_rc_select(data_app, mtb_registration_module) |> 
    filter(mtb_registration_module == "1") |> 
    pull(record_id)
  
  t_dates <- 
    ody_rc_select(data_app, t_date_m1_1) |> 
    select(
      record = record_id, 
      redcap_event_name, 
      instrument = redcap_form_name, 
      instance = redcap_instance_number, 
      treatment_date = t_date_m1_1
    ) |> 
    mutate(
      instance = as.integer(instance)
    )
  
  lock_status <-
    map(
      mod1_pacs,
      ~ get_lock_status(., api_key = token),
      .progress = "Getting lock status for Mod1 BOB"
    )
  
  lock_status_df <- 
    bind_rows(lock_status) |> 
    rename(locked = lock_status) |> 
    filter(
      !(instrument == "id_generation" & redcap_event_name != "preregistration_arm_1")
    )
  
  form_status <- 
    data_app |> 
    select(-redcap_repeating_event) |> 
    unnest(redcap_event_data) |> 
    select(-redcap_repeating_form) |> 
    mutate(
      complete_data = map(
        redcap_form_data, 
        ~ select(
          ., 
          record = record_id, 
          instance = redcap_instance_number,
          form_status = ends_with("_complete")
        )
      )
    ) |> 
    select(-redcap_form_data) |> 
    unnest(complete_data) |> 
    mutate(
      instance = if_else(is.na(instance), "1", instance) |> as.integer(),
      form_status = as.character(form_status)
    ) |> 
    rename(instrument = redcap_form_name)
  
  lock_status_df |> 
    left_join(
      sdv_events_and_forms, 
      by = c("redcap_event_name", "instrument")
    ) |>
    mutate(
      sdv_required = if_else(is.na(sdv_required), "0", sdv_required)
    ) |> 
    left_join(
      form_status, 
      by = join_by(record, redcap_event_name, instrument, instance)
    ) |>
    left_join(
      t_dates, 
      by = join_by(record, redcap_event_name, instrument, instance)
    ) |> 
    mutate(
      locked = as.numeric(locked),
      sdv_required = as.numeric(sdv_required),
      form_status = as.numeric(form_status)
    )
  
}