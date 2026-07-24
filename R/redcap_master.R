#' Search Patients with Specific Treatment
#'
#' Filters RedCap data to identify patients who have received a specific treatment, suited for Master-like REDCap projects.
#'
#' @param rc_data A REDCap data object imported via `ody_rc_import`.
#' @param filter_expression An expression to filter treatments. It is applyed to the `antineoplasic_therapy` form.
#' @param variables_of_interest A character vector of additional variables to include in the output. Default variables are  "ttm_startdate", "ttm_enddate", "ttm_pddate" and "ttm_bestresponse".
#' @param join_rc_spread Logical. If TRUE, joins the filtered cases with the wider REDCap data spread. Defaults to TRUE.
#'
#' @return A tibble with the filtered cases, optionally combined with the wider REDCap data.
#' @export
ody_rc_search_ttm <- function(
  rc_data,
  filter_expression,
  variables_of_interest = c(
    "ttm_met_line_num",
    "ttm_name",
    "ttm_startdate",
    "ttm_enddate",
    "ttm_pddate",
    "ttm_bestresponse"
  ),
  join_rc_spread = TRUE
) {
  filter_expression <- rlang::enquo(filter_expression)

  filtered_cases <-
    ody_rc_select_form(rc_data, "antineoplasic_therapy") |>
    dplyr::filter(!!filter_expression) |>
    ody_rc_format() |>
    dplyr::select("dem_sap", tidyselect::all_of(variables_of_interest)) |>
    unique()

  if (join_rc_spread) {
    ody_rc_filter_subject(rc_data, filtered_cases$dem_sap) |>
      ody_rc_spread() |>
      dplyr::right_join(filtered_cases, by = "dem_sap") |>
      dplyr::relocate(
        tidyselect::all_of(variables_of_interest),
        .after = "dem_sap"
      )
  } else {
    filtered_cases
  }
}

# Helper function to relocate adjuvantttm in advanced setting in
# ody_rc_arrange_master_therapy
relocate_advanced_adjuvance <- function(case) {
  # Si alguna instancia no tiene setting definido o directamente no tiene
  # adjuvancia avanzada, no hay nada que reubicar, se devuelve el caso tal cual.
  has_missing_setting <- any(is.na(case$setting_number))
  has_adv_adj <- any(case$setting_number == 0.5)

  if (!has_adv_adj || has_missing_setting) {
    return(case)
  }

  # Entradas de adjuvancia avanzada.
  adv_adj_rows <- case |> dplyr::filter(.data$setting_number == 0.5)

  # Entradas de líneas metastásicas.
  adv_lines_rows <- case |> dplyr::filter(.data$setting_number >= 1)

  # Si no hay líneas metastásicas, no hay nada que reubicar, se devuelve el caso
  # tal cual.
  if (nrow(adv_lines_rows) == 0) {
    return(case)
  }

  # Si alguna de las líneas metastásicas no tiene fecha de referencia, no se
  # puede ubicar la adjuvancia, se devuelve el caso con el setting de las
  # adjuvancias modificado a NA.
  if (any(is.na(adv_lines_rows$reference_date))) {
    return(
      case |>
        dplyr::mutate(
          setting_number = dplyr::case_when(
            setting_number == 0.5 ~ NA_real_,
            TRUE ~ setting_number
          )
        )
    )
  }

  relocated_adv_adj_rows <-
    adv_adj_rows |>
    dplyr::mutate(
      setting_number = purrr::map_dbl(
        .data$reference_date,
        \(x) {
          # Se mira por fecha cuál es el next setting a la adjuvancia avanzada.
          next_setting <-
            adv_lines_rows |>
            dplyr::filter(.data$reference_date > x) |>
            dplyr::filter(
              .data$setting_number == min(.data$setting_number)
            ) |>
            dplyr::pull("setting_number") |>
            unique()

          # Se mira por fecha cuál es el prev setting a la adjuvancia avanzada.
          prev_setting <-
            adv_lines_rows |>
            dplyr::filter(.data$reference_date < x) |>
            dplyr::filter(
              .data$setting_number == max(.data$setting_number)
            ) |>
            dplyr::pull("setting_number") |>
            unique()

          # Si hay next setting y prev setting se ubica la adjuvancia
          # exactamente entre ambos. Esto resuelve el caso de que la adjuvancia
          # quede entre dos instancias con el mismo setting.
          if (length(next_setting) == 1 && length(prev_setting) == 1) {
            return(mean(c(next_setting, prev_setting)))
          }

          #Si  solo hay next setting se actualiza el setting de  la
          # adjuvancia avanazda respecto a este.
          if (length(next_setting) == 1 && length(prev_setting) == 0) {
            return(next_setting - 0.5)
          }

          # Si hay prev setting se actualiza el setting de  la adjuvancia
          # avanzada respecto a este.
          if (length(next_setting) == 0 && length(prev_setting) == 1) {
            return(prev_setting + 0.5)
          }
        }
      )
    )

  dplyr::bind_rows(
    case |> dplyr::filter(.data$setting_number != 0.5),
    relocated_adv_adj_rows
  )
}

#' Arrange Therapy Data in Master REDCap Projects
#'
#' Organize `antineoplasic_therapy` forms from master REDCap projects so
#' instances are ordered chronologically.
#'
#' @param rc_data A Master-like REDCap data object. Accepted pojects so far:
#'   - First Visits - Colorectal (PID 376)
#'
#' @return The input `rc_data` object with the antineoplasic_therapy form data
#'   arranged and updated with sequential `redcap_instance_number` values. The
#'   attribute "rearranged_cases" is added to the returned object, containing a
#'   tibble with the cases that have been rearranged, with their original and
#'   new instance numbers.
#'   When some patients cannot be fully arranged, the attribute
#'   "uncomplete_arrangements" is also added, containing one row per affected
#'   patient with summary fields on defined settings, undefined instances, and
#'   reference-date completeness.
#'   If the REDCap data does not belong to an accepted project, a warning is
#'   issued and the original data is returned unchanged.
#'
#' @export
ody_rc_arrange_master_therapy <- function(rc_data) {
  project_id <- attr(rc_data, "project_info")$project_id

  if (!project_id %in% c(376)) {
    warning(
      "The provided REDCap data does not belong to an accepted project. Returning original data unchanged."
    )
    return(rc_data)
  }

  therapy <-
    rc_data$redcap_form_data[
      rc_data$redcap_form_name == "antineoplasic_therapy"
    ][[1]]

  # Definición del orden de cada instancia.
  setting_order_v0 <-
    therapy |>
    dplyr::mutate(
      setting_fct_temp = labelled::to_factor(.data$ttm_setting),
      reference_date = dplyr::case_when(
        # Fecha para ordenar dentro de cada setting_number.
        !is.na(.data$ttm_startdate) ~ .data$ttm_startdate,
        !is.na(.data$ttm_enddate) ~ .data$ttm_enddate,
        !is.na(.data$ttm_pddate) ~ .data$ttm_pddate,
        .default = NA
      ),
      setting_number = dplyr::case_when(
        # Instancias con ttm_stat = "No" se asignan el orden -2, para que queden
        # antes de las adjuvancias.
        ttm_stat == "0" ~ "-2",
        setting_fct_temp == "Neoadjuvant" ~ "-1",
        # Se asigna el orden 0 a las adjuvancias después del primario.
        setting_fct_temp == "Adjuvant" & ttm_ad_int == "1" ~ "0",
        # Inicialnete se asigna el 0.5 a las adjuvancias avanzadas .
        # Posteriormente se reubican exactamente entre qué lineas metastasicas
        # cae. Como la reubicación se basa en la fech de referencia, que esta
        # exista también es un requerimiento.
        setting_fct_temp == "Adjuvant" &
          ttm_ad_int == "2" &
          !is.na(reference_date) ~ "0.5",
        setting_fct_temp == "Metastatic or Palliative" ~
          labelled::to_factor(.data$ttm_met_line_num)
      ) |>
        as.numeric(),
    ) |>
    dplyr::select(
      "dem_sap",
      "redcap_instance_number",
      "setting_number",
      "reference_date"
    )

  # Reubicación de las adjuvancias avanzadas.
  setting_order <-
    unique(setting_order_v0$dem_sap) |>
    purrr::map(
      ~ setting_order_v0 |>
        dplyr::filter(.data$dem_sap == .x) |>
        relocate_advanced_adjuvance() |>
        suppressWarnings(),
      .progress = "Arranging 'adjuvance' at advanced settings"
    ) |>
    purrr::list_rbind()

  # Casos que no se pueden ordenar del todo por falta de setting_number o fecha
  # de referencia.
  uncomplete_arrangements <-
    setting_order |>
    dplyr::group_by(.data$dem_sap, .data$setting_number) |>
    dplyr::summarise(
      n_instances = dplyr::n(),
      reference_dates_complete = all(!is.na(.data$reference_date))
    ) |>
    dplyr::ungroup() |>
    dplyr::mutate(
      intrasetting_arrangable = dplyr::case_when(
        is.na(.data$setting_number) ~ NA,
        .data$n_instances == 1 ~ TRUE,
        .data$n_instances > 1 & .data$reference_dates_complete ~ TRUE,
        .default = FALSE
      )
    ) |>
    dplyr::group_by(.data$dem_sap) |>
    dplyr::summarise(
      n_defined_settings = sum(!is.na(.data$setting_number)),
      internally_arrangable_settings = sum(
        .data$intrasetting_arrangable,
        na.rm = TRUE
      ),
      has_undefined_instances = any(is.na(.data$setting_number)),
      reference_dates_complete = all(.data$reference_dates_complete)
    ) |>
    dplyr::mutate(
      arrangement_success = dplyr::case_when(
        !.data$has_undefined_instances &
          .data$n_defined_settings ==
            .data$internally_arrangable_settings ~ TRUE,
        .data$has_undefined_instances & .data$reference_dates_complete ~ TRUE,
        .default = FALSE
      )
    ) |>
    dplyr::filter(!.data$arrangement_success) |>
    dplyr::select(-"arrangement_success")

  # Pacientes con alguna instancia con setting_number NA.
  any_missing_setting_pts <-
    setting_order |>
    dplyr::filter(is.na(.data$setting_number)) |>
    dplyr::pull("dem_sap") |>
    unique()

  # Ordenación según setting_number y dentro de este por reference_date.
  # Se aplica a los pacientes que no tienen ningún setting_number faltante.
  # Se crea un nuevo numero de instancia después de reordenar.
  arranged_therapy_by_setting <-
    therapy |>
    dplyr::filter(
      # Se excluyen pacientes con algún setting_number faltante.
      !.data$dem_sap %in% any_missing_setting_pts
    ) |>
    dplyr::left_join(
      setting_order,
      by = c("dem_sap", "redcap_instance_number")
    ) |>
    dplyr::arrange(
      .data$dem_sap,
      .data$setting_number,
      .data$reference_date
    ) |>
    dplyr::group_by(.data$dem_sap) |>
    dplyr::mutate(
      new_redcap_instance_number = dplyr::row_number() |> as.character(),
      .after = "redcap_instance_number"
    ) |>
    dplyr::ungroup() |>
    dplyr::select(-"setting_number", -"reference_date")

  # Ordenación solo por reference date para los pacientes con algún
  # setting_number faltante.
  # Ojo que aunque no haya setting_number, si todas las instancias tienen
  # reference_date, el caso se ordenará bien.
  arranged_therapy_by_date <-
    therapy |>
    dplyr::filter(.data$dem_sap %in% any_missing_setting_pts) |>
    dplyr::left_join(
      setting_order,
      by = c("dem_sap", "redcap_instance_number")
    ) |>
    dplyr::arrange(
      .data$dem_sap,
      .data$reference_date
    ) |>
    dplyr::group_by(.data$dem_sap) |>
    dplyr::mutate(
      new_redcap_instance_number = dplyr::row_number() |> as.character(),
      .after = "redcap_instance_number"
    ) |>
    dplyr::ungroup() |>
    dplyr::select(-"setting_number", -"reference_date")

  # Unión de ambos tipos de ordenación.
  arranged_therapy_v0 <-
    dplyr::bind_rows(
      arranged_therapy_by_setting,
      arranged_therapy_by_date
    ) |>
    dplyr::arrange(.data$dem_sap, as.numeric(.data$new_redcap_instance_number))

  # Antes de convertir las new_redcap_instance_number en las instancias
  # definitivas, miramos qué casos han cambiado de instancia para incluir esta
  # información en forma de attributo.
  arranged_cases <-
    arranged_therapy_v0 |>
    dplyr::filter(
      .data$new_redcap_instance_number != .data$redcap_instance_number
    ) |>
    dplyr::select(
      "dem_sap",
      "redcap_instance_number",
      "new_redcap_instance_number"
    )

  # Tabla final con el nuevo número de instancia.
  arranged_therapy <-
    arranged_therapy_v0 |>
    dplyr::select(-"redcap_instance_number") |>
    dplyr::rename(
      redcap_instance_number = "new_redcap_instance_number"
    )

  rc_data$redcap_form_data[
    rc_data$redcap_form_name == "antineoplasic_therapy"
  ] <-
    list(arranged_therapy)

  attr(rc_data, "rearranged_cases") <- arranged_cases
  if (nrow(uncomplete_arrangements) > 0) {
    warning(
      "Some patients (",
      nrow(uncomplete_arrangements),
      ") were not fully arranged. They are listed in the 'uncomplete_arrangements' attribute."
    )
    attr(rc_data, "uncomplete_arrangements") <- uncomplete_arrangements
  }

  rc_data
}
