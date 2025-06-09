concepts <- tibble::tribble(
    ~concept_id, ~concept_name, ~domain_id, ~vocabulary_id,
    3027018, 'Heart rate', 'Measurement', '',
    3027598, 'Mean blood pressure', 'Measurement', '',
    3004249, 'Systolic blood pressure', 'Measurement', '',
    3012888, 'Diastolic blood pressure', 'Measurement', '',
    21492240, 'Diastolic blood pressure by Noninvasive', 'Measurement', '',
    21492239, 'Systolic blood pressure by Noninvasive', 'Measurement', '',
    21492241, 'Mean blood pressure by Noninvasive', 'Measurement', ''
)

features <- list()
features_names <- c()
raw_data <- tibble::tibble()
data_datetimes_range <- c()
combined_features <- c()

sql <- glue::glue_sql('
    SELECT 
        measurement_concept_id AS concept_id,
        measurement_source_concept_id AS source_concept_id,
        measurement_datetime AS datetime,
        value_as_number
    FROM measurement 
    WHERE person_id = {m$selected_person} 
    AND (measurement_concept_id IN ({concepts$concept_id*}) OR measurement_source_concept_id IN ({concepts$concept_id*}))
    UNION
    SELECT 
        observation_concept_id AS concept_id,
        observation_source_concept_id AS source_concept_id,
        observation_datetime AS datetime, value_as_number
    FROM observation 
    WHERE person_id = {m$selected_person} 
    AND (observation_concept_id IN ({concepts$concept_id*}) OR observation_source_concept_id IN ({concepts$concept_id*}))
', .con = d$con)

raw_data <- DBI::dbGetQuery(d$con, sql) %>% tibble::as_tibble()

if (!is.na(m$selected_person)){
    sql <- glue::glue_sql('
        SELECT 
            MIN(visit_start_datetime) AS min_visit_start_datetime, 
            MAX(visit_end_datetime) AS max_visit_end_datetime 
        FROM visit_occurrence 
        WHERE person_id = {m$selected_person} 
    ', .con = d$con)

    data_datetimes_range <- DBI::dbGetQuery(d$con, sql)
}

if (length(data_datetimes_range) > 0){
    data_datetimes_range <- c(data_datetimes_range$min_visit_start_datetime, data_datetimes_range$max_visit_end_datetime)
    m$data_datetimes_range_26 <- data_datetimes_range
}

datetimes <- data_datetimes_range
if(!is.null(m$debounced_datetimes_timeline_10)) if (length(m$debounced_datetimes_timeline_10()) > 0) datetimes <- m$debounced_datetimes_timeline_10()

if (length(datetimes) > 0) m$datetimes_26 <- datetimes

for (concept_id in concepts$concept_id) {
    concept <- concepts %>% dplyr::filter(concept_id == !!concept_id)

    if (nrow(concept) > 0){
        if (concept$domain_id %in% c('Measurement', 'Observation')) {
            data <- raw_data

            if (nrow(data) > 0) {
                data <- data %>%
                    dplyr::filter(concept_id == !!concept_id | source_concept_id == !!concept_id) %>%
                    dplyr::select(datetime, value_as_number)

                if (nrow(data) > 0) {
                    fake_data <- tibble::tibble(
                        datetime = c(data_datetimes_range[[1]] - lubridate::seconds(1), data_datetimes_range[[2]] + lubridate::seconds(1)),
                        value_as_number = c(NA, NA)
                    )

                    data <- dplyr::bind_rows(fake_data, data)
                    data <- data %>% dplyr::arrange(datetime)

                    features[[paste0('concept_', concept_id)]] <- xts::xts(data$value_as_number, data$datetime)
                    features_names <- c(features_names, concept$concept_name)
                }
            }
        }
    }
}

if (length(features) > 0) combined_features <- do.call(merge, features)
if (length(features_names) > 0) colnames(combined_features) <- features_names

if (length(combined_features) > 0){
    fig <- 
        dygraphs::dygraph(combined_features, group = 'tab_10') %>%
        dygraphs::dyOptions(drawPoints = TRUE, pointSize = 2, useDataTimezone = TRUE) %>%
        dygraphs::dyRangeSelector(dateWindow = c(
            format(datetimes[[1]], '%Y-%m-%d %H:%M:%S'),
            format(datetimes[[2]], '%Y-%m-%d %H:%M:%S')
        )) %>%
        dygraphs::dyAxis('y', valueRange = c(0, NA))
}

fig
