patients <- d$visit_detail %>% dplyr::select(person_id, visit_occurrence_id, visit_detail_id)

add_patients_to_subset(patients)
