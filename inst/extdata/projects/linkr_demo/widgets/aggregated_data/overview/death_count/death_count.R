visits <- d$data_subset$visit_occurrence %>% dplyr::collect()
deaths <- d$data_subset$death %>% dplyr::collect()

death_rate <- if (nrow(visits) > 0 && nrow(deaths) > 0) {
    deaths_during_stay <- deaths %>%
        dplyr::inner_join(visits, by = "person_id") %>%
        dplyr::filter(
            death_datetime >= visit_start_datetime,
            death_datetime <= visit_end_datetime
        )
    
    round((nrow(deaths_during_stay) / nrow(visits)) * 100, 1)
} else {
    0
}

if (m$language == "en") legend <- "mortality" else if (m$language == "fr") legend <- "mortalité"

res <- div()
if (nrow(visits) > 0) {
    res <- 
    div(
        style = "
            text-align: center; 
            height: 100%; 
            display: flex;
            align-items: center;
            flex-direction: column;
            justify-content: center;
        ",
        div(
            style = "color: #E35D6A; margin-bottom: 10px;",
            tags$i(class = "fas fa-heart-broken fa-2x")
        ),
        div(
            style = "color: #E35D6A; font-size: 36px; font-weight: bold; margin: 10px 0;",
            paste0(death_rate, " %")
        ),
        div(
            style = "color: #666; font-size: 14px; text-transform: uppercase;",
            legend
        )
    )
}
res
