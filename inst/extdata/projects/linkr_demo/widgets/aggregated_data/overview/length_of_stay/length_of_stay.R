visits <- d$data_subset$visit_occurrence %>% dplyr::collect()

mean_los <- if (nrow(visits) > 0) {
    visits %>%
        dplyr::mutate(
            los = as.numeric(
                difftime(
                    visit_end_datetime,
                    visit_start_datetime,
                    units = "days"
                )
            )
        ) %>%
        dplyr::summarise(
            mean_los = mean(los, na.rm = TRUE)
        ) %>%
        dplyr::pull(mean_los) %>%
        round(1)
} else {
    0
}

if (m$language == "en"){
    legend <- "average length of stay"
    los <- paste0(mean_los, " d")
} else if (m$language == "fr"){
    legend <- "durée moyenne de séjour"
    los <- paste0(mean_los, " d")
}

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
            style = "color: #4B937C; margin-bottom: 10px;",
            tags$i(class = "fas fa-clock fa-2x")
        ),
        div(
            style = "color: #4B937C; font-size: 36px; font-weight: bold; margin: 10px 0;",
            los
        ),
        div(
            style = "color: #666; font-size: 14px; text-transform: uppercase;",
            legend
        )
    )
}
res
