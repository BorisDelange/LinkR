patients <- d$data_subset$person %>% dplyr::collect()

if (m$language == "en") legend <- "patients" else if (m$language == "fr") legend <- "patients"

res <- div()

if (nrow(patients) > 0) {
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
            style = "color: #2C699A; margin-bottom: 10px;",
            tags$i(class = "fas fa-hospital-user fa-2x")
        ),
        div(
            style = "color: #2C699A; font-size: 36px; font-weight: bold; margin: 10px 0;",
            nrow(patients)
        ),
        div(
            style = "color: #666; font-size: 14px; text-transform: uppercase;",
            legend
        )
    )
}

res
