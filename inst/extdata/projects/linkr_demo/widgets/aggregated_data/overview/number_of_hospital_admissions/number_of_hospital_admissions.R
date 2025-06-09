visits <- d$data_subset$visit_occurrence %>% dplyr::collect()

if (m$language == "en") legend <- "stays" else if (m$language == "fr") legend <- "séjours"

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
            style = "color: #F4A261; margin-bottom: 10px;",
            tags$i(class = "fas fa-hospital fa-2x")
        ),
        div(
            style = "color: #F4A261; font-size: 36px; font-weight: bold; margin: 10px 0;",
            nrow(visits)
        ),
        div(
            style = "color: #666; font-size: 14px; text-transform: uppercase;",
            legend
        )
    )
}
res
