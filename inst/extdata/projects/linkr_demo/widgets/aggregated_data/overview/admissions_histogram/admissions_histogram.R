hosp_admissions <- d$data_subset$visit_occurrence %>% dplyr::collect()

if (m$language == "en"){
    title <- "Number of hospital admissions over time"
    x_legend <- "Date"
    y_legend <- "Count"
} else if (m$language == "fr"){
    title <- "Nombre d'admissions hospitalières en fonction du temps"
    x_legend <- "Date"
    y_legend <- "Occurrences"
}

if (nrow(hosp_admissions) > 0){
    
    time_range <- range(hosp_admissions$visit_start_date)
    breaks <- seq(time_range[1], time_range[2], length.out = 9)
    
    hosp_admissions %>%
        ggplot2::ggplot(ggplot2::aes(x = visit_start_date)) +
        ggplot2::geom_histogram(colour = "white", fill = "#428BCA", bins = 30) +
        ggplot2::theme_minimal() +
        ggplot2::scale_x_date(labels = scales::date_format("%Y-%m-%d"), breaks = breaks) +
        ggplot2::labs(title = title, x = x_legend, y = y_legend) +
        ggplot2::theme(
            axis.text.x = ggplot2::element_text(angle = 45, hjust = 1),
            plot.title = ggplot2::element_text(hjust = 0.5)
        )
}
