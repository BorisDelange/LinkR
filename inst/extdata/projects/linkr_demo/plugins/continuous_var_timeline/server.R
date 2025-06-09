# Server - Main file

# Tabs

## All tabs
tabs <- c("figure", "figure_settings", "code", "general_settings")

observe_event(input$current_tab_trigger_%widget_id%, {
    
    tab <- input$current_tab_%widget_id%
    
    if (tab == "general_settings") shinyjs::hide("figure_settings_code_div_%widget_id%")
    else shinyjs::show("figure_settings_code_div_%widget_id%")
    
    sapply(paste0(setdiff(c("figure_settings", "code", "general_settings"), tab), "_div_%widget_id%"), shinyjs::hide)
    shinyjs::hide("settings_files_div_%widget_id%")
    
    if (tab != "code" | (tab == "code" & "projects_widgets_console" %in% user_accesses)) shinyjs::show(paste0(tab, "_div_%widget_id%"))
    
    if (tab %in% c("figure_settings", "code")){
        if (length(input$figure_and_settings_side_by_side_%widget_id%) > 0) if (input$figure_and_settings_side_by_side_%widget_id%) shinyjs::show("figure_div_%widget_id%")
        else shinyjs::hide("figure_div_%widget_id%")
        
        shinyjs::show("figure_settings_code_sidenav_%widget_id%")
        
        if (tab == "figure_settings") anti_tab <- "code"
        else anti_tab <- "figure_settings"
        
        shinyjs::runjs(paste0("
            var figureSettingsDiv = document.getElementById('", id, "-figure_settings_div_%widget_id%');
            var codeDiv = document.getElementById('", id, "-code_div_%widget_id%');
            
            if ('", tab, "' === 'figure_settings') {
                figureSettingsDiv.style.flexBasis = codeDiv.style.flexBasis;
            } else {
                codeDiv.style.flexBasis = figureSettingsDiv.style.flexBasis;
            }
        "))
    }
    else {
        shinyjs::hide("figure_settings_code_sidenav_%widget_id%")
        if (tab != "figure") shinyjs::hide("figure_div_%widget_id%")
    }
    
    # Prevent a bug with scroll into ace editor
    shinyjs::runjs("var event = new Event('resize'); window.dispatchEvent(event);")
})

# Figure settings
%import_script('server_figure_settings.R')%

# Code
%import_script('server_code.R')%

# Settings files
%import_script('server_settings_files.R')%

# General settings
%import_script('server_general_settings.R')%
