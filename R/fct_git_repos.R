#' @noRd
load_git_repo <- function(id, r, git_repo){
  
  local_path <- ""
  
  if (git_repo$unique_id %not_in% r$loaded_git_repos$unique_id){
    
    repo_url <- git_repo %>% dplyr::pull(repo_url_address)
    local_path <- paste0(r$app_folder, "/temp_files/", r$user_id, "/git_repos/", paste0(sample(c(0:9, letters[1:6]), 64, TRUE), collapse = ''))
    
    credentials <- git2r::cred_user_pass("linkr_user", "")
    
    repo <- git2r::clone(repo_url, local_path, credentials = credentials, progress = FALSE)
    # git2r::config(repo, user.name = "username", user.email = "")
    
    # Create dirs & files that don't exist
    for (dir in c("datasets", "plugins", "data_cleaning_scripts", "projects")){
      dir_path <- paste0(local_path, "/", dir)
      file_path <- paste0(local_path, "/", dir, "/.gitkeep")
      xml_file_path <- paste0(dir_path, "/", dir, ".xml")
      
      if (!dir.exists(dir_path)) dir.create(dir_path)
      if (!file.exists(file_path)) file.create(file_path)
      if (!file.exists(xml_file_path)){
        xml <- XML::newXMLDoc()
        XML::newXMLNode(dir, doc = xml)
        XML::saveXML(xml, file = xml_file_path)
      }
    }
    
    readme_file_path <- paste0(local_path, "/README.md")
    if (!file.exists(readme_file_path)) file.create(readme_file_path)
    
    # Create .gitignore with .DS_Store in it (it is a Mac file)
    writeLines(".DS_Store", paste0(local_path, "/.gitignore"))
    
    # Add this repo to loaded repos
    r$loaded_git_repos <- 
      r$loaded_git_repos %>%
      dplyr::bind_rows(tibble::tibble(unique_id = r$git_repo$unique_id, datetime = now()))
  }
  
  return(local_path)
}