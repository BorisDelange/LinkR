#' @noRd
load_git_repo <- function(id, r, git_repo){
  
  if (length(r$loaded_git_repos) == 0) r$loaded_git_repos <- tibble::tibble(unique_id = character(), datetime = character(), local_path = character(), repo = list())
  if (length(r$loaded_git_repos_objects) == 0) r$loaded_git_repos_objects <- list()
  
  if (git_repo$unique_id %not_in% r$loaded_git_repos$unique_id){
    
    repo_url <- git_repo %>% dplyr::pull(repo_url_address)
    local_path <- paste0(r$app_folder, "/temp_files/", r$user_id, "/git_repos/", git_repo$unique_id)
    
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
      dplyr::bind_rows(tibble::tibble(unique_id = git_repo$unique_id, datetime = now(), local_path = local_path))
    
    r$loaded_git_repos_objects[[git_repo$unique_id]] <- repo
  }
  
  return(r$loaded_git_repos %>% dplyr::filter(unique_id == git_repo$unique_id))
}