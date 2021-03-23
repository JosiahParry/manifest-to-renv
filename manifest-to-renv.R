library(renv)
library(tidyverse)

# Create empty lockfile. Use system call because it will be completely empty
# system("touch renv.lock")

manifest_to_renv <- function(manifest) {
  if (!file.exists("renv.lock")) {
    rlang::inform("Creating an empty renv.lock file.")
    file.create("renv.lock")
  }
  
  # create R6 objec
  lock <- renv:::lockfile(file = "renv.lock")  

  # read in manifest
  manifest <- jsonlite::read_json(manifest)
  
  # extract dependencies
  depends <- manifest$packages %>% 
    map_chr(pluck, "description", "Version") %>% 
    enframe() %>% 
    mutate(purl = glue::glue("{name}@{value}"))
  
  # extract repos
  repos <- manifest$packages %>% 
    map_chr(pluck, "Source") %>% 
    enframe() %>% 
    mutate(repo = map_chr(manifest$packages, pluck, "Repository")) %>% 
    distinct(value, repo) %>% 
    deframe() %>% 
    as.list()
  
  # update repos
  do.call(lock$repos, repos)
  
  # convert deps
  deps_list <- depends %>% 
    select(name, purl) %>% 
    deframe() %>% 
    as.list()
  
  # update dependencies
  do.call(lock$add, deps_list)
  
  # write the lockfile
  lock$write("renv.lock")
  # what is missing is the R version.
}

manifest_to_renv("manifest.json")









