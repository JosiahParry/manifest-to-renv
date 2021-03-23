
<!-- README.md is generated from README.Rmd. Please edit that file -->

# Convert manifest.json to renv.lock

As the R community moves away from packrat towards its successor, renv,
it is becoming increasingly more commonplace to convert from packrat to
renv. But how can we take a manifest.json file and turn it into an
renv.lock.

The file `manifest-to-renv.R` creates a function to do this using a
not-yet-exported R6 object from {renv}. This functionality is subject to
change.

Below is the function definition.

``` r
library(renv)
#> 
#> Attaching package: 'renv'
#> The following object is masked from 'package:stats':
#> 
#>     update
#> The following objects are masked from 'package:utils':
#> 
#>     history, upgrade
#> The following objects are masked from 'package:base':
#> 
#>     load, remove
library(tidyverse)
#> ── Attaching packages ─────────────────────────────────────── tidyverse 1.3.0 ──
#> ✓ ggplot2 3.3.0     ✓ purrr   0.3.4
#> ✓ tibble  3.0.5     ✓ dplyr   1.0.5
#> ✓ tidyr   1.1.2     ✓ stringr 1.4.0
#> ✓ readr   1.4.0     ✓ forcats 0.5.0
#> ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
#> x dplyr::filter() masks stats::filter()
#> x dplyr::lag()    masks stats::lag()
#> x purrr::modify() masks renv::modify()

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
```

Below creates an `renv.lock` file from the `manifest.json` file.

    manifest_to_renv("manifest.json")
