#!/usr/bin/env Rscript


# Deploy this app to shinyapps.io.
tryCatch({
    # load env vars from load_dot_env if that package is intalled.
    somefakelib::load_dot_env()
    message("Loaded .env")
  },
  error = function(e) {return(NA)}
)

message("Deploying to shinyapps.")
rsconnect::setAccountInfo(name=Sys.getenv("SHINYAPPS_ACCOUNT"), token=Sys.getenv("SHINYAPPS_TOKEN"), secret=Sys.getenv("SHINYAPPS_SECRET"))
rsconnect::deployApp(Sys.getenv("PROJECT_ROOT"), appFileManifest=file.path(Sys.getenv("PROJECT_ROOT"), 'filemanifest.txt'), launch.browser = FALSE)
