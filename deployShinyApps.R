#!/usr/bin/env Rscript


# Deploy this app to shinyapps.io.

message("Deploying to shinyapps.")
rsconnect::setAccountInfo(name=Sys.getenv("SHINYAPPS_ACCOUNT"), token=Sys.getenv("SHINYAPPS_TOKEN"), secret=Sys.getenv("SHINYAPPS_SECRET"))
rsconnect::deployApp(Sys.getenv("PROJECT_ROOT"),
                     appName="ui-data-explorer",
                     appFileManifest=file.path(config::get("PROJECT_ROOT"), 'filemanifest.txt'), 
                     launch.browser = FALSE)
