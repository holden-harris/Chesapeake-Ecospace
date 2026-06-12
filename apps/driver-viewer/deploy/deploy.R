## -----------------------------------------------------------------------------
## deploy.R -- publish the driver-viewer to shinyapps.io.
##
## Prerequisites (one-time):
##   1. install.packages("rsconnect")
##   2. Authenticate with YOUR shinyapps.io token (from the dashboard ->
##      Account -> Tokens -> Show -> "With rsconnect"):
##        rsconnect::setAccountInfo(name="<account>", token="<token>", secret="<secret>")
##   3. Stage the data first:  Rscript apps/driver-viewer/deploy/stage-data.R
##
## Then deploy from the REPO ROOT:
##   Rscript apps/driver-viewer/deploy/deploy.R
## -----------------------------------------------------------------------------

library(rsconnect)

APP_DIR  <- "apps/driver-viewer"
APP_NAME <- "ches-icat-env-drivers"   # -> https://<account>.shinyapps.io/ches-icat-env-drivers/

if (!dir.exists(file.path(APP_DIR, "data"))) {
  stop("No data/ folder. Run deploy/stage-data.R first.")
}
if (nrow(rsconnect::accounts()) == 0) {
  stop("No shinyapps.io account configured. Run rsconnect::setAccountInfo(...) first ",
       "(see the header of this script).")
}

## Explicit file list. rsconnect scans the bundled files for package
## dependencies, so EXCLUDING deploy/ and R/prebuild.R here (not just via
## .rscignore, which only affects upload) keeps rsconnect + the non-CRAN
## rnaturalearth* out of the manifest. Keep this in sync with bundle_files().
bundle_files <- function(app_dir) {
  all  <- list.files(app_dir, recursive = TRUE, all.files = FALSE, no.. = TRUE)
  drop <- grepl("^deploy/", all) |
          all %in% c("R/prebuild.R", "smoke-test.R", "manifest.json")
  all[!drop]
}

rsconnect::deployApp(
  appDir      = APP_DIR,
  appFiles    = bundle_files(APP_DIR),
  appName     = APP_NAME,
  appTitle    = "Chesapeake environmental drivers (Ches-ICAT)",
  forceUpdate = TRUE,
  launch.browser = FALSE
)
