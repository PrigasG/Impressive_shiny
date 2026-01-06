wd <- commandArgs(TRUE)[1]  
rd <- dirname(wd)           

if (is.na(wd) || !nzchar(wd)) {
  stop("Please use run.bat (or LaunchApp.hta) to launch the application.")
}

# ------------------ Package bootstrap ------------------
req_file <- file.path(rd, "req.txt")
req <- if (file.exists(req_file)) readLines(req_file, warn = FALSE) else character(0)

lib_path <- if (
  file.exists(file.path(rd, "R", "bin", "R.exe")) &&
  .libPaths()[1] != file.path(rd, "R", "library")
) file.path(rd, "R", "library") else .libPaths()[1]

if (length(req) > 0) {
  missing_packages <- req[!(req %in% installed.packages()[, "Package"])]
  if (length(missing_packages) > 0) {
    install.packages(
      missing_packages,
      lib   = lib_path,
      repos = "https://cloud.r-project.org",
      clean = TRUE
    )
  }
}
suppressPackageStartupMessages(invisible(lapply(req, library, character.only = TRUE)))

# ------------------ Shiny run options ------------------
host <- Sys.getenv("LTC_HOST", unset = "127.0.0.1")
pref_port <- suppressWarnings(as.integer(Sys.getenv("LTC_PORT", unset = "3402")))
if (is.na(pref_port) || pref_port <= 0) pref_port <- 3402

Sys.setenv(SHINY_LOG_LEVEL = Sys.getenv("SHINY_LOG_LEVEL", unset = "INFO"))
options(shiny.fullstacktrace = TRUE)

run_on_port <- function(port) {
  browser_path <- file.path(rd, "chrome", "chrome.exe")
  if (file.exists(browser_path)) {
    shiny::runApp(
      appDir = wd,
      host   = host,
      port   = port,
      launch.browser = function(shinyurl) {
        system(paste0("\"", browser_path, "\" --app=", shinyurl, " -incognito"), wait = FALSE)
      }
    )
  } else {
    shiny::runApp(
      appDir = wd,
      host   = host,
      port   = port,
      launch.browser = TRUE
    )
  }
}

message(sprintf("[run] Trying preferred port %s:%d ...", host, pref_port))
tryCatch(
  run_on_port(pref_port),
  error = function(e) {
    message(sprintf("[run] Preferred port %d failed: %s", pref_port, conditionMessage(e)))
    alt <- httpuv::randomPort()
    message(sprintf("[run] Retrying on fallback port %s:%d ...", host, alt))
    run_on_port(alt)
  }
)
