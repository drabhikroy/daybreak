# Run this file when opening Daybreak on a new computer. The package check is
# quick after the first run and keeps missing dependencies out of the session.

launch_file <- tryCatch(sys.frame(1)$ofile, error = function(e) NULL)
app_dir <- if (is.null(launch_file)) getwd() else dirname(normalizePath(launch_file))

source(file.path(app_dir, "install.R"), chdir = TRUE)
shiny::runApp(app_dir, launch.browser = TRUE)
