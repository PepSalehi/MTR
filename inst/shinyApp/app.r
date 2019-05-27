appDir <- system.file("MTRdemand", package = "MTRdemand")
if (appDir == "") {
  stop("Could not find MTRdemand Try re-installing `MTRdemand`.", call. = FALSE)
}

shiny::runApp(appDir, display.mode = "normal")

# DemandPackage::launchApp()
