appDir <- system.file("ShinyApp", package = "MTRdemand")
if (appDir == "") {
  stop("Could not find MTRdemand Try re-installing `MTRdemand`.", call. = FALSE)
}
setwd(appDir)
shiny::shinyAppDir(".")
shiny::runApp(appDir, display.mode = "normal")
# DemandPackage::launchApp()
