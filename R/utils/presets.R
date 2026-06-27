refresh_presets <- function(session = session) {
  
  if (!dir.exists("presets")) {
    dir.create("presets")
  }
  
  dateien <- list.files(path = "presets", pattern = "\\.json$", full.names = TRUE)
  choices <- c("Bitte Preset auswählen" = "", setNames(dateien, basename(dateien)))
  updateSelectInput(session, "preset_datei", choices = choices, selected = "")
}