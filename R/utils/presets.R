refresh_presets <- function(
    session,
    preset_dir = "presets",
    selected = ""
) {
  
  if (!dir.exists(preset_dir)) {
    dir.create(
      preset_dir,
      recursive = TRUE
    )
  }
  
  preset_files <- list.files(
    path = preset_dir,
    pattern = "\\.json$",
    full.names = TRUE
  )
  
  preset_names <- tools::file_path_sans_ext(
    basename(preset_files)
  )
  
  choices <- c(
    "Bitte Preset auswählen" = "",
    setNames(
      preset_files,
      preset_names
    )
  )
  
  updateSelectInput(
    session = session,
    inputId = "preset_datei",
    choices = choices,
    selected = selected
  )
}


write_preset_file <- function(
    preset,
    preset_name,
    preset_dir = "presets"
) {
  
  if (!dir.exists(preset_dir)) {
    dir.create(
      preset_dir,
      recursive = TRUE
    )
  }
  
  safe_name <- gsub(
    "[^A-Za-z0-9_-]",
    "_",
    preset_name
  )
  
  preset_path <- file.path(
    preset_dir,
    paste0(safe_name, ".json")
  )
  
  jsonlite::write_json(
    x = preset,
    path = preset_path,
    auto_unbox = TRUE,
    pretty = TRUE
  )
  
  preset_path
}


read_preset_file <- function(path) {
  
  jsonlite::read_json(
    path = path,
    simplifyVector = TRUE
  )
}