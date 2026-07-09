ui <- dashboardPage(
  dashboardHeader(title = "ClusterIt!"),
  
  dashboardSidebar(
    width = 350,
    sidebarMenu(
      id = "tabs",
      menuItem("Startseite", tabName = "Startseite", icon = icon("home")),
      menuItem("Datei Hochladen", icon = icon("upload"), tabName = "datei_hochladen"),
      menuItem("Parametern Wählen", icon = icon("sliders"), tabName = "parameter"),
      menuItem("Visualisierung", tabName = "heatmap"),
      
      conditionalPanel(
        condition = 'input.tabs == "heatmap"',
        
        div(
          title = "Cluster Einstellungen",
          width = 6,
          solidHeader = TRUE,
          status = "warning",
          class = "heatmap-controls",
          id = "heatmap",
          
          selectInput(
            inputId = "clusterverfahren_sidebar",
            label = "Clusterverfahren auswählen",
            choices = c(
              "Single-Linkage",
              "Average-Linkage",
              "Complete-Linkage",
              "Custom-Linkage"
            )
          ),
          
          
          conditionalPanel(
            condition = "input.clusterverfahren_sidebar == 'Custom-Linkage'",
            numericInput("alpha_a", "Alpha a", value = 0.5),
            numericInput("alpha_b", "Alpha b", value = 0.5),
            numericInput("beta", "Beta", value = 0),
            numericInput("gamma", "Gamma", value = 0)
          ),
          
          
          selectInput(
            inputId = "normalisierung_sidebar",
            label = "Normalisierungsverfahren auswählen",
            choices = c(
              "Keine Normalisierung",
              "normalize_log_zscore",
              "normalize_log_only",
              "normalize_log_median_centering",
              "normalize_log_mad"
            )
          ),
          
          selectInput(
            inputId = "distanzmatrix_sidebar",
            label = "Distanzmatrix auswählen",
            choices = c(
              "Euklidische Distanz",
              "Manhattan-Distanz",
              "Minkowski-Distanz",
              "Canberra-Distanz",
              "Pearson-Distanz",
              "Winkeldistanz (Angular Seperation)"
            )
          ),
          
          conditionalPanel(
            condition = "input.distanzmatrix_sidebar == 'Minkowski-Distanz'",
            numericInput(
              inputId = "param_heatmap",
              label = "Parameter p eingeben",
              value = 1
            ),
            textOutput("result")
          ),
          
          radioButtons(
            inputId = "farbpaletten_sidebar",
            label = "Farbpalette für Heatmaps auswählen",
            choiceNames = list(
              tagList(
                "RdYlBu",
                
                tags$span(
                  class = "badge bg-info",
                  # Creates the blue box style from your image
                  style = "cursor: pointer; padding: 3px 6px; font-weight: bold;",
                  `data-toggle` = "popover",
                  `data-html` = "true",
                  # Allows text inside to wrap cleanly
                  title = "Standard",
                  # Bold title of the popover
                  `data-content` = "Farben: Rot, Gelb, Blau",
                  # Subtext
                  "?"
                )
              ),
              
              tagList(
                "viridis",
                
                tags$span(
                  class = "badge bg-info",
                  style = "cursor: pointer; padding: 3px 6px; font-weight: bold;",
                  `data-toggle` = "popover",
                  `data-html` = "true",
                  title = "viridis",
                  `data-content` = "Farben: Lila, Grün, Gelb",
                  "?"
                )
              ),
              
              tagList(
                "RdBu",
                
                tags$span(
                  class = "badge bg-info",
                  style = "cursor: pointer; padding: 3px 6px; font-weight: bold;",
                  `data-toggle` = "popover",
                  `data-html` = "true",
                  title = "Magma",
                  `data-content` = "Farben: Rot, Blau",
                  "?"
                )
              ),
              
              tagList(
                "PRGn",
                
                tags$span(
                  class = "badge bg-info",
                  style = "cursor: pointer; padding: 3px 6px; font-weight: bold;",
                  `data-toggle` = "popover",
                  `data-html` = "true",
                  title = "Magma",
                  `data-content` = "Farben: Lila, Grün",
                  "?"
                )
              )
            ),
            choiceValues = list("RdYlBu", "viridis", "RdBu", "PRGn")
          ),
          selectizeInput("focus_patient", "Patient suchen", choices = NULL, selected = "", multiple = FALSE,
                         options = list(placeholder = "Patient suchen...", 
                                        searchField = "label", allowEmptyOption = TRUE, plugins = list("remove_button"))),
        
          actionButton("refreshButton", "Parameter aktualisieren")
          
        )

      )

    ),
    br(),
    div(style = "padding: 10px; background-color: #D1D1D1; text-color: #000000", downloadButton("download_pdf", "PDF exportieren", class="pdf-button"))
  ),
  
  dashboardBody(
    useShinyFeedback(),
    useShinyjs(),
    
    tags$head(
      
      tags$style(HTML("
             html, body {
             height: 100%;
             }
             
             .wrapper{
             min-height: 100vh !important;
             }
             
             .main-sidebar{
             min-height: 100vh !important;
             }
        
             body{
             background-color: #FFFFFF !important;
             color: #000000 !important;
             }
             
             .main-header {position:fixed; width:100%;}
             
             .content-wrapper{
             background-color: #FFFFFF !important;
             padding-top: 60px !important; 
             margin-top:0px !important;}
                        
             .wrapper{
             background-color: #FFFFFF !important;}    
             
             .main-sidebar {
             position: fixed !important;
             height: 100vh !important;
             overflow-y: auto !important;
             }
             
             .left-side {
             min-height: 100% !important;
             }
             
             .wrapper {
             min-height: 100vh !important;
             }
             
             .content-wrapper {
             min-height: 100vh !important;
             }
                        
                        ")),
      
      
      tags$style(HTML("
      /* Main header */
      .main-header .logo {
        background-color: #D1D1D1 !important;
        color: #000000 !important;
      }
      
      .main-header .logo:hover {
      background-color: #FFFFFF !important;
      color: black !important;
    }

      .main-header .navbar {
        background-color: #D1D1D1 !important;
      }

      /* Sidebar */
      .main-sidebar {
        background-color: #D1D1D1 !important;
      }
      
      /* All sidebar text */
    .sidebar-menu > li > a {
      color: black !important;
    }
     
     /* Active menu item */
    .sidebar-menu > li.active > a {
      background-color: #ECECEC !important;
      color: black !important;
    }
    
    /* Sidebar hover */
    .sidebar-menu > li:hover > a {
     background-color: #ECECEC !important;
     color: #000000 !important;
    }
      
       /* Treeview arrows/icons */
    .sidebar-menu li a .fa,
    .sidebar-menu li a .glyphicon {
      color: black !important;
    }
    
    .sidebar-toggle,
    .main-header .sidebar-toggle,
    .main-header .navbar .sidebar-toggle {
    color: #000000 !important;
    }
    
    /* Sometimes it's rendered as icon inside */
    .sidebar-toggle .fa,
    .main-header .sidebar-toggle .fa {
    color: #000000 !important;
    }
    .custom-box .box-header{
    background-color: #FBEEB9 !important;
    }
    
    .abt-box .box-header{
    background-color: #FBEEB9 !important;
    }
    
    .custom-box .box-title{
    color: black !important;
    }
    
    .abt-box .box-title{
    color: black !important;
    }
    
    .cluster-box .box-header{
    background-color:  #FBEEB9 !important;
    }
    
    .cluster-box .box-title{
    color: black !important;
    }
    
    .preset-box .box-header{
    background-color:  #FBEEB9 !important;
    }
    
    .preset-box .box-title{
    color: black !important;
    }
    
    .box {
    border: 1px solid #000000 !important;
    box-shadow: none !important;
    }
    
    /* Overrides SUCCESS box header */
    .box.box-success > .box-header {
      background-color: #FEFAEC !important;
      color: black !important;
      border-bottom: 1px solid #000000 !important;
    }
    
    /* Overrides PRIMARY box header */
    .box.box-primary > .box-header {
      background-color: #FEFAEC !important;
      color: black !important;
      border-bottom: 1px solid #000000 !important;
    }
    
    #changes text in sidebar to black
    .heatmap-controls label {
    color: black !important;
    }
    
    .heatmap-controls .control-label {
    color: black !important;
    }
    
    .heatmap-controls .radio-label {
    color: black !important;
    }
    
    .heatmap-controls .form-group label{
    color: black !important;
    }
    
    #heatmap .radio label{
    color: black !important;
    }
    
    .pdf-button {
    background-color: #777777 !important;
    color: white !important;
    border: none !important;
    }
  
    "))
    ),
    
    tabItems(
      tabItem(
        tabName = "Startseite",
        h2("Willkommen bei ClusterIt!"),
        
        box(width = 12, class = "abt-box", status = "primary",
            title = tags$span(style = "font-size: 24px;", "Einleitung"),
            
            p(style = "font-size: 18px; line-height: 1.6;","ClusterIt! ist ein interaktives Analyse-Dashboard zur Untersuchung 
              biologischer Datensätze mithilfe von hierarchischem Clustering."),
            
            p(style = "font-size: 18px; line-height: 1.6;", "Das Dashboard ermöglicht die Auswahl relevanter Pathways, 
            die Vorbereitung und Normalisierung von Daten sowie die Visualisierung 
            von Gen- und Patienten-Clustern in Form von Heatmaps und Dendrogrammen."),
            
            tags$b(style = "font-size: 20px; line-height: 1.6;", "Workflow"),
            
            tags$ul(style = "font-size: 18px; line-height: 1.6;",
              tags$li("Laden Sie einen Datensatz im CSV-Format hoch"),
              tags$li("Wählen Sie relevante Pathways und Analyseparameter aus"),
              tags$li("Führen Sie die Clusteranalyse durch"),
              tags$li("Analysieren Sie die Ergebnisse anhand der Heatmap und Dendrogramme")
            ),
            
            p(style = "font-size: 18px; line-height: 1.6;", "Die Visualisierung unterstützt dabei, Muster, Ähnlichkeiten und 
              Gruppierungen innerhalb komplexer biologischer Daten zu erkennen.")
            
            ),
        
        
        actionButton('nextpage', 'Datei Hochladen')
      ),
      
      tabItem(
        tabName = "datei_hochladen",
        h2("CSV Datei hochladen"),
        
        fancyFileInput("Datei_csv", "CSV Datei hochladen", accept = ".csv"),
          uiOutput("upload_status"),
        
        fluidRow(box(
          width = 12,
          h4("NA-Fehlerbehandlung"),
          verbatimTextOutput("na_info"),
          
          actionButton(
            inputId = "drop_na",
            label = "NA-Spalten entfernen",
            class = "btn-warning"
          ),
        )),
        
        tableOutput("coverage_table"),
        
        fluidRow(
          box(
            title = "Pathways auswählen",
            width = 12,
            class = "custom-box",
            
            selectizeInput(
              "pathways",
              label = NULL,
              choices = NULL,
              multiple = TRUE
            )
          )
        ),
        
        actionButton('confirm_button', "Weiter mit diesem Pathways", disabled = TRUE),
      ),
      
      tabItem(
        tabName = "parameter",
        h2("Parameter auswahl für Cluster Analyse"),
        
        fluidRow(
          box(
            title = "Cluster Einstellungen",
            width = 12,
            solidHeader = TRUE,
            status = "success",
            class = "cluster-box",
            
            selectInput(
              inputId = "clusterverfahren",
              label = "Clusterverfahren auswählen",
              choices = c(
                "Single-Linkage",
                "Average-Linkage",
                "Complete-Linkage",
                "Custom-Linkage"
              )
            ),
            
            uiOutput("customInfo"),
            
            conditionalPanel(
              condition = "input.clusterverfahren == 'Custom-Linkage'",
              numericInput("alpha_a", "Alpha a", value = 0.5),
              numericInput("alpha_b", "Alpha b", value = 0.5),
              numericInput("beta", "Beta", value = 0),
              numericInput("gamma", "Gamma", value = 0)
            ),
            
            radioButtons(
              inputId = "farbpaletten",
              label = "Farbpalette für Heatmaps auswählen",
              choiceNames = list(
                tagList(
                  "RdYlBu",
                  
                  tags$span(
                    class = "badge bg-info",
                    # Creates the blue box style from your image
                    style = "cursor: pointer; padding: 3px 6px; font-weight: bold;",
                    `data-toggle` = "popover",
                    `data-html` = "true",
                    # Allows text inside to wrap cleanly
                    title = "Standard",
                    # Bold title of the popover
                    `data-content` = "Farben: Rot, Gelb, Blau",
                    # Subtext
                    "?"
                  )
                ),
                
                tagList(
                  "viridis",
                  
                  tags$span(
                    class = "badge bg-info",
                    style = "cursor: pointer; padding: 3px 6px; font-weight: bold;",
                    `data-toggle` = "popover",
                    `data-html` = "true",
                    title = "viridis",
                    `data-content` = "Farben: Lila, Grün, Gelb",
                    "?"
                  )
                ),
                
                tagList(
                  "RdBu",
                  
                  tags$span(
                    class = "badge bg-info",
                    style = "cursor: pointer; padding: 3px 6px; font-weight: bold;",
                    `data-toggle` = "popover",
                    `data-html` = "true",
                    title = "Magma",
                    `data-content` = "Farben: Rot, Blau",
                    "?"
                  )
                ),
                
                tagList(
                  "PRGn",
                  
                  tags$span(
                    class = "badge bg-info",
                    style = "cursor: pointer; padding: 3px 6px; font-weight: bold;",
                    `data-toggle` = "popover",
                    `data-html` = "true",
                    title = "Magma",
                    `data-content` = "Farben: Lila, Grün",
                    "?"
                  )
                )
                
              ),
              choiceValues = list("RdYlBu", "viridis", "RdBu", "PRGn")
            ),
            
            selectInput(
              inputId = "normalisierung",
              label = "Normalisierungsverfahren auswählen",
              choices = c(
                "Keine Normalisierung",
                "normalize_log_zscore",
                "normalize_log_only",
                "normalize_log_median_centering",
                "normalize_log_mad"
              )
            ),
            
            selectInput(
              inputId = "distanzmatrix",
              label = "Distanzmatrix auswählen",
              choices = c(
                "Euklidische Distanz",
                "Manhattan-Distanz",
                "Minkowski-Distanz",
                "Canberra-Distanz",
                "Pearson-Distanz",
                "Winkeldistanz (Angular Seperation)"
              )
            ),
            
            conditionalPanel(
              condition = "input.distanzmatrix == 'Minkowski-Distanz'",
              numericInput(
                inputId = "param_paramtab",
                label = "Parameter p eingeben",
                value = 1
              ),
              textOutput("result")
            ),
          ),
        ),
        
        fluidRow(
          box(
            title = "Preset speichern/laden",
            width = 12,
            solidHeader = TRUE,
            status = "primary",
            class = "preset-box",
            
            textInput("preset_name", "Name des Presets"),
            actionButton("save_preset", "Preset speichern"),
            br(),
            br(),
            selectInput("preset_datei", "Preset auswählen", choices = NULL),
            actionButton("load_preset", "Preset laden")
          )
        ),
        actionButton('back2upload', 'Zurück zum Datei Hochladen'),

        disabled(
          actionButton("run", "Run Cluster Analyse", class = "btn-successful")
        ),
        
        uiOutput("analysis_status")
      ),

      tabItem(
        tabName = "heatmap",
        h2("Visualisierung"),
        
        navset_card_underline(
          nav_panel(
            "Grafikpanel",
            withSpinner(
              plotlyOutput("grafikpanel", height = "85vh", width = "100%"),
              type = 6,
              color = "#000000"
            )
          ),
          
          nav_panel(
            "Dendrogram: Patient",
            withSpinner(
              plotlyOutput("patientDendrogram", height = "85vh", width = "100%"),
              type = 6,
              color = "#000000"
            )
          ),
          
          nav_panel(
            "Dendrogram: Gene",
            withSpinner(
              plotlyOutput("geneDendrogram", height = "85vh", width = "100%")
              ,
              type = 6,
              color = "#000000"
            )
          )
        ),

        verbatimTextOutput("debug_matrix"),

        tags$script(
          HTML(
            '
          $(document).ready(function(){
            $("body").popover({
              selector: "[data-toggle=popover]",
              trigger: "hover click", // Opens on hover OR click
              container: "body"       // Fixes layout breaking issues
            });
          });
        '
          )
        ),

        textOutput("selection_feedback"),
        actionButton('back', 'Zurück zum Parametern wählen'),
        conditionalPanel(condition = "input.distanzmatrix == 'Minkowski-Distanz'", )
      )
    )
  )
)