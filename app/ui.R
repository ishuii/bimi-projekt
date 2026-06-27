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
          
          selectInput(
            inputId = "normalisierung_sidebar",
            label = "Normalisierungs Verfahren auswählen",
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
            label = "Distanz Matrix auswählen",
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
          
          conditionalPanel(
            condition = "input.clusterverfahren_sidebar == 'Custom-Linkage'",
            numericInput("alpha_a", "Alpha a", value = 0.5),
            numericInput("alpha_b", "Alpha b", value = 0.5),
            numericInput("beta", "Beta", value = 0),
            numericInput("gamma", "Gamma", value = 0)
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
        ),
        selectizeInput("focus_patient", "Lieblingspatient suchen", choices = NULL)
      )

    ),
    br(),
    div(style = "padding: 10px;", downloadButton("download_pdf", "PDF exportieren"))
  ),
  
  dashboardBody(
    useShinyFeedback(),
    useShinyjs(),
    
    tags$head(tags$style(
      HTML(
        "
             .main-header {position:fixed; width:100%;}
             .content-wrapper{padding-top: 50px !important;}            "
      )
    ), tags$style(
      HTML(
        "
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

       /* Treeview arrows/icons */
    .sidebar-menu li a .fa,
    .sidebar-menu li a .glyphicon {
      color: black !important;
    }

    .custom-box .box-header{
    background-color: #FBEEB9 !important;
    }

    .custom-box .box-title{
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

    /* Overrides SUCCESS box header */
    .box.box-success > .box-header {
      background-color: #FEFAEC !important;
      color: black !important;
      border-bottom: none;
    }

    /* Overrides PRIMARY box header */
    .box.box-primary > .box-header {
      background-color: #FEFAEC !important;
      color: black !important;
      border-bottom: none;
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

    #heatmap .radio label{
    color: black !important;
    }

    .selectize-input,
    .selectize-input input,
    .selectize-dropdown,
    .selectize-dropdown-content {
    color: black !important;
    }

    .heatmap-controls h1,
    .heatmap-controls h2,
    .heatmap-controls h3,
    .heatmap-controls h4,
    .heatmap-controls h5,
    .heatmap-controls h6 {
    color: black !important;
    }

    "
      )
    )),
    
    tabItems(
      tabItem(
        tabName = "Startseite",
        h2("Wilkommen zum Dashboard für Cluster Analyse"),
        
        actionButton('nextpage', 'Datei Hochladen')
      ),
      
      tabItem(
        tabName = "datei_hochladen",
        h2("CSV Datei hochladen"),
        
        fancyFileInput("Datei_csv", "CSV Datei hochladen", accept = ".csv"),
        withSpinner(
          uiOutput("upload_status"),
          type = 6,
          color = "#000000"
        ),
        
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
            title = "Datensatz Parametern einstellen",
            width = 12,
            class = "custom-box",
            
            selectizeInput(
              "pathways",
              "Pathways auswählen",
              
              choices = NULL,
              multiple = TRUE
            )
          )
        ),
        
        actionButton('confirm_button', "Weiter mit diesem Pathways"),
        actionButton('switchtab', 'Parametern Wählen'),
      ),
      
      tabItem(
        tabName = "parameter",
        h2("Bitte Parametern benötigt zur Cluster Analyse, auswählen"),
        
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
              label = "Normalisierungs Verfahren auswählen",
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
              label = "Distanz Matrix auswählen",
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

        disabled(
          actionButton("run", "Run Cluster Analyse", class = "btn-successful")
        ),
      ),

      tabItem(
        tabName = "heatmap",
        h2("Visualisierung"),
        
        navset_card_underline(
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
          ),
          
          nav_panel(
            "Grafikpanel",
            withSpinner(
              plotOutput("grafikpanel", height = "85vh", width = "100%"),
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
        actionButton('back', 'zurück zum Parametern wählen'),
        conditionalPanel(condition = "input.distanzmatrix == 'Minkowski-Distanz'", )
      )
    )
  )
)