server <- function(input, output, session) {
  cat("===== SERVER FUNCTION LOADED =====\n")
  
  cluster_result <- reactiveVal(NULL)
  cluster_bundle <- reactiveVal(NULL)
  skip_mink1 <- reactiveVal(FALSE)
  skip_mink2 <- reactiveVal(FALSE)
  skip_pathways <- reactiveVal(FALSE)
  
  current_warn <- reactiveVal(NULL)
  d_mat_result <- reactiveVal(NULL)
  pathway_list <- reactiveVal()
  coverage_result <- reactiveVal(NULL)
  prepared_data <- reactiveVal(NULL)
  
  tree_patient <- reactiveVal(NULL)
  order_patient <- reactiveVal(NULL)
  cluster_patient <- reactiveVal(NULL)
  patient_names <- reactiveVal(NULL)
  class_labels <- reactiveVal(NULL)
  selected_patient <- reactiveVal(NULL)
  
  tree_gene <- reactiveVal(NULL)
  order_gene <- reactiveVal(NULL)
  cluster_gene <- reactiveVal(NULL)
  gene_names <- reactiveVal(NULL)
  
  heatmap_store <- reactiveVal(NULL)
  patient_store <- reactiveVal(NULL)
  gene_store <- reactiveVal(NULL)

  dataset_name <- reactiveVal(NULL)

  clust_config <- reactiveValues(
    method = "Single-Linkage",
    normalisation = "normalize_log_zscore",
    distance = "Euklidische Distanz",
    palette = "RdYlBu",
    alpha_a = 0.5,
    alpha_b = 0.5,
    beta = 0,
    gamma = 0,
    minkowski_p = 1
  )
  distance_cache <- reactiveValues(
    key = NULL,
    patient = NULL,
    gene = NULL
  )
  #-------------------UPLOAD DATASET---------------------
  
  output$Beispieltext <- renderText({
    paste("Deine Datei:", input$x)
  })
  preset_values <- reactiveVal(list()) #Create reactive variable list
  preset_dir <- "presets"
  session$onFlushed(function() {
    refresh_presets(session)
  }, once = TRUE)
  options(shiny.maxRequestSize = 300 * 1024^2)
  
  # CSV IMPORT BACKEND
  daten_original <- reactiveVal(NULL)
  daten_aktuell <- reactiveVal(NULL)
  na_infos <- reactiveVal(NULL)
  
  observeEvent(input$Datei_csv, {
    req(input$Datei_csv)
    
    shinyjs::disable("confirm_button")
    
    output$upload_status <- renderUI({
      div(
        style = "font-size: 16px; color: black;",
        icon("spinner", class = "fa-spin"),
        "Datei wird hochgeladen, bitte warten..."
      )
    })
    
    withProgress(
      message = "Datei wird verarbeitet...",
      value = 0,
      {
        incProgress(0.2, detail = "CSV-Datei wird eingelesen")
        
        uploaded <- read_uploaded_csv(input$Datei_csv$datapath)
        
        dataset_name(tools::file_path_sans_ext(input$Datei_csv$name))
        
        incProgress(0.5, detail = "NA-Werte werden geprüft")
        
        cleaned <- auto_clean_na_upload(uploaded$df)
        
        if (!check_50_na(cleaned$info)) {
          
          cleaned <- User_handle_na_decision(
            df = cleaned$df,
            info = cleaned$info,
            action = "mean"
          )
        }
        
        incProgress(0.8, detail = "Datensatz wird gespeichert")
        
        daten_original(cleaned$df)
        daten_aktuell(cleaned$df)
        na_infos(cleaned$info)
        
        
        
        distance_cache$key <- NULL
        distance_cache$patient <- NULL
        distance_cache$gene <- NULL
        
        incProgress(1, detail = "Fertig")
      }
    )
    
    output$upload_status <- renderUI({
      div(
        style = "font-size: 16px; font-weight: bold; color: #000000; margin-top: 10px;",
        icon("check-circle"),
        "Datei erfolgreich hochgeladen und geprüft."
      )
    })
    
    if (check_50_na(cleaned$info) && !isTRUE(cleaned$info$bereits_bereinigt)) {
      shinyjs::disable("confirm_button")
    } else {
      shinyjs::enable("confirm_button")
    }
    
    session$sendInputMessage("Datei_csv", list(value = character(0)))
  })
  
  output$na_info <- renderPrint({
    
    info <- na_infos()
    
    if (is.null(info)) {
      cat("Noch keine CSV-Datei hochgeladen.")
      return(invisible(NULL))
    }
    
    cat("NA-Status\n")
    cat("---------\n")
    cat("Anzahl aller NA-Werte:", info$na_gesamt, "\n")
    cat("Zeilen mit mindestens einem NA-Wert:", info$zeilen_mit_na, "\n")
    cat("Spalten mit mindestens einem NA-Wert:", length(info$spalten_mit_na), "\n")
    cat("Zeilen gesamt:", info$zeilen_gesamt, "\n")
    cat("Spalten gesamt:", info$spalten_gesamt, "\n\n")
    
    cat("Automatisch entfernte 100%-NA-Zeilen:", length(info$auto_removed_rows), "\n")
    if (length(info$auto_removed_rows) > 0) {
      print(info$auto_removed_rows)
    }
    
    cat("Automatisch entfernte 100%-NA-Spalten:", length(info$auto_removed_cols), "\n")
    if (length(info$auto_removed_cols) > 0) {
      print(info$auto_removed_cols)
    }
    
    cat("\nZeilen mit mindestens 50% NA:", length(info$rows_over_50_na), "\n")
   
    
    
    cat("\nSpalten mit mindestens 50% NA:", length(info$cols_over_50_na), "\n")
    if (length(info$cols_over_50_na) > 0) {
      print(info$cols_over_50_na)
    }
    
    if (isTRUE(info$bereits_bereinigt)) {
      cat("\nStatus: User-Entscheidung wurde angewendet.\n")
      
      cat("Entfernte 50%-NA-Zeilen:", length(info$removed_50_rows), "\n")
      if (length(info$removed_50_rows) > 0) {
        print(info$removed_50_rows)
      }
      
      cat("Entfernte 50%-NA-Spalten:", length(info$removed_50_cols), "\n")
      if (length(info$removed_50_cols) > 0) {
        print(info$removed_50_cols)
      }
      
      cat("Durch Mittelwert ersetzte NA-Werte:", info$imputed_values, "\n")
      
    } else {
      cat("\nStatus: Auto-Cleanup wurde durchgeführt. User-Entscheidung steht noch aus.\n")
    }
    
    invisible(NULL)
  })
  
  output$na_decision_ui <- renderUI({
    
    info <- na_infos()
    
    if (is.null(info)) {
      return(NULL)
    }
    
    if (!check_50_na(info)) {
      return(NULL)
    }
    
    if (isTRUE(info$bereits_bereinigt)) {
      return(NULL)
    }
    
    tagList(
      br(),
      
      actionButton(
        inputId = "na_replace_mean",
        label = "50%-Zeilen/Spalten behalten und NA durch Mittelwert ersetzen",
        class = "na-mean-button"
      ),
      
      br(),
      br(),
      
      actionButton(
        inputId = "na_drop_and_replace",
        label = "50%-Zeilen/Spalten entfernen und Rest durch Mittelwert ersetzen",
        class = "na-drop-button"
      )
    )
  })
  
  
  
  observeEvent(input$na_replace_mean, {
    
    req(daten_aktuell())
    req(na_infos())
    
    result <- User_handle_na_decision(
      df = daten_aktuell(),
      info = na_infos(),
      action = "mean"
    )
    
    daten_aktuell(result$df)
    na_infos(result$info)
    
    distance_cache$key <- NULL
    distance_cache$patient <- NULL
    distance_cache$gene <- NULL
    shinyjs::enable("confirm_button")
    
    showNotification(
      "NA-Werte wurden zeilenweise durch Mittelwerte ersetzt.",
      type = "message"
    )
  })
  
  observeEvent(input$na_drop_and_replace, {
    
    req(daten_aktuell())
    req(na_infos())
    
    result <- User_handle_na_decision(
      df = daten_aktuell(),
      info = na_infos(),
      action = "drop"
    )
    
    daten_aktuell(result$df)
    na_infos(result$info)
    
    distance_cache$key <- NULL
    distance_cache$patient <- NULL
    distance_cache$gene <- NULL
    
    shinyjs::enable("confirm_button")
    
    showNotification(
      "50%-NA-Zeilen und 50%-NA-Spalten wurden entfernt. Restliche NA-Werte wurden zeilenweise durch den Mittelwert ersetzt.",
      type = "message"
    )
  })
  
  
  #-----------------------FINISH DATASET UPLOAD---------------------------------
  
  ##############################################################################
  # PDF EXPORT 
  ##############################################################################
  
  daten <- reactive({
    req(daten_aktuell())
    daten_aktuell()
  })
  
  produced_pdfs <- "tests/produced_pdfs"
  
  if (!dir.exists(produced_pdfs)) {
    dir.create(produced_pdfs, recursive = TRUE)
  }
  
  watched_pdf <- reactivePoll(intervalMillis = 2000, session = session, 
                              checkFunc = function () pdf_check(produced_pdfs), valueFunc = function() pdf_value(produced_pdfs))
  
  output$download_pdf <- downloadHandler(
    filename = paste0("ClusterIt_Report_", Sys.Date(), ".pdf"),
    contentType = "application/pdf",
    content = function(file) {pdf_content(file, watched_pdf, daten_aktuell, input, clust_config)} 
  )

  ##############################################################################
  # PDF EXPORT ENDE
  ##############################################################################
  
  observeEvent(input$anzahlcluster, {
    # Save User Choice Cluster
    tmp <- preset_values()
    tmp$anzahlcluster <- input$anzahlcluster
    preset_values(tmp)
  })
  
  observeEvent(input$clusterverfahren, {
    #Save User Choice Clusterfunction
    tmp <- preset_values()
    tmp$clusterverfahren <- input$clusterverfahren
    preset_values(tmp)
  })
  
  observeEvent(input$distanzmatrix, {
    #Save User Choice distance
    tmp <- preset_values()
    tmp$distanzmatrix <- input$distanzmatrix
    preset_values(tmp)
    
  })
  
  observeEvent(input$normalisierung, {
    #Save User Choice Normalisierung
    tmp <- preset_values()
    tmp$normalisierung <- input$normalisierung
    preset_values(tmp)
  })
  observeEvent(input$farbpaletten, {
    #Save User Choice Color
    tmp <- preset_values()
    tmp$farbpaletten <- input$farbpaletten
    preset_values(tmp)
  })
  
  observeEvent(input$load_preset, {
    #Load Preset after user choice
    req(input$preset_datei)
    
    preset <- jsonlite::fromJSON(input$preset_datei)
    
    if (!is.null(preset$anzahlcluster)) {
      updateNumericInput(session, "anzahlcluster", value = preset$anzahlcluster)
    }
    
    if (!is.null(preset$clusterverfahren)) {
      updateSelectInput(session,
                        "clusterverfahren",
                        selected = preset$clusterverfahren)
    }
    
    if (!is.null(preset$normalisierung)) {
      updateSelectInput(session, "normalisierung", selected = preset$normalisierung)
    }
    
    if (!is.null(preset$farbpaletten)) {
      updateRadioButtons(session, "farbpaletten", selected = preset$farbpaletten)
    }
    
    if (!is.null(preset$distanzmatrix)) {
      updateSelectInput(session, "distanzmatrix", selected = preset$distanzmatrix)
    }
  })
  
  observeEvent(input$nextpage, {
    updateTabItems(session, "tabs", selected = "datei_hochladen")
  })
  
  observeEvent(input$clusterverfahren, {
    clust_config$method <- input$clusterverfahren
  })
  
  observeEvent(input$farbpaletten, {
    clust_config$palette <- input$farbpaletten
  })
  
  observeEvent(input$distanzmatrix, {
    clust_config$distance <- input$distanzmatrix
  })
  
  observeEvent(input$normalisierung, {
    clust_config$normalisation <- input$normalisierung
  })
  
  
  observeEvent(input$clusterverfahren_sidebar, {
    clust_config$method <- input$clusterverfahren_sidebar
  })
  
  observeEvent(input$farbpaletten_sidebar, {
    clust_config$palette <- input$farbpaletten_sidebar
  })
  
  observeEvent(input$distanzmatrix_sidebar, {
    clust_config$distance <- input$distanzmatrix_sidebar
  })
  
  observeEvent(input$normalisierung_sidebar, {
    clust_config$normalisation <- input$normalisierung_sidebar
  })
  
  observeEvent(input$alpha_a, {
    clust_config$alpha_a <- input$alpha_a
  })
  
  observeEvent(input$alpha_b, {
    clust_config$alpha_b <- input$alpha_b
  })
  
  observeEvent(input$beta, {
    clust_config$beta <- input$beta
  })
  
  observeEvent(input$gamma, {
    clust_config$gamma <- input$gamma
  })
  
  output$customInfo <- renderUI({
    
    if(clust_config$method == "Custom-Linkage"){
      div(style = "background-color: #f8f9fa; border-left: 4px solid #007bff; padding: 8px 12px;
          margin-top: 5px; font-size: 14px;",
          
          icon("info-circle"),
          tags$b("Hinweis: "),
          "Nicht alle Werte sind sinnvoll. Bitte geben Sie nur geeignete Werte ein"
      )
    }
  })
  
  #---------------------Favorite Patient search---------------------------
  observeEvent(input$focus_patient, {
    if(is.null(input$focus_patient) || input$focus_patient == ""){
      selected_patient(NULL)
    }else{
      selected_patient(input$focus_patient)
    }
  }, ignoreNULL = FALSE)
  
  #------------------End of patient search-----------------------------------
  
  observe({
    updateSelectInput(session, "clusterverfahren", selected = clust_config$method)
    
    updateSelectInput(session, "clusterverfahren_sidebar", selected = clust_config$method)
    
    updateSelectInput(session, "distanzmatrix", selected = clust_config$distance)
    
    updateSelectInput(session, "distanzmatrix_sidebar", selected = clust_config$distance)
    
    updateSelectInput(session, "normalisierung", selected = clust_config$normalisation)
    
    updateSelectInput(session,
                      "normalisierung_sidebar",
                      selected = clust_config$normalisation)
    
    updateRadioButtons(session, "farbpaletten", selected = clust_config$palette)
    
    updateRadioButtons(session, "farbpaletten_sidebar", selected = clust_config$palette)
  })
  
  run_analysis <- function() {
    cat("Analysis started\n")
 
    
    withProgress(
      message = "Analyse gestartet...",
      value = 0,
      {
        incProgress(0.2, detail = "Daten werden verarbeitet")
        
        tryCatch({
          req(daten())
          req(clust_config$distance)
          req(clust_config$method)
          req(clust_config$normalisation)
          req(clust_config$palette)
          
          #calls the updated data
          data <- daten()
          cat("data dims:", nrow(data), ncol(data), "\n")
          
          cat("Your data first column sample:\n")
          print(head(data[, 1]))
          cat("Your data first column class:", class(data[, 1]), "\n")
          
          #filters rows by selected pathways
          selected_pathways <- input$pathways
          req(selected_pathways)
          req(length(selected_pathways) > 0)
          
          #------------------ PREPROCESS + INTEGRATION OF DATA -------------------
          
          preprocess <- preprocess_general(data)
          data_preprocessed <- preprocess$dataset_preprocessed
          cat("Preprocessed dims:", nrow(data_preprocessed), ncol(data_preprocessed), "\n")
          
          result <- run_data_integration(dataset = data_preprocessed,
                                         chosen_pathways = selected_pathways,
                                         con = con)
          
          gefilteterDatensatz <- result$filtered_dataset
          metaDaten_gefiltert <- result$meta_data
          cat("Filtered dims:", nrow(gefilteterDatensatz), ncol(gefilteterDatensatz), "\n")
          
          

          #------------------ PREPARE + NORMALIZE DATA ---------------------------
          #str(df_prepared[,1:3])
          
          norm_number <- switch(
            clust_config$normalisation,
            "Keine Normalisierung" = 0,
            "normalize_log_zscore" = 1,
            "normalize_zscore" = 2,
            "normalize_log_only" = 3,
            "normalize_log_median_centering" = 4,
            "normalize_median_centering" = 5,
            "normalize_log_mad" = 6,
            "normalize_mad" = 7,
            0
          )
          cat("norm_number:", norm_number, "\n")
          
          #---------------- PREPARED DATA ----------------------------------------
          df_prepared <- prepare_data(gefilteterDatensatz, clust_config$normalisation == "Keine Normalisierung")
          
          df_normalized <- normalization(df_prepared, norm_number)
          cat("normalisation OK, dims:", nrow(df_normalized), ncol(df_normalized), "\n")
          
          prepared_data(df_prepared)
          
          patient_names_vec <- colnames(result$meta_data)
          updateSelectizeInput(session, "focus_patient", choices = patient_names_vec, selected = character(0), server = TRUE)
          
          gene_names_vec <- result$gene_names
          
          label_row <- grep("lab", rownames(result$meta_data), ignore.case = TRUE, value = TRUE)[1]
          class_labels_vec <- if(!is.na(label_row)) as.character(result$meta_data[label_row, ]) else NULL
          cat("Class labels:", paste(unique(class_labels_vec), collapse = ", "), "\n")
          
          #--------------- DISTANCE + CLUSTERING ----------------------------------
          
          method <- switch(
            clust_config$distance,
            "Euklidische Distanz" = "euclidean",
            "Manhattan-Distanz" = "manhattan",
            "Minkowski-Distanz" = "minkowski",
            "Canberra-Distanz" = "canberra",
            "Pearson-Distanz" = "pearson",
            "Winkeldistanz (Angular Seperation)" = "angular"
          )
          
          cat("distance method String:", method, "\n")
          
          method_name <- switch (
            clust_config$method,
            "Single-Linkage" = "single",
            "Average-Linkage" = "average",
            "Complete-Linkage" = "complete",
            "Custom-Linkage" = "custom"
          )
          
          cat("cluster method string:", method_name, "\n")
          
          custom_params <- if (method_name == "custom") {
            list(
              alpha_a = clust_config$alpha_a,
              alpha_b = clust_config$alpha_b,
              beta = clust_config$beta,
              gamma = clust_config$gamma
            )
          } else
            NULL
          
          
          #---------------------DISTANCE MATRIX CACHE ------------------------
          
          minkowski_p_for_key <- if (identical(method, "minkowski")) {
            input$param_paramtab
          } else {
            NA
          }
          
          cache_key <- make_distance_cache_key(
            df_normalized = df_normalized,
            method = method,
            selected_pathways = selected_pathways,
            normalisation = clust_config$normalisation,
            minkowski_p = minkowski_p_for_key
          )
          
          if (
            !is.null(distance_cache$key) &&
            identical(distance_cache$key, cache_key) &&
            !is.null(distance_cache$patient) &&
            !is.null(distance_cache$gene)
          ) {
            
            
            
            dist_mat_pat <- distance_cache$patient
            dist_mat_genes <- distance_cache$gene
            
          } else {
            
          
            dist_mat_pat <- dist_cpp(t(df_normalized), method)
            dist_mat_genes <- dist_cpp(df_normalized, method)
            
            distance_cache$key <- cache_key
            distance_cache$patient <- dist_mat_pat
            distance_cache$gene <- dist_mat_genes
          }
          
          d_mat_result(list(
            patient = dist_mat_pat,
            gene = dist_mat_genes,
            key = cache_key
          ))
          
          incProgress(0.7, detail = "Daten werden Visualisiert")          
          
          #---------------------CLUSTERING ------------------------
          
          cluster_pat <- hierarchical_clustering(
            dist_mat_pat,
            method_name,
            custom_params = custom_params
          )
          
          cluster_pat$height <- cluster_pat$matched_at
          
          
          cluster_genes <- hierarchical_clustering(
            dist_mat_genes,
            method_name,
            custom_params = custom_params
          )
          
          cluster_genes$height <- cluster_genes$matched_at
          
          #---------------------DENDROGRAM PREP ----------------------------------
          
          tree_pat <- build_tree(cluster_pat)
          order_pat <- get_order_vector(tree_pat)
          
          tree_genes <- build_tree(cluster_genes)
          order_genes <- get_order_vector(tree_genes)

          #--------------------BUILD PLOTS ---------------------------------------
          
          dendro_data_pat <- generate_dendro_data(
            cluster_result = cluster_pat,
            tree_result = tree_pat,
            order_vector = order_pat,
            class_labels = class_labels_vec
          )
          
          dendro_data_genes <- generate_dendro_data(
            cluster_result = cluster_genes,
            tree_result = tree_genes,
            order_vector = order_genes,
            class_labels = NULL
          )
          
          patient_dendro <- plot_dendro_plotly(
            dendro_data = dendro_data_pat,
            side = "top",
            names_vector = patient_names_vec,
            palette_name = clust_config$palette,
            show_legend = TRUE,
            show_x_axis = TRUE,
            show_y_axis = TRUE
          ) %>%
            layout(title = paste("Patient Dendrogram: ", dataset_name()),
                   title = list(x=0.5, font = list(size=20)))
          
          gene_dendro <- plot_dendro_plotly(
            dendro_data = dendro_data_genes,
            side = "top",
            names_vector = gene_names_vec,
            palette_name = NULL,
            show_legend = FALSE,
            show_x_axis = TRUE,
            show_y_axis = TRUE
          ) %>%
            layout(title = paste("Gene Dendrogram: ", dataset_name()),
                   title = list(x=0.5, font = list(size=20)))
          
          final_plot <- grafikpanel(
            gene_dendro_data = dendro_data_genes,
            patient_dendro_data = dendro_data_pat,
            gene_order = order_genes,
            patient_order = order_pat,
            data_matrix = df_normalized,
            metaDaten_gefiltert = result$meta_data,
            gene_names = gene_names_vec,
            patient_names = patient_names_vec,
            palette_name = clust_config$palette
          ) %>%
            layout(title = paste("Grafikpanel: ", dataset_name()),
                   title = list(x=0.5, font = list(size=20)))
          
          system.time({
            heatmap_store(final_plot)
          })
          patient_store(patient_dendro)
          gene_store(gene_dendro)
          
          cluster_patient(cluster_pat)
          tree_patient(tree_pat)
          order_patient(order_pat)
          patient_names(patient_names_vec)
          class_labels(class_labels_vec)
          
          cluster_gene(cluster_genes)
          tree_gene(tree_genes)
          order_gene(order_genes)
          gene_names(gene_names_vec)
          
          cat("Before tab switch\n")
          incProgress(0.8, detail = "Visualisierung wird geladen...")
          
          updateTabItems(session, "tabs", selected = "heatmap")
          cat("After switch\n")
          
          incProgress(1, detail = "Fertig")
          

        }, error = function(e) {
          cat("\n=== ERROR after step above ===\n")
          cat("Message:", conditionMessage(e), "\n")
          print(traceback())
          cat("====================\n")
        })
      }
    )
      
    
  }
  
  
  
  observeEvent(input$run, {
    req(inputs_valid())
    
    if (clust_config$distance == "Minkowski-Distanz" &&
        input$param_paramtab == 1 &&
        !skip_mink1()) {
      
      current_warn("p1")
      
      showModal(
        modalDialog(
          title = "Warnung",
          "hier wird mit Manhattan-Distanz statt Minkowski-Distanz berechnet. Möchten Sie fortfahren?",
          
          checkboxInput("dont_show1", "Diese Meldung nicht mehr zeigen", value = FALSE),
          
          footer = tagList(
            modalButton("Abbrechen"),
            
            actionButton("confirm_run", "Ja")
          )
        )
      )
    } else if (clust_config$distance == "Minkowski-Distanz" &&
              input$param_paramtab == 2 &&
              !skip_mink2()) {
      
      current_warn("p2")
      
      showModal(
        modalDialog(
          title = "Warnung",
          "hier wird mit Euklidische Distanz statt Minkowski-Distanz berechnet. Möchten Sie fortfahren?",
          
          checkboxInput("dont_show2", "Diese Meldung nicht mehr zeigen", value = FALSE),
          
          footer = tagList(modalButton("Abbrechen"), actionButton("confirm_run", "Ja"))
        )
      )
    } else{
      run_analysis()
    }
  })
  
  observeEvent(input$refreshButton, {
    req(inputs_valid())
    
    if (clust_config$distance == "Minkowski-Distanz" &&
        input$param_heatmap == 1 &&
        !skip_mink1()) {
      
      current_warn("p1")
      
      showModal(
        modalDialog(
          title = "Warnung",
          "hier wird mit Manhattan-Distanz statt Minkowski-Distanz berechnet. Möchten Sie fortfahren?",
          
          checkboxInput("dont_show1", "Diese Meldung nicht mehr zeigen", value = FALSE),
          
          footer = tagList(
            modalButton("Abbrechen"),
            
            actionButton("confirm_run", "Ja")
          )
        )
      )
    } else if (clust_config$distance == "Minkowski-Distanz" &&
               input$param_heatmap == 2 &&
               !skip_mink2()) {
      
      current_warn("p2")
      
      showModal(
        modalDialog(
          title = "Warnung",
          "hier wird mit Euklidische Distanz statt Minkowski-Distanz berechnet. Möchten Sie fortfahren?",
          
          checkboxInput("dont_show2", "Diese Meldung nicht mehr zeigen", value = FALSE),
          
          footer = tagList(modalButton("Abbrechen"), actionButton("confirm_run", "Ja"))
        )
      )
    } else{
      run_analysis()
    }
  })
  
  
  observeEvent(input$save_preset, {
    
    req(input$preset_name)
    
    if (!dir.exists(preset_dir)) {
      dir.create(preset_dir, recursive = TRUE)
    }
    
    preset_name_clean <- gsub("[^A-Za-z0-9_\\-]", "_", input$preset_name)
    pfad <- file.path(preset_dir, paste0(preset_name_clean, ".json"))
    
    preset <- list(
      anzahlcluster = input$anzahlcluster,
      clusterverfahren = clust_config$method,
      normalisierung = clust_config$normalisation,
      distanzmatrix = clust_config$distance,
      farbpaletten = clust_config$palette,
      alpha_a = clust_config$alpha_a,
      alpha_b = clust_config$alpha_b,
      beta = clust_config$beta,
      gamma = clust_config$gamma,
      minkowski_p = clust_config$minkowski_p,
      pathways = input$pathways
    )
    
    jsonlite::write_json(
      preset,
      path = pfad,
      auto_unbox = TRUE,
      pretty = TRUE
    )
    
    showNotification(
      paste("Preset gespeichert unter:", pfad),
      type = "message"
    )
    
    refresh_presets(session)
  })
  
  
  
  observe({
    if (input$distanzmatrix != "Minkowski-Distanz") {
      shinyFeedback::hideFeedback("param_paramtab")
      return()
    }
    
    val <- input$param_paramtab
    msg <- NULL
    
    #error message: p has to be a number
    if (is.null(val) ||
        is.na(val)) {
      #error message: p has to be a number
      msg <- "Bitte eine Zahl eingeben"
    } else if (val <= 0) {
      #if p<0, error msg: p has to be greater than 0
      msg <- "Falsche eingabe: bitte ein Zahl größer als 0 eingeben"
    } else if (val > 10000) {
      msg <- "Maximale eingabe Zahl ist 10000"
    } else if (val %% 1 != 0) {
      msg <- "Falsche eingabe: bitte ein Integer eingeben"
    }
    shinyFeedback::feedbackDanger("param_paramtab", !is.null(msg), msg)
    
  })
  
  observe({
    if (input$distanzmatrix_sidebar != "Minkowski-Distanz") {
      shinyFeedback::hideFeedback("param_heatmap")
      return()
    }
    
    val <- input$param_heatmap
    msg <- NULL
    
    #error message: p has to be a number
    if (is.null(val) ||
        is.na(val)) {
      #error message: p has to be a number
      msg <- "Bitte eine Zahl eingeben"
    } else if (val <= 0) {
      #if p<0, error msg: p has to be greater than 0
      msg <- "Falsche eingabe: bitte ein Zahl größer als 0 eingeben"
    } else if (val > 10000) {
      msg <- "Maximale eingabe Zahl ist 10000"
    } else if (val %% 1 != 0) {
      msg <- "Falsche eingabe: bitte ein Integer eingeben"
    }
    shinyFeedback::feedbackDanger("param_heatmap", !is.null(msg), msg)
  })
  
  observeEvent(input$back, {
    updateTabItems(session, "tabs", selected = "parameter")
  })
  
  observeEvent(input$back2upload, {
    updateTabItems(session, "tabs", selected = "datei_hochladen")
  })
  
  con <- dbConnect(RSQLite::SQLite(), "GeneDatabase.sqlite")
  
  observe({
    req(con)
    pw <- get_pathwaynames_from_database(con)
    pathway_list(pw)
  })
  

  observe({
    req(pathway_list())
    
    updateSelectizeInput(session, "pathways", choices = pathway_list(), server = TRUE)
  })
  

  observeEvent(input$confirm_button, {
    if(is.null(input$pathways) || length(input$pathways) == 0) {
      showNotification(
        ui = div(style = "font-size: 18px; font-weight: bold; color: #000000;", "Bitte mindestens eine Pathway auswählen!"),
        type = "error")
      return()
    }
    selected_pathways <- input$pathways
    print(selected_pathways)
  })
  
  observeEvent(input$confirm_run, {
    
    if(current_warn() == "p1" &&
      isTRUE(input$dont_show1)){
      
      skip_mink1(TRUE)
    }
    
    if(current_warn() == "p2" &&
      isTRUE(input$dont_show2)){
      
      skip_mink2(TRUE)
    }
    
    removeModal()
    
    run_analysis()
  })
  
  inputs_valid <- reactive({
    req(clust_config$method)
    req(clust_config$normalisation)
    req(clust_config$distance)
    req(clust_config$palette)
    
    mink_valid <- TRUE
    
    if (input$distanzmatrix == "Minkowski-Distanz") {
      mink_valid <- !is.null(input$param_paramtab) &&
        !is.na(input$param_paramtab) &&
        input$param_paramtab > 0 &&
        input$param_paramtab <= 10000 &&
        input$param_paramtab == as.integer(input$param_paramtab)
    }
    
    TRUE && mink_valid
  })
  
  observe({
    if (isTRUE(inputs_valid())) {
      shinyjs::enable("run")
    } else{
      shinyjs::disable("run")
    }
  })
  
  #-------------------------CALLING ANALYSE PATHWAY COVERAGE--------------------
  output$coverage_table <- renderTable({
    req(coverage_result())
    coverage_result()
    
  }, rownames = TRUE, digits = 0)
  
  
 
  observeEvent(input$confirm_button, {
    
    req(input$pathways)
    req(daten())
    
    coverage <- analyze_pathways_coverage(
      chosen_pathways = input$pathways,
      dataset_cleaned = daten(),
      con = con
    )
    
    coverage_result(coverage$matrix_unused)
    
    if(skip_pathways()){
      updateTabItems(session, "tabs", selected = "parameter")
      return()
    
    }
    
    showModal(
      modalDialog(
        title = "Warnung!",
        
        tableOutput("coverage_table"),
        
        "Möchten Sie mit dem angegebenen Pathways fortfahren?",
        
        checkboxInput("dont_showBox", "Diese Meldung nicht mehr zeigen", value = FALSE),
        
        footer = tagList(
          modalButton("Andere Pathways auswählen"),
          
          actionButton("continue_analysis", "Ja")
        )
      )
    )
  })
  
  observeEvent(input$continue_analysis, {
    
    if(isTRUE(input$dont_showBox)){
      skip_pathways(TRUE)
    }
    removeModal()
    
    updateTabItems(session, "tabs", selected = "parameter")
  })
  
  
  #---------------VISUALISATION-------------------------
  
  output$patientDendrogram <- renderPlotly({
    req(patient_store())
    
    patient_store()
  })
  
  output$geneDendrogram <- renderPlotly({
    req(gene_store())
    
    gene_store()
  })

  
  highlighted_heatmap <- reactive({
    req(heatmap_store())
    
    plot <- heatmap_store()
    
    if(is.null(selected_patient())){
      return(plot)
    }
    
    req(order_gene())
    req(patient_names())
    req(order_patient())
    
    patient <- selected_patient()
    
    patient_index <- which(patient_names()[order_patient()] == patient)
    
    if(length(patient_index)==0){
      return(plot)
    }
    
    n_genes <- length(order_gene())
    
    plot <- layout(
      plot,
      shapes = list(
        list(type = "rect", xref = "x", yref = "y", x0 = patient_index-0.5, x1 = patient_index+0.5,
             y0=0.5, y1 = n_genes+0.5, line = list(color= "black",width=1.5),
             fillcolor = "rgba(0,0,0,0)")
      )
    )
  })
  
  output$grafikpanel <- renderPlotly({
    highlighted_heatmap()
    
  })
  
  
}
