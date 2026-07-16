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
  error_message <- reactiveVal(NULL)

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
    cluster_bundle(NULL)
    
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
          
          cleaned <- User_handle_na_decision(df = cleaned$df,info = cleaned$info,action = "mean")
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
  
  observe({
    if (is.null(cluster_bundle())) {
      shinyjs::hide("download_pdf")
    } else {
      shinyjs::show("download_pdf")
    }
  })
  
  
  output$error_output <- renderUI({
    
    msg <- error_message()
    
    if (is.null(msg)) {
      return(NULL)
    }
    
    div(
      style = "
      background-color: #F8D7DA;
      color: #721C24;
      border: 1px solid #F5C6CB;
      padding: 12px;
      margin-bottom: 15px;
      border-radius: 4px;
      font-weight: bold;
    ",
      icon("triangle-exclamation"),
      " Fehler: ",
      msg
    )
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
      
      actionButton(inputId = "na_replace_mean",label = "50%-Zeilen/Spalten behalten und NA durch Mittelwert ersetzen",class = "na-mean-button"),
      
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
    
    withProgress(message = "NA-Werte werden verarbeitet...",value = 0,
      {
        
        incProgress(
          0.2,
          detail = "Zeilenmittelwerte werden berechnet"
        )
        
        result <- User_handle_na_decision(
          df = daten_aktuell(),
          info = na_infos(),
          action = "mean"
        )
        
        incProgress(
          0.8,
          detail = "Bereinigter Datensatz wird gespeichert"
        )
        
        daten_aktuell(result$df)
        na_infos(result$info)
        
        distance_cache$key <- NULL
        distance_cache$patient <- NULL
        distance_cache$gene <- NULL
        
        shinyjs::enable("confirm_button")
        
        incProgress(
          1,
          detail = "Fertig"
        )
      }
    )
    
    showNotification(
      paste(
        result$info$imputed_values,
        "NA-Werte wurden durch Zeilenmittelwerte ersetzt."
      ),
      type = "message",
      duration = 5
    )
  })
  
  observeEvent(input$na_drop_and_replace, {
    
    req(daten_aktuell())
    req(na_infos())
    
    withProgress(
      message = "NA-Werte werden verarbeitet...",
      value = 0,
      {
        
        incProgress(
          0.2,
          detail = "Zeilen und Spalten mit vielen NA-Werten werden entfernt"
        )
        
        result <- User_handle_na_decision(
          df = daten_aktuell(),
          info = na_infos(),
          action = "drop"
        )
        
        incProgress(
          0.8,
          detail = "Restliche NA-Werte werden ersetzt"
        )
        
        daten_aktuell(result$df)
        na_infos(result$info)
        
        distance_cache$key <- NULL
        distance_cache$patient <- NULL
        distance_cache$gene <- NULL
        
        shinyjs::enable("confirm_button")
        
        incProgress(
          1,
          detail = "Fertig"
        )
      }
    )
    
    showNotification(
      paste0(
        length(result$info$removed_50_rows),
        " Zeilen und ",
        length(result$info$removed_50_cols),
        " Spalten entfernt. ",
        result$info$imputed_values,
        " NA-Werte wurden ersetzt."
      ),
      type = "message",
      duration = 6
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
  
  produced_pdfs <- "app/produced_pdfs"
  
  if (!dir.exists(produced_pdfs)) {
    dir.create(produced_pdfs, recursive = TRUE)
  }
  
  watched_pdf <- reactivePoll(intervalMillis = 2000, session = session, 
                              checkFunc = function () pdf_check(produced_pdfs), valueFunc = function() pdf_value(produced_pdfs))
  
  output$download_pdf <- downloadHandler(
    filename = function() {paste0("ClusterIt_Report_",format(Sys.time(), "%Y-%m-%d_%H-%M-%S"),".pdf")
    },
    
    contentType = "application/pdf",
    
    content = function(file) {
      
      bundle <- cluster_bundle()
      
      if (is.null(bundle)) {
        stop("Bitte zuerst eine Analyse durchführen.")
      }
      
      report_config <- bundle$settings
      report_config$palette <- bundle$palette
      
      pdf_content(
        file = file,
        watched_pdf = watched_pdf,
        daten_aktuell = daten_aktuell,
        dataset_name = dataset_name(),
        report_config = report_config
      )
    }
  )

  # save settings paramters
  observeEvent(input$clusterverfahren, {
    clust_config$method <- input$clusterverfahren
  }, ignoreInit = TRUE)
  
  observeEvent(input$normalisierung, {
    clust_config$normalisation <- input$normalisierung
  }, ignoreInit = TRUE)
  
  observeEvent(input$distanzmatrix, {
    clust_config$distance <- input$distanzmatrix
  }, ignoreInit = TRUE)
  
  observeEvent(input$farbpaletten, {
    clust_config$palette <- input$farbpaletten
  }, ignoreInit = TRUE)
  
  observeEvent(input$alpha_a, {
    clust_config$alpha_a <- input$alpha_a
  }, ignoreInit = TRUE)
  
  observeEvent(input$alpha_b, {
    clust_config$alpha_b <- input$alpha_b
  }, ignoreInit = TRUE)
  
  observeEvent(input$beta, {
    clust_config$beta <- input$beta
  }, ignoreInit = TRUE)
  
  observeEvent(input$gamma, {
    clust_config$gamma <- input$gamma
  }, ignoreInit = TRUE)
  
  observeEvent(input$param_paramtab, {
    clust_config$minkowski_p <- input$param_paramtab
  }, ignoreInit = TRUE)
  
  observeEvent(input$clusterverfahren_sidebar, {
    clust_config$method <- input$clusterverfahren_sidebar
  }, ignoreInit = TRUE)
  
  observeEvent(input$normalisierung_sidebar, {
    clust_config$normalisation <- input$normalisierung_sidebar
  }, ignoreInit = TRUE)
  
  observeEvent(input$distanzmatrix_sidebar, {
    clust_config$distance <- input$distanzmatrix_sidebar
  }, ignoreInit = TRUE)
  
  observeEvent(input$farbpaletten_sidebar, {
    clust_config$palette <- input$farbpaletten_sidebar
  }, ignoreInit = TRUE)
  
  observeEvent(input$param_heatmap, {
    clust_config$minkowski_p <- input$param_heatmap
  }, ignoreInit = TRUE)

  
  observeEvent(input$nextpage, {
    updateTabItems(session, "tabs", selected = "datei_hochladen")
  })
  
  
  
  observeEvent(input$load_preset, {
    
    req(input$preset_datei)
    
    preset <- read_preset_file(input$preset_datei)
    
    # Loading preset
    clust_config$method <- preset$clusterverfahren
    clust_config$normalisation <- preset$normalisierung
    clust_config$distance <- preset$distanzmatrix
    clust_config$palette <- preset$farbpaletten
    
    clust_config$alpha_a <- preset$alpha_a
    clust_config$alpha_b <- preset$alpha_b
    clust_config$beta <- preset$beta
    clust_config$gamma <- preset$gamma
    
    clust_config$minkowski_p <- preset$minkowski_p
    
    
    updateSelectInput(session,"clusterverfahren",selected = preset$clusterverfahren
    )
    
    updateSelectInput(session,"normalisierung",selected = preset$normalisierung
    )
    
    updateSelectInput(
      session,
      "distanzmatrix",
      selected = preset$distanzmatrix
    )
    
    updateRadioButtons(
      session,
      "farbpaletten",
      selected = preset$farbpaletten
    )
    
    updateNumericInput(session,"alpha_a",value = preset$alpha_a
    )
    
    updateNumericInput(session,"alpha_b",value = preset$alpha_b
    )
    
    updateNumericInput(session,"beta",value = preset$beta
    )
    
    updateNumericInput(session,"gamma",value = preset$gamma)
    
    updateNumericInput(session,"param_paramtab",value = preset$minkowski_p)
    
    # Update sidebar
    updateSelectInput(session,"clusterverfahren_sidebar",selected = preset$clusterverfahren)
    
    updateSelectInput(session,"normalisierung_sidebar",selected = preset$normalisierung)
    
    updateSelectInput(session,"distanzmatrix_sidebar",selected = preset$distanzmatrix)
    
    updateRadioButtons(session,"farbpaletten_sidebar",selected = preset$farbpaletten)
    
    updateNumericInput(session,"param_heatmap",value = preset$minkowski_p)
    
    
    req(pathway_list())
    
    updateSelectizeInput(session,"pathways",choices = pathway_list(),selected = preset$pathways,server = TRUE)
    
    #Delete old cache
    distance_cache$key <- NULL
    distance_cache$patient <- NULL
    distance_cache$gene <- NULL
    
    showNotification(
      paste(
        "Preset geladen:",
        basename(input$preset_datei)
      ),
      type = "message"
    )
  })
  
  output$customInfo <- renderUI({
    if (clust_config$method == "Custom-Linkage") {
      div(
        style = paste(
          "background-color: #f8f9fa;",
          "border-left: 4px solid #007bff;",
          "padding: 8px 12px;",
          "margin-top: 5px;",
          "font-size: 14px;"
        ),
        icon("info-circle"),
        tags$b("Hinweis: "),
        "Nicht alle Werte sind sinnvoll. Bitte geben Sie nur geeignete Werte ein."
      )
    }
  })
  observeEvent(
    input$focus_patient,
    {
      if (
        is.null(input$focus_patient) ||
        input$focus_patient == ""
      ) {
        selected_patient(NULL)
      } else {
        selected_patient(input$focus_patient)
      }
    },
    ignoreNULL = FALSE
  )
  
  
  
  run_analysis <- function() {
    cat("Analysis started\n")
    error_message(NULL)
    withProgress(
      message = "Analyse gestartet...",
      value = 0,
      {
        incProgress(0.4, detail = "Daten werden verarbeitet")
        
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
            clust_config$minkowski_p
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
            
            if (identical(method, "minkowski")) {
              
              dist_mat_pat <- dist_cpp(
                t(df_normalized),
                method = method,
                p = clust_config$minkowski_p
              )
              
              dist_mat_genes <- dist_cpp(
                df_normalized,
                method = method,
                p = clust_config$minkowski_p
              )
              
            } else {
              
              dist_mat_pat <- dist_cpp(
                t(df_normalized),
                method = method
              )
              
              dist_mat_genes <- dist_cpp(
                df_normalized,
                method = method
              )
            }
            
            
            distance_cache$key <- cache_key
            distance_cache$patient <- dist_mat_pat
            distance_cache$gene <- dist_mat_genes
          }
          
          d_mat_result(list(
            patient = dist_mat_pat,
            gene = dist_mat_genes,
            key = cache_key
          ))
          
          incProgress(0.65, detail = "Daten werden Visualisiert")          
          
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
          
          cluster_bundle(list(
            dendro_data_pat = dendro_data_pat,
            dendro_data_genes = dendro_data_genes,
            order_pat = order_pat,
            order_genes = order_genes,
            df_normalized = df_normalized,
            meta_data = result$meta_data,
            gene_names = gene_names_vec,
            patient_names = patient_names_vec,
            palette = clust_config$palette,
            
            settings = list(
              method = clust_config$method,
              normalisation = clust_config$normalisation,
              distance = clust_config$distance,
              alpha_a = clust_config$alpha_a,
              alpha_b = clust_config$alpha_b,
              beta = clust_config$beta,
              gamma = clust_config$gamma,
              minkowski_p = clust_config$minkowski_p,
              pathways = sort(input$pathways)
            )
          ))
          
          
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
          
          # Delte old analysis
          old_plot_pdfs <- list.files(
            path = produced_pdfs,
            pattern = "^(01_patient_dendrogram|02_gene_dendrogram|03_heatmap)\\.pdf$",
            full.names = TRUE
          )
          
          if (length(old_plot_pdfs) > 0) {
            unlink(old_plot_pdfs)
          }
          
          
          patient_dendro_pdf_plot <- plot_dendro_ggplot(
            dendro_data = dendro_data_pat,
            title = paste(
              "Patient Dendrogram:",
              dataset_name()
            ),
            names_vector = patient_names_vec,
            palette_name = clust_config$palette,
            show_legend = TRUE,
            show_x_axis = TRUE,
            show_y_axis = TRUE
          )
          
          # Gen-Dendrogramm als ggplot erzeugen
          gene_dendro_pdf_plot <- plot_dendro_ggplot(
            dendro_data = dendro_data_genes,
            title = paste(
              "Gene Dendrogram:",
              dataset_name()
            ),
            names_vector = gene_names_vec,
            palette_name = NULL,
            show_legend = FALSE,
            show_x_axis = TRUE,
            show_y_axis = TRUE
          )
          
          
          save_dendro_pdf(plot = patient_dendro_pdf_plot,dateiname = "01_patient_dendrogram.pdf",pfad = produced_pdfs)
          
          
          save_dendro_pdf(plot = gene_dendro_pdf_plot,dateiname = "02_gene_dendrogram.pdf",pfad = produced_pdfs)
          
          
          heatmap_pdf(df_normalized = df_normalized,patient_order = order_pat,gene_order = order_genes,
            
            gene_names = as.character(
              unlist(
                gene_names_vec,
                use.names = FALSE
              )
            ),
            
            file = file.path(
              produced_pdfs,
              "03_heatmap.pdf"
            ),
            pages = 1,
            width = 40,
            height = 55,
            show_x_axis = TRUE
          )
          
          
          
          
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
          
          msg <- conditionMessage(e)
          
          cat("\n=== ERROR after step above ===\n")
          cat("Message:", msg, "\n")
          print(traceback())
          cat("====================\n")
          
          error_message(msg)
          
         
          output$analysis_status <- renderUI(NULL)
          
          
          showNotification(
            ui = div(
              style = "
      font-size: 20px;
      font-weight: bold;
      line-height: 1.4;
    ",
              paste("Fehler in der Analyse:", msg)
            ),
            type = "error",
            duration = 8
          )
          
        })
      }
    )
      
    
  }
  
  
  
  observeEvent(input$run, {
    req(inputs_valid())
    
    if (
      clust_config$distance == "Minkowski-Distanz" &&
      clust_config$minkowski_p == 1 &&
      !skip_mink1()
    ) {
      
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
    } else if (
      clust_config$distance == "Minkowski-Distanz" &&
      clust_config$minkowski_p == 2 &&
      !skip_mink2()
    ) {
      
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
  
  refresh_plots <- function() {
    
    bundle <- cluster_bundle()
    req(bundle)
    
    withProgress(
      message = "Grafik wird aktualisiert...",
      value = 0,
      {
        
        incProgress(
          0.2,
          detail = "Patienten-Dendrogramm wird erstellt"
        )
        
        patient_dendro <- plot_dendro_plotly(dendro_data = bundle$dendro_data_pat,side = "top",names_vector = bundle$patient_names,palette_name = clust_config$palette,
          show_legend = TRUE,
          show_x_axis = TRUE,
          show_y_axis = TRUE
        )%>%
          layout(title = paste("Patient Dendrogram: ", dataset_name()),
                 title = list(x=0.5, font = list(size=20)))
        
        
        incProgress(
          0.4,
          detail = "Gen-Dendrogramm wird erstellt"
        )
        
        gene_dendro <- plot_dendro_plotly(dendro_data = bundle$dendro_data_genes,side = "top",names_vector = bundle$gene_names,palette_name = NULL,
          show_legend = FALSE,
          show_x_axis = TRUE,
          show_y_axis = TRUE
        ) %>%
          layout(title = paste("Gene Dendrogram: ", dataset_name()),
                 title = list(x=0.5, font = list(size=20)))
        
        
        incProgress(
          0.7,
          detail = "Heatmap wird aktualisiert"
        )
        
        final_plot <- grafikpanel(gene_dendro_data = bundle$dendro_data_genes,patient_dendro_data = bundle$dendro_data_pat,gene_order = bundle$order_genes,patient_order = bundle$order_pat,data_matrix = bundle$df_normalized,metaDaten_gefiltert = bundle$meta_data,gene_names = bundle$gene_names,patient_names = bundle$patient_names,palette_name = clust_config$palette
        ) %>%
          layout(title = paste("Grafikpanel: ", dataset_name()),
                 title = list(x=0.5, font = list(size=20)))
        
        
        incProgress(
          0.9,
          detail = "Grafiken werden angezeigt"
        )
        
        patient_store(patient_dendro)
        gene_store(gene_dendro)
        heatmap_store(final_plot)
        bundle$palette <- clust_config$palette
        cluster_bundle(bundle)
        
        incProgress(
          1,
          detail = "Fertig"
        )
      }
    )
  }
  
  
  observeEvent(input$refreshButton, {
    
    req(inputs_valid())
    
    bundle <- cluster_bundle()
    
    current_settings <- list(
      method = clust_config$method,
      normalisation = clust_config$normalisation,
      distance = clust_config$distance,
      alpha_a = clust_config$alpha_a,
      alpha_b = clust_config$alpha_b,
      beta = clust_config$beta,
      gamma = clust_config$gamma,
      minkowski_p = clust_config$minkowski_p,
      pathways = sort(input$pathways)
    )
    
    # Nur die Farbpalette wurde geändert:
    # keine neue Analyse, sondern nur Grafiken aktualisieren
    if (
      !is.null(bundle) &&
      identical(bundle$settings, current_settings)
    ) {
      
      cat("Nur Grafik wird aktualisiert\n")
      
      refresh_plots()
      
      return()
    }
    
    # Andere Einstellungen wurden geändert:
    # Analyse mit den bisherigen Minkowski-Warnungen starten
    if (
      clust_config$distance == "Minkowski-Distanz" &&
      clust_config$minkowski_p == 1 &&
      !skip_mink1()
    ) {
      
      current_warn("p1")
      
      showModal(
        modalDialog(
          title = "Warnung",
          
          "hier wird mit Manhattan-Distanz statt Minkowski-Distanz berechnet. Möchten Sie fortfahren?",
          
          checkboxInput(
            "dont_show1",
            "Diese Meldung nicht mehr zeigen",
            value = FALSE
          ),
          
          footer = tagList(
            modalButton("Abbrechen"),
            actionButton("confirm_run", "Ja")
          )
        )
      )
      
    } else if (
      clust_config$distance == "Minkowski-Distanz" &&
      clust_config$minkowski_p == 2 &&
      !skip_mink2()
    ) {
      
      current_warn("p2")
      
      showModal(
        modalDialog(
          title = "Warnung",
          
          "hier wird mit Euklidische Distanz statt Minkowski-Distanz berechnet. Möchten Sie fortfahren?",
          
          checkboxInput(
            "dont_show2",
            "Diese Meldung nicht mehr zeigen",
            value = FALSE
          ),
          
          footer = tagList(
            modalButton("Abbrechen"),
            actionButton("confirm_run", "Ja")
          )
        )
      )
      
    } else {
      
      cat("Analyse wird aktualisiert\n")
      
      run_analysis()
    }
  })
  
  
  
  observeEvent(input$save_preset, {
    preset_name <- trimws(input$preset_name)
    
    if (!nzchar(preset_name)) {
      showNotification(
        "Bitte einen Namen für das Preset eingeben.",
        type = "error"
      )
      return()
    }
    
    selected_pathways <- if (is.null(input$pathways)) {
      character(0)
    } else {
      as.character(input$pathways)
    }
    
    preset <- list(
      preset_version = 1L,
      saved_at = format(
        Sys.time(),
        "%Y-%m-%d %H:%M:%S"
      ),
      
      clusterverfahren = clust_config$method,
      normalisierung = clust_config$normalisation,
      distanzmatrix = clust_config$distance,
      farbpaletten = clust_config$palette,
      
      alpha_a = clust_config$alpha_a,
      alpha_b = clust_config$alpha_b,
      beta = clust_config$beta,
      gamma = clust_config$gamma,
      
      minkowski_p = clust_config$minkowski_p,
      pathways = selected_pathways
    )
    
    tryCatch(
      {
        preset_path <- write_preset_file(
          preset = preset,
          preset_name = preset_name,
          preset_dir = preset_dir
        )
        
        refresh_presets(
          session = session,
          preset_dir = preset_dir,
          selected = preset_path
        )
        
        updateTextInput(
          session,
          "preset_name",
          value = ""
        )
        
        showNotification(
          paste(
            "Preset gespeichert:",
            basename(preset_path)
          ),
          type = "message"
        )
      },
      error = function(e) {
        showNotification(
          conditionMessage(e),
          type = "error",
          duration = NULL
        )
      }
    )
  })
  
  observe({
    updateSelectInput(
      session,
      "clusterverfahren",
      selected = clust_config$method
    )
    
    updateSelectInput(
      session,
      "clusterverfahren_sidebar",
      selected = clust_config$method
    )
    
    updateSelectInput(
      session,
      "normalisierung",
      selected = clust_config$normalisation
    )
    
    updateSelectInput(
      session,
      "normalisierung_sidebar",
      selected = clust_config$normalisation
    )
    
    updateSelectInput(
      session,
      "distanzmatrix",
      selected = clust_config$distance
    )
    
    updateSelectInput(
      session,
      "distanzmatrix_sidebar",
      selected = clust_config$distance
    )
    
    updateRadioButtons(
      session,
      "farbpaletten",
      selected = clust_config$palette
    )
    
    updateRadioButtons(
      session,
      "farbpaletten_sidebar",
      selected = clust_config$palette
    )
    
    updateNumericInput(
      session,
      "param_paramtab",
      value = clust_config$minkowski_p
    )
    
    updateNumericInput(
      session,
      "param_heatmap",
      value = clust_config$minkowski_p
    )
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
    
    if (clust_config$distance != "Minkowski-Distanz") {
      return(TRUE)
    }
    
    p <- clust_config$minkowski_p
    
    !is.null(p) &&
      !is.na(p) &&
      p > 0 &&
      p <= 10000
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
    
    
    if (
      is.null(input$pathways) ||
      length(input$pathways) == 0
    ) {
      showNotification(
        ui = div(
          style = paste(
            "font-size: 18px;",
            "font-weight: bold;",
            "color: #000000;"
          ),
          "Bitte mindestens einen Pathway auswählen!"
        ),
        type = "error"
      )
      
      return()
    }
    
    
    req(daten())
    
    
    coverage <- analyze_pathways_coverage(
      chosen_pathways = input$pathways,
      dataset_cleaned = daten(),
      con = con
    )
    
    #
    coverage_result(coverage$matrix_unused)
    
   
    if (skip_pathways()) {
      updateTabItems(session,"tabs",selected = "parameter"
      )
      
      return()
    }
    
   
    showModal(
      modalDialog(
        title = "Warnung!",
        
        tableOutput("coverage_table"),
        
        "Möchten Sie mit den angegebenen Pathways fortfahren?",
        
        checkboxInput(inputId = "dont_showBox",label = "Diese Meldung nicht mehr zeigen",value = FALSE
        ),
        
        footer = tagList(
          modalButton("Andere Pathways auswählen"),
          
          actionButton(inputId = "continue_analysis",label = "Ja"
          )
        )
      )
    )
  })
  
  
  observeEvent(input$continue_analysis, {
    
    if (isTRUE(input$dont_showBox)) {
      skip_pathways(TRUE)
    }
    
    removeModal()
    
    updateTabItems(
      session,
      "tabs",
      selected = "parameter"
    )
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
