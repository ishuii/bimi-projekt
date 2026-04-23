
source("R/clustering/single_linkage.R")
source("R/clustering/distance_matrix.R")

cluster_methods <- list("Single-Linkage" = single_linkage,
                        "Average-Linkage" = single_linkage,
                        "Complete-Linkage"= single_linkage)

distance_methods <- list("Euklidische distanz" = function(data) dist_cpp(data, method = "euklidische"),
                         "Manhattan distanz" = function(data) dist_cpp(data, method = "euklidische"))


observeEvent(input$run, {
  
  #calls the updated data
  data <- daten()
  
  #reads user's choice input- if single linkage is chosen, it stores that choice
  cluster_fun <- cluster_methods[[input$clusterverfahren]]
  dist_fun <- distance_methods[[input$distanzmatrix]]
  
  #computes the distance matrix based on user's choice and stores in d_mat
  d_mat <- dist_fun(data)
  
  #runs the cluster analysis using the chosen method and the stored distance matrix
  result <- cluster_fun(d_mat)
  
  print("Cluster analysis successful")
  print(result)
  
  updateTabItems(session, "tabs", selected = "heatmap")
})