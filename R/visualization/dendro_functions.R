calculate_coords <- function(order, height, tree){
  
  if(is.null(tree)) return(NULL)
  ###1. Positionen berechnen x und y
  
  ##1. Fall es handelt sich um einen Knoten
  if(!is.null(tree$id)){
    
    #x Position = order vektor Position
    x = which(tree$id == order)
    
    #y Position = 0
    y = 0
      
    return(list(x=x,y=y))
  }
  
  ##2. Fall: Cluster
  else {
    
    #x Position: Berechnet sich aus dem Mittelwert der beiden Kinder -> rekursive Berechnung
    left_coord= calculate_coords(order, height, tree$left)
    right_coord= calculate_coords(order, height, tree$right)
    x = mean(c(left_coord$x,right_coord$x))
    
    #y Position: Höhenvektor
    y = tree$height
    
    return(list(x=x,y=y))
  }
}

draw_segments <- function(coords, tree, labels, height, order, names){
  
  #1. Basecase => tree = 0
  if(is.null(tree)) return(NULL)
  
  #2. Basecase => Blatt = tree$id not NULL => return NULL (nichts soll gezeichnet werden)
  if(!is.null(tree$id)){
    text(x = which(tree$id == order), 
         y = -0.8, 
         labels = names[tree$id],
         srt = 90)   # 90° Rotation falls die Namen lang sind
    return(NULL)
  }
  
  #3. Case => Elternknoten 
  else{
    
    #Koordinaten berechnen
    left_coords  <- calculate_coords(order, height, tree$left)
    right_coords <- calculate_coords(order, height, tree$right)
    
    #Linien zeichnen 
    #Horizontale 
    segments(x0 = left_coords$x, 
             y0 = coords$y, 
             x1 = right_coords$x, 
             y1 = coords$y)
    
    #Vertikale Linie links
    segments(x0 = left_coords$x,
             y0 = coords$y,
             x1 = left_coords$x,
             y1 = left_coords$y)
    
    #Vertikale Linie rechts
    segments(x0 = right_coords$x,
             y0 = coords$y,
             x1 = right_coords$x,
             y1 = right_coords$y)
    
    #Kinderknoten berechnen
    draw_segments(calculate_coords(order, height, tree$left), tree$left, height, labels, order, names)
    draw_segments(calculate_coords(order, height, tree$right), tree$right, height, labels, order, names)
  }
}

plot_dendro <- function(coords, tree, order, height, labels, names,title=""){
  
  #Leeren Plot erzeugen mit x = 1 bis order,  y = 1 bis max. height
  par(mar = c(10, 4, 2, 2))
  plot(0, 0,
       type = "n",
       xlim = c(1, length(order)),
       ylim = c(-1.5, max(height)),
       xlab = "",
       ylab = "Distanz",
       xaxt = "n",
       main = title)   # ← Überschrift
}