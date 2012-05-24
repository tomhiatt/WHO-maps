# Tom Hiatt
# 8 May 2012
# Functions for creating WHO maps

# ----------------------------------------------------------
# A slide-worthy WHO map
# ----------------------------------------------------------
  
WHOmap.slide <- function(data, map.title, legend.title, low.color='#BDD7E7',  high.color='#08519C', shapefiles.path=getwd(), na.label='No data') {
  
  # tests to make sure inputs are right
  if(nchar(legend.title)>25) warning("You might want to try and trim your legend title a bit.")
  if(nchar(legend.title)>10 & any(grep("\\n", legend.title))==FALSE) warning("You might want to wrap that legend title a couple of times with '\\n'.") 
  if(nchar(map.title)>50) warning("You might want to try and trim your title a bit or wrap it with '\\n'.")
  if(max(nchar(levels(data[["cat"]]))) > 10) warning("Check if your categories are running into Australia. (I hate it when that happens.)")
  
  if(all(names(data) %in% c("iso2", "cat"))==FALSE) stop("I need the data with just two columns labeled 'iso2' and 'cat' (for category).")
  if(!is.factor(data[["cat"]])) stop("I need you to make that cat column into a factor. (And order it in the order you want the legend to appear.)")
  
  #   OK let's get started!
  
  # Get and format shapefiles
  
  require(ggplot2)
  require(maptools)
  require(gpclib)
  require(plyr)
  require(grid)
  require(scales)
  
  # ----------------------------------------------------
  # Prepare map
  # ----------------------------------------------------
  
  old_path <- getwd()
  setwd (shapefiles.path)
  
  general <- readShapeSpatial("general_2011.shp")
  general_poly <- readShapeSpatial("maskpoly_general_2011.shp")  
  general_line <- readShapeSpatial("maskline_general_2011.shp")	
  
  gpclibPermit()	
  gworld <- fortify(general, region = "ISO_2_CODE")
  gpoly <- fortify(general_poly, region = "AREA")
  gline <- fortify(general_line, region = "COUNTRY")
  
  setwd(old_path)
  
  # Generic map parts
  
  pol1 <- geom_polygon(data=gworld, aes(group = group), colour = "grey50", fill = NA)   # A layer to map all countries (so none are missing.)
  pol2 <- geom_polygon(data = subset(gpoly, id=="Lakes"), aes(group = group), fill = I("white"), colour = "grey50") 	# Adds the polygon layer
  pol3 <- geom_polygon(data = subset(gpoly, id=="Jammu and Kashmir"), aes(group = group), fill = I("grey75"), colour = "grey50")  # To color Jammu Kashmir 
  pol4 <- geom_polygon(data = subset(gpoly, id=="Abyei"), aes(group = group), fill = I("grey75"), colour = "grey50", linetype="dotted")  # To color Abyei
  pol5 <-	geom_polygon(data = gworld[gworld$id=='EH',], aes(group = group), fill = I("grey75"), colour = "grey50")  # To color Western Sahara
  lin1 <- geom_path(data = subset(gline, id %in% 2), aes(group = group), colour = "grey50") 					# solid black lines
  lin2 <- geom_path(data = subset(gline, id %in% c(0,3,6,7)), aes(group = group), colour = "white", linetype = "dashed") 	# dashed white and black lines
  lin3 <- geom_path(data = subset(gline, id %in% c(1,4,5)), aes(group = group), colour = "grey50", linetype = "dashed") 	# dashed black lines
  lin4 <- geom_path(data = subset(gline, id %in% c(8)), aes(group = group), colour = "white", linetype = "dotted")   # dotted white lines (8 and 9 are the same!)
  thm1 <- scale_y_continuous('', breaks = NULL) 
  thm2 <- scale_x_continuous('', breaks = NULL) 
  thm3 <- theme_bw()
  
  #   Get colors
  x <- seq(0, 1, length=length(levels(data[["cat"]])))
  
  colors <- c(seq_gradient_pal(low.color, high.color)(x), 'grey90', 'grey75')
  
  
  #   Merge data
  toplot <- merge(gworld, data, by.x = "id", by.y = "iso2", all.x=TRUE)  
  toplot <- toplot[order(toplot$order), ]
  levels(toplot$cat) <- c(levels(toplot$cat), na.label, 'Not applicable')
  toplot[is.na(toplot$cat),"cat"] <- na.label
  toplot[toplot$id=="EH","cat"] <- 'Not applicable'
  
  # ----------------------------------------------------
  # Plot map
  # ----------------------------------------------------
  
  
  windows (12,8)	
  
  plot <-  ggplot(toplot, aes(long, lat)) +  
    geom_polygon(aes(group=group, fill=cat)) +
    pol1+pol2+pol3+pol4+pol5+lin1+lin2+lin3+lin4+thm1+thm2+thm3+
    geom_polygon(aes(group=group, fill=cat), toplot[toplot$id %in% c('SZ', 'LS'),]) +
    scale_fill_manual(legend.title, values=colors) +
    coord_cartesian(xlim = c(-180, 180)) +
    opts(title = paste(map.title, "\n"), aspect.ratio = 2.2/4, plot.title=theme_text(size=30),  
         legend.key.size = unit(0.75, "cm"), legend.text=theme_text(size=14), 
         legend.position=c(0.65, 0.45), legend.justification= c(0,1),
         legend.title=theme_text(size=17, hjust=0), panel.border=theme_blank())
  print(plot)
  return(plot)
  
  cat("Here you go. You can save it with ggsave if you'd like.")
  
}

# ----------------------------------------------------------
# A print-worthy WHO map
# ----------------------------------------------------------

WHOmap.print <- function(data, map.title, legend.title, low.color='#BDD7E7',  high.color='#08519C', shapefiles.path=getwd(), na.label='No data') {
  
  # tests to make sure inputs are right
  if(nchar(legend.title)>45) warning("You might want to try and trim your legend title a bit.")
  if(nchar(legend.title)>25 & any(grep("\\n", legend.title))==FALSE) warning("You might want to wrap that legend title a couple of times with '\\n'.") 
  if(nchar(map.title)>100) warning("You might want to try and trim your title a bit or wrap it with '\\n'.")
  if(max(nchar(levels(data[["cat"]]))) > 20) warning("Check if your categories are running into Australia. (I hate it when that happens.)")
  
  if(all(names(data) %in% c("iso2", "cat"))==FALSE) stop("I need the data with just two columns labeled 'iso2' and 'cat' (for category).")
  if(!is.factor(data[["cat"]])) stop("I need you to make that cat column into a factor. (And order it in the order you want the legend to appear.)")
  if(length(levels(data[["cat"]])) > 5) warning("You probably noticed your legend is dancing with the copyright. I can do up to 5 categories excluding NA.")
  
  #   OK let's get started!
  
  # Get and format shapefiles
  
  require(ggplot2)
  require(maptools)
  require(gpclib)
  require(plyr)
  require(grid)
  require(scales)
  
  # ----------------------------------------------------
  # Prepare map
  # ----------------------------------------------------
  
  old_path <- getwd()
  setwd (shapefiles.path)
  
  general <- readShapeSpatial("general_2011.shp")
  general_poly <- readShapeSpatial("maskpoly_general_2011.shp")  
  general_line <- readShapeSpatial("maskline_general_2011.shp")  
  
  gpclibPermit()	
  gworld <- fortify(general, region = "ISO_2_CODE")
  gpoly <- fortify(general_poly, region = "AREA")
  gline <- fortify(general_line, region = "COUNTRY")
  
  setwd(old_path)
  
  # Generic map parts
  
  pol1 <- geom_polygon(data=gworld, aes(group = group), colour = "grey50", fill = NA)   # A layer to map all countries (so none are missing.)
  pol2 <- geom_polygon(data = subset(gpoly, id=="Lakes"), aes(group = group), fill = I("white"), colour = "grey50") 	# Adds the polygon layer
  pol3 <- geom_polygon(data = subset(gpoly, id=="Jammu and Kashmir"), aes(group = group), fill = I("grey75"), colour = "grey50")  # To color Jammu Kashmir 
  pol4 <- geom_polygon(data = subset(gpoly, id=="Abyei"), aes(group = group), fill = I("grey75"), colour = "grey50", linetype="dotted")  # To color Abyei
  pol5 <-	geom_polygon(data = gworld[gworld$id=='EH',], aes(group = group), fill = I("grey75"), colour = "grey50")  # To color Western Sahara
  lin1 <- geom_path(data = subset(gline, id %in% 2), aes(group = group), colour = "grey50") 					# solid black lines
  lin2 <- geom_path(data = subset(gline, id %in% c(0,3,6,7)), aes(group = group), colour = "white", linetype = "dashed") 	# dashed white and black lines
  lin3 <- geom_path(data = subset(gline, id %in% c(1,4,5)), aes(group = group), colour = "grey50", linetype = "dashed") 	# dashed black lines
  lin4 <- geom_path(data = subset(gline, id %in% c(8)), aes(group = group), colour = "white", linetype = "dotted")   # dotted white lines (8 and 9 are the same!)
  thm1 <- scale_y_continuous('', breaks = NULL) 
  thm2 <- scale_x_continuous('', breaks = NULL) 
  thm3 <- theme_bw()
  
#   Copyright text
  copyright <- "\uA9 World Health Organization 2011. All rights reserved. \nThe designations employed and the presentation of the material in this publication do not \nimply the expression of any opinion whatsoever on the part of the World Health Organization \nconcerning the legal status of any country, territory, city or area or of its authorities, \nor concerning the delimitation of its frontiers or boundaries. Dotted and dashed lines on \nmaps represent approximate borderlines for which there may not yet be full agreement."
  
  #   Get colors
  x <- seq(0, 1, length=length(levels(data[["cat"]])))
  
  colors <- c(seq_gradient_pal(low.color, high.color)(x), 'grey90', 'grey75')
  
  
  #   Merge data
  toplot <- merge(gworld, data, by.x = "id", by.y = "iso2", all.x=TRUE)  
  toplot <- toplot[order(toplot$order), ]
  levels(toplot$cat) <- c(levels(toplot$cat), na.label, 'Not applicable')
  toplot[is.na(toplot$cat),"cat"] <- na.label
  toplot[toplot$id=="EH","cat"] <- 'Not applicable'
  
  # ----------------------------------------------------
  # Plot map
  # ----------------------------------------------------
  
  
  windows (12,8)	
  
  plot <-  ggplot(toplot, aes(long, lat)) +  
    geom_polygon(aes(group=group, fill=cat)) +
    pol1+pol2+pol3+pol4+pol5+lin1+lin2+lin3+lin4+thm1+thm2+thm3+
    geom_polygon(aes(group=group, fill=cat), toplot[toplot$id %in% c('SZ', 'LS'),]) +
    scale_fill_manual(legend.title, values=colors) +
    coord_cartesian(xlim = c(-180, 180)) +
    opts(title = paste(map.title, "\n"), aspect.ratio = 2.2/4, 
         plot.title=theme_text(size=16, hjust=0), 
       legend.key.size = unit(0.50, "cm"), legend.text=theme_text(size=8), 
       legend.position=c(0.73, 0.41), legend.justification= c(0.5,1),
       legend.title=theme_text(size=10, hjust=0), panel.border=theme_blank()) +
         annotate("text", 70, -54, label=copyright, size=2, hjust=0)
  
  print(plot)
  
  return(plot)
  
  cat("Here you go. You can save it with ggsave if you'd like.")
  
}