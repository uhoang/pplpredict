#' @imports ggplot2
my.theme <- function(base_size = 11, base_family = "",
                     grid.x_colour = NA, grid.y_colour = NA,
                     grid.x_linetype = 1, grid.y_linetype = 1,
                     strip_colour = "grey15", strip_text = "black",
                     background_colour = "transparent",
                     tick_colour = "black",
                     borderless = 0, bordersize = 0.5){ 
  if(is.na(grid.x_colour)) grid.x_colour <- "transparent"
  if(is.na(grid.y_colour)) grid.y_colour <- "transparent"
  if(borderless == 2){
    border <- theme(#axis.line = element_line(colour = "black", size = 0.25`),
                    panel.border = element_blank(),
                    panel.background = element_blank(),
                    strip.background = element_blank())
  }

  else if(borderless == 1){
    border <- theme(axis.line = element_line(colour = "black", size = 0.25),
                    panel.border = element_blank(),
                    panel.background = element_blank(),
                    strip.background = element_blank())
  }
  else if(borderless == 0) border <- theme()
    theme( 
      axis.text.x       = element_text(family = base_family, colour = "grey15", size = base_size, vjust = 1, lineheight = 0.9),
      axis.text.y       = element_text(family = base_family, colour = "grey15", size = base_size, hjust = 1, lineheight = 0.9),
      axis.ticks        = element_line(colour = tick_colour, size = 0.2),
      axis.ticks.length = unit(0.25, "lines"),
      axis.ticks.margin = unit(0.15, "cm"),
      axis.title.x      = element_text(family = base_family, size = base_size, colour = "black", vjust = -0.5),
      axis.title.y      = element_text(family = base_family, size = base_size, angle = 90, colour = "black", vjust = 0.25),
      legend.background = element_rect(colour = "grey15", fill = "transparent", size = 0.25),
      legend.key = element_blank(), #element_rect(colour = "transparent", fill = "transparent") 
      legend.key.size = unit(0.6, "lines"),
      legend.text = element_text(family = base_family, size = base_size, face = "italic", lineheight = 1),
      legend.text.align = 0, 
      legend.title = element_text(family = base_family, size = base_size, face = "plain", hjust = 0),
      legend.title.align = 0,
      legend.position = "top",
      legend.direction = "horizontal",
      legend.justification = c(1, 1),
      panel.background = element_rect(fill = "transparent", colour = "black", size = 0.25),
      panel.border = element_rect(fill = "transparent", colour = "black", size = bordersize),
      panel.grid.major.x = element_line(colour = grid.x_colour, size = 0.15, linetype = grid.x_linetype),
      panel.grid.major.y = element_line(colour = grid.y_colour, size = 0.15, linetype = grid.y_linetype),
      panel.grid.minor = element_blank(),
      panel.margin = unit(0.75, "lines"),
      strip.background = element_rect(fill = strip_colour, colour = "black", size = 0), # STRIP BACKGROUND
      strip.text.x = element_text(family = base_family, size = base_size, face = "plain", colour = strip_text),
      strip.text.y = element_text(family = base_family, size = base_size, face = "plain", angle = -90, colour = strip_text),
      plot.background = element_rect(fill = background_colour, colour = background_colour),
      plot.title = element_text(family = base_family, size = base_size * 1.1, vjust = 0, face = "bold")
    ) +
    border
}



makeTitle <- function(title, titleLines = 2, maxTitle = 58){
    n <- ceiling(nchar(title) / maxTitle)
    if(nchar(title) / maxTitle > titleLines){
       warningMessage <- paste0("The number of lines in your title is too few for the number of characters. Setting titleLines to ", ceiling(nchar(title) / maxTitle), ".")
       warning(warningMessage)
       titleLines <- ceiling(nchar(title) / maxTitle)
    }
    if(n == 1) return(paste0(c(rep("\n", titleLines - 1), title), collapse = ""))

    newTitle <- ""
    for(i in 1:(n-1)){
        cutNear <- nchar(title)/(n+1-i)
        cutAt <- unlist(gregexpr(" ", title))[which(abs(cutNear - unlist(gregexpr(" ", title))) == min(abs(cutNear - unlist(gregexpr(" ", title)))))] - 1
        newTitle[i] <- substr(title, 1, cutAt)
        title <- substr(title, cutAt+2, nchar(title))
    }
    newTitle <- paste(c(newTitle, title), collapse = "\n")

    return(paste0(c(rep("\n", titleLines - n), newTitle), collapse = ""))
}



barGraph <- function(Data, yMax = NULL, colour = NULL, fontsize = 11, decimal = 0, scale = 3, ncol = NA, sortCateType = "none", title = "", 
                     legend.position = "top", legend.title = NULL, legend.title.position = "left", legend.title.hjust = 0, xlabels = NULL){
  if(is.null(yMax)){
    yMax <- max(Data$value)
    yMax <- yMax + (0.16 * yMax)# Add 15% of the max to the y-axis height to leave room for number on top of bar
  }
  
  if(is.null(colour)) colour <- c("#86BADE", "#D86C58", "#49BB6C", "#C3A0C4", "#E3DB4A", "#C4758E", "#77B1A4", "grey40")

  if(!is.factor(Data$group1)) Data$group1 <- factor(Data$group1, levels = unique(Data$group1))
  levels(Data$group1) <- paste0(" ", levels(Data$group1), "  ")
  if(!is.null(names(colour))) names(colour) <- paste0(" ", names(colour), "  ")
  
  if(is.na(ncol)) ncol <- length(unique(Data$group1))
  if(ncol > length(unique(Data$group1))) ncol <- length(unique(Data$group1))

  # Sort category1 by a name of each category
  if(sortCateType == "order") Data <- Data[with(Data, order(category1)),]
  # Sort category1 descendingly by an overall mean value of each category
  else if(sortCateType == "value") Data$category1 <- reorder(Data$category1, -Data$value, mean) #Data <- Data[with(Data, order())]

  # Formatting the text of value labels for each bar graph by the number of decimal inputted
  Data$valueLabel <- sprintf(paste0("%.", decimal, "f"), round(Data$value, decimal))
  
  # xlabels is unspecified
  if(is.null(xlabels)){
    xbreaks <- unique(as.character(Data$category1))
    xlabels <- unique(as.character(Data$category1))
  # xlabels is specified as a vector
  }else if(is.vector(xlabels)){
    if(is.null(names(xlabels))) xbreaks <- unique(as.character(Data$category1))
    else xbreaks <- names(xlabels)
  # xlabels is specified as a data frame with the 1st column - an origional name of category1, 
  # the 2nd column - an new name of category1
  }else if(is.data.frame(xlabels)){
    xbreaks <- as.character(xlabels[,1])
    xlabels <- as.character(xlabels[,2])
  }

  print(ggplot(Data, aes(x = category1, y = value, label = valueLabel, fill = group1, colour = group1)) +
          my.theme(base_size = fontsize, borderless = 2,
                   background_colour = "transparent", tick_colour = "#A0A4A2") +
          coord_cartesian(y = c(0, yMax)) +
          scale_y_continuous(breaks = c()) +
          scale_x_discrete(breaks = xbreaks, labels = xlabels) +
          labs(x = "", y = "", title = title, colour = "", fill = "") +
          geom_bar(stat = "identity", fill = "transparent", position = position_dodge(width = 0.85), width = 0.6, size = 0.7, alpha = 1) +
          geom_bar(stat = "identity", position = position_dodge(width = 0.87), width = 0.6, size = 0.7, alpha = 0.88) +
          geom_text(colour = "grey15", stat = "identity", position = position_dodge(width = 0.85), size = (fontsize/scale), vjust = -0.85) +
          geom_hline(yintercept = 0, colour = "#A0A4A2", size = 0.5) +
          scale_fill_manual(values = colour) +
          scale_colour_manual(values = colour, guide = "none") +
          theme(plot.margin = unit(c(0, 0.5, -0.5, -1), "lines"),
                legend.position = legend.position) +
          guides(fill = guide_legend(title = legend.title, title.position = legend.title.position, title.hjust = legend.title.hjust, ncol = ncol, byrow = TRUE,
                 override.aes = list(colour = "transparent")
                 ))
       )

}


barGraph2 <- function(Data, yMax = NULL, colour = NULL, fontsize = 11, decimal = 0, scale = 3, ncol = NA, sortCateType = "none", title = "", 
                     legend.position = "top", legend.title = NULL, legend.title.position = "left", legend.title.hjust = 0, xlabels = NULL){
  if(is.null(yMax)){
    yMax <- max(Data$value)
    yMax <- yMax + (0.16 * yMax)# Add 15% of the max to the y-axis height to leave room for number on top of bar
  }
  
  if(is.null(colour)) colour <- c("#86BADE", "#D86C58", "#49BB6C", "#C3A0C4", "#E3DB4A", "#C4758E", "#77B1A4", "grey40")

  if(!is.factor(Data$group1)) Data$group1 <- factor(Data$group1, levels = unique(Data$group1))
  levels(Data$group1) <- paste0(" ", levels(Data$group1), "  ")
  if(!is.null(names(colour))) names(colour) <- paste0(" ", names(colour), "  ")
  
  if(is.na(ncol)) ncol <- length(unique(Data$group1))
  if(ncol > length(unique(Data$group1))) ncol <- length(unique(Data$group1))

  # Sort category1 by a name of each category
  if(sortCateType == "order") Data <- Data[with(Data, order(category1)),]
  # Sort category1 descendingly by an overall mean value of each category
  else if(sortCateType == "value") Data$category1 <- reorder(Data$category1, -Data$value, mean) #Data <- Data[with(Data, order())]

  # Formatting the text of value labels for each bar graph by the number of decimal inputted
  Data$valueLabel <- sprintf(paste0("%.", decimal, "f"), round(Data$value, decimal))
  
  # xlabels is unspecified
  if(is.null(xlabels)){
    xbreaks <- unique(as.character(Data$category1))
    xlabels <- unique(as.character(Data$category1))
  # xlabels is specified as a vector
  }else if(is.vector(xlabels)){
    if(is.null(names(xlabels))) xbreaks <- unique(as.character(Data$category1))
    else xbreaks <- names(xlabels)
  # xlabels is specified as a data frame with the 1st column - an origional name of category1, 
  # the 2nd column - an new name of category1
  }else if(is.data.frame(xlabels)){
    xbreaks <- as.character(xlabels[,1])
    xlabels <- as.character(xlabels[,2])
  }

  return(ggplot(Data, aes(x = category1, y = value, label = valueLabel, fill = group1, colour = group1)) +
          my.theme(base_size = fontsize, borderless = 2,
                   background_colour = "transparent", tick_colour = "#A0A4A2") +
          coord_cartesian(y = c(0, yMax)) +
          scale_y_continuous(breaks = c()) +
          scale_x_discrete(breaks = xbreaks, labels = xlabels) +
          labs(x = "", y = "", title = title, colour = "", fill = "") +
          geom_bar(stat = "identity", fill = "transparent", position = position_dodge(width = 0.85), width = 0.6, size = 0.7, alpha = 1) +
          geom_bar(stat = "identity", position = position_dodge(width = 0.87), width = 0.6, size = 0.7, alpha = 0.88) +
          geom_text(colour = "grey15", stat = "identity", position = position_dodge(width = 0.85), size = (fontsize/scale), vjust = -0.85) +
          geom_hline(yintercept = 0, colour = "#A0A4A2", size = 0.5) +
          scale_fill_manual(values = colour) +
          scale_colour_manual(values = colour, guide = "none") +
          theme(plot.margin = unit(c(0, 0.5, -0.5, -1), "lines"),
                legend.position = legend.position) +
          guides(fill = guide_legend(title = legend.title, title.position = legend.title.position, title.hjust = legend.title.hjust, ncol = ncol, byrow = TRUE,
                 override.aes = list(colour = "transparent")
                 ))
       )

}


dotPlot <- function(Data, fontsize = 11, scale = 3, ncol = 3, labels, title = "",
                    legend.title = NULL, legend.title.position = "left", legend.title.hjust = 0, 
                    legend.position = "top", legend = TRUE, plot.margin.left = 0, plot.margin.top = 0, colour = NULL, 
                    plot.title.vjust = 0) {

  levels(Data$group1) <- paste0(" ", levels(Data$group1), "  ")
  if(length(unique(Data$group1)) < ncol) ncol <- length(unique(Data$group1))

  Data$category1 <- reorder(Data$category1, Data$value, mean)  
  if (legend) { 
    guides <-  guides(fill = guide_legend(legend.position = legend.position, title = legend.title, title.position = legend.title.position, 
                 title.hjust = legend.title.hjust, ncol = ncol, byrow = TRUE, override.aes = list(colour = "transparent") ) ) 
  } else {
    guides <- guides(fill = FALSE)
  }
  if (is.null(colour)) colour <- c("#86BADE", "#D86C58", "#49BB6C", "#C3A0C4", "#E3DB4A", "#C4758E", "#77B1A4")

  print(ggplot(Data, aes(x = value, y = category1, colour = group1, fill = group1)) +
          my.theme(base_size = fontsize,          borderless = 2,
                   grid.x_colour = "grey80",      grid.y_colour = "grey80",
                   grid.x_linetype = 1,           grid.y_linetype = 1,
                   background_colour = "transparent", tick_colour = "grey80") +
          coord_cartesian(x = c(0.88, 6.1)) +
          scale_x_continuous(breaks = 1:6, labels = labels) +
          labs(x = "", y = "", colour = "", fill = "", title = title) +
          # geom_vline(xintercept = 3, linetype = 3, size = 0.25) +
          # geom_vline(xintercept = Inf, linetype = 1, size = 0.25, colour = "black") +
          geom_point(pch = 21, size = 3, fill = "transparent", alpha = 1) +
          geom_point(pch = 21, size = 3, alpha = 0.7) +
          # scale_fill_manual(values = c("#86BADE", "#D86C58", "#49BB6C", "#C3A0C4", "#E3DB4A", "#C4758E", "#77B1A4", "#C3A0C4", "#C7CED1")) +
          # scale_colour_manual(values = c("#86BADE", "#D86C58", "#49BB6C", "#C3A0C4", "#E3DB4A", "#C4758E", "#77B1A4", "#C3A0C4", "#C7CED1"), guide = "none") +
          scale_fill_manual(values = colour) +
          scale_colour_manual(values = colour, guide = "none") +
          theme(legend.position = "top",
                legend.key.size = unit(0.8, "lines"),
                legend.text = element_text(size = fontsize, face = "italic"),
                legend.background = element_rect(colour = "grey15", fill = "transparent", size = 0.25),
                axis.ticks.length = unit(0, "lines"),
                plot.margin = unit(c(plot.margin.top, 3, 0, plot.margin.left), "lines"),
                plot.title = element_text(vjust = plot.title.vjust)) +
          guides 
        )
}


dotPlot2 <- function(Data, fontsize = 11, scale = 3, ncol = 3, labels, title = "",
                    legend.title = NULL, legend.title.position = "left", legend.title.hjust = 0, 
                    legend.position = "top", legend = TRUE, plot.margin.left = 0, plot.margin.top = 0, colour = NULL, 
                    plot.title.vjust = 0) {

  levels(Data$group1) <- paste0(" ", levels(Data$group1), "  ")
  if(length(unique(Data$group1)) < ncol) ncol <- length(unique(Data$group1))

  Data$category1 <- reorder(Data$category1, Data$value, mean)  
  if (legend) { 
    guides <-  guides(fill = guide_legend(legend.position = legend.position, title = legend.title, title.position = legend.title.position, 
                 title.hjust = legend.title.hjust, ncol = ncol, byrow = TRUE, override.aes = list(colour = "transparent") ) ) 
  } else {
    guides <- guides(fill = FALSE)
  }
  if (is.null(colour)) colour <- c("#86BADE", "#D86C58", "#49BB6C", "#C3A0C4", "#E3DB4A", "#C4758E", "#77B1A4")

  ggplot(Data, aes(x = value, y = category1, colour = group1, fill = group1)) +
          my.theme(base_size = fontsize,          borderless = 2,
                   grid.x_colour = "grey80",      grid.y_colour = "grey80",
                   grid.x_linetype = 1,           grid.y_linetype = 1,
                   background_colour = "transparent", tick_colour = "grey80") +
          coord_cartesian(x = c(0.88, 6.1)) +
          scale_x_continuous(breaks = 1:6, labels = labels) +
          labs(x = "", y = "", colour = "", fill = "", title = title) +
          # geom_vline(xintercept = 3, linetype = 3, size = 0.25) +
          # geom_vline(xintercept = Inf, linetype = 1, size = 0.25, colour = "black") +
          geom_point(pch = 21, size = 3, fill = "transparent", alpha = 1) +
          geom_point(pch = 21, size = 3, alpha = 0.7) +
          # scale_fill_manual(values = c("#86BADE", "#D86C58", "#49BB6C", "#C3A0C4", "#E3DB4A", "#C4758E", "#77B1A4", "#C3A0C4", "#C7CED1")) +
          # scale_colour_manual(values = c("#86BADE", "#D86C58", "#49BB6C", "#C3A0C4", "#E3DB4A", "#C4758E", "#77B1A4", "#C3A0C4", "#C7CED1"), guide = "none") +
          scale_fill_manual(values = colour) +
          scale_colour_manual(values = colour, guide = "none") +
          theme(legend.position = "top",
                legend.key.size = unit(0.8, "lines"),
                legend.text = element_text(size = fontsize, face = "italic"),
                legend.background = element_rect(colour = "grey15", fill = "transparent", size = 0.25),
                axis.ticks.length = unit(0, "lines"),
                plot.margin = unit(c(plot.margin.top, 3, 0, plot.margin.left), "lines"),
                plot.title = element_text(vjust = plot.title.vjust)) +
          guides 
        
}



