#' import()
vplTable <- function(x, by = NULL, weight = 1, label.x = NULL, label.by = NULL, label.category = NULL, by.subset = NULL, 
            type = c("across","within","means"), sortData = c("alphabet","rank","manual"), categoryOrder = NULL){
  
  DF = data.frame()
  if(!is.null(by)) {D = data.frame(x, group1 = by, w = weight, stringsAsFactors = FALSE)
  }else{ D = data.frame(x, group1 = "Overall", w = weight, stringsAsFactors = FALSE) }
  
  #default Sort
  sortData <- sortData[1]
  
  #default type
  type <- type[1]
  
  # if by.subset is a data.frame, convert it to a vector 
  if(is.data.frame(by.subset)) by.subset <- as.vector(t(by.subset)) 
  
  # label.x is a vector 
  if(is.vector(label.x)){
    if(length(label.x) != (ncol(D) -2)) {
      stop("The length of labels.x does not match the number of variables.")
    }
    if(length(names(label.x)) > 0) { 
      label.x <- data.frame(variable = names(label.x), category1 = label.x) 
    }else{
      label.x <- data.frame(variable = names(D)[1:(ncol(D)-2)], category1 = label.x)
    }
  }

  if(is.data.frame(label.x)) names(label.x) <- c("variable", paste0("category", 1:(ncol(label.x) - 1)))
    
    # label.by is a vector 
  if(is.vector(label.by)){
    if(length(names(label.by)) > 0) { 
      label.by <- data.frame( group1 = names(label.by), group1Labels = label.by) 
    }else{
      label.by <- data.frame(variable = names(D)[1:(ncol(D)-2)], category1 = label.by)
    }
  }

  if(is.data.frame(label.by)) names(label.by) <- c("group1", "group1Labels")

  if( !is.null(by.subset) ) D <- subset(D, group1 %in% by.subset)

  if(is.numeric(D[,1])){
    DF <- plyr::ddply(D, plyr::.(group1), function(X){ plyr::colwise(weighted.mean, names(X)[1:(ncol(X)-2)]) (X, na.rm = TRUE, w = X$w) })
    DF <- reshape2::melt(DF, id.vars = "group1", strinsAsFactors = FALSE)
  }else{
    
    Table <- survey::svydesign(id = ~0, weights = ~ D$w, data = D)
    for(i in 1: (ncol(D) - 2)){
      variableOfInterest <- as.formula(paste0("~", names(D)[i], "+ group1"))
      sum.by <- 2
      if(type == "within") sum.by = 1
      SingleQuestion <-   reshape2::melt(prop.table(survey::svytable(variableOfInterest, design = Table, Ntot = 100), sum.by), stringsAsFactors = FALSE)
      SingleQuestion$value <- SingleQuestion$value * 100
      names(SingleQuestion)[1] <- "category1"
      SingleQuestion$variable <- names(D)[i]
      DF <- plyr::rbind.fill(DF, SingleQuestion)
    }
    if( !is.null(label.x) ) names(label.x) <- c("variable", "variableRename")
  }
  
  if( !is.null(label.x) ) { 
    if( nrow(label.x) == 1 ) DF$variable <- label.x$variable
    if( any(!DF$variable %in% label.x$variable)) stop("One of the variables in the data does not match the variables in label.x list.")
    DF <- merge(DF, label.x, by = "variable", all = FALSE)
  }
  
  if( !is.null(label.by) ) DF <- merge(DF, label.by, by = "group1", all = FALSE)
  if(!is.null(label.category)) { 
    names(DF)[names(DF) == "category1"] <- "category1Old"
    names(label.category) <- c("variable", "category1Old", paste0("category", 1:(ncol(label.category) -2)))
    DF <- merge(DF, label.category, by = c("variable","category1Old"), all = FALSE)
  }
  if(is.null(DF$category1)) DF$category1 <- DF$variable 
  
  if(sortData == "alphabet"){
    DF$category1 <- as.character(DF$category1)
    DF <- plyr::ddply(DF, plyr::.(variable,group1), function(X) X[order(X[,"category1"]),])
  }else if(sortData == "rank"){
    DF <- plyr::ddply(DF, plyr::.(variable,group1), function(X) X[order(X[,"value"]),])
  }else if(sortData == "manual"){
    DF$category1 <- factor(DF$category1, levels = categoryOrder)
    DF <- plyr::ddply(DF, plyr::.(variable,group1), function(X) X[order(X[,"category1"]),])
  }

  if(!is.null(label.category)) {
    for(i in 3:ncol(label.category)){
      var <- paste0("category", (i-2))
      DF[, var] <- factor(DF[,var], levels = unique(label.category[,i]))  
    }
  }
    
  if( !is.null(label.category) ) DF$category1Old <- factor(DF$category1Old, levels = sort(unique(DF$category1Old)))
  if( !is.null(by.subset) ) DF$group1 <- factor(DF$group1, levels = by.subset)
  DF <- DF[, sort(names(DF))]

  return(DF)
}
