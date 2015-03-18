#' @import ebal
#' @import plyr
weight <- function(Data, WeightData, variables, trim = TRUE){
    require(ebal)
    require(plyr)

    if(!all(variables %in% names(Data))){
        stopMessage <- paste0("Dataset does not contain the following variables:\n ", variables[which(!variables %in% names(Data))])
        stop(stopMessage)
    }

    if(!all(variables %in% names(WeightData))){
        stopMessage <- paste0("Weighting dataset does not contain the following variables:\n ", variables[which(!variables %in% names(WeightData))])
        stop(stopMessage)
    }

    makeBinary <- function(x, removeLast = TRUE){
        categories <- unique(na.omit(x))
        D <- matrix(ncol = length(categories), nrow = length(x))

        for(i in 1:length(categories)){
          D[, i] <- as.integer(x == categories[i])
        }
        
        invisible(gc())

        if(removeLast == FALSE) return(D)
        else if(removeLast == TRUE) return(D[, -ncol(D)])
    }

    Data$treatment <- 0
    WeightData$treatment <- 1

    missingRows <- which(apply(Data[, which(names(Data) %in% c("treatment", variables))], 1, function(X) any(is.na(X))))
    if(length(missingRows > 0)) warning("Missing data in dataset.")

    # Dataset which has missing data and cannot be weighted
    MissingData <- data.frame()
    if(length(missingRows) > 0){
      MissingData <- Data[missingRows, ]
      Data <- Data[-missingRows, ]
    }
    
    Merged <- rbind.fill(Data[, which(names(Data) %in% c("treatment", variables))],
                         WeightData[, which(names(WeightData) %in% c("treatment", variables))])

    treatment <- as.matrix(Merged$treatment); invisible(gc())

    binaries <- names(Merged)[which(unlist(lapply(Merged, class)) %in% c("character", "factor"))]
    continuous <- names(Merged)[which(unlist(lapply(Merged, class)) %in% c("numeric", "integer"))]
    continuous <- continuous[-which(continuous == "treatment")]

    X <- matrix(nrow = nrow(Merged), ncol = 0)

    # BINARIES
    if(length(binaries) > 0){
      for(i in 1:length(binaries)){
          X <- cbind(X, makeBinary(Merged[, binaries[i]], removeLast = TRUE))
      }
    }
    invisible(gc())
    
    # CONTINUOUS
    if(length(continuous) > 0) X <- cbind(X, Merged[, continuous])

    eb.out <- ebalance(Treatment = treatment, X = X)
    invisible(gc())
    if(trim == TRUE) eb.out <- ebalance.trim(eb.out)

    Data$weight <- eb.out$w

    if(nrow(MissingData) > 0) Data <- rbind.fill(Data, MissingData)
    Data <- Data[, -which(names(Data) == "treatment")]
    row.names(Data) <- 1:nrow(Data)
    return(Data)

}