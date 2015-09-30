#' Rescale vector elements.
#'
#' \code{rescaleWrapper} returns the vector of rescaled values 
#' @aliases resecaleWrapper rescale rescaled
#' @param x a numeric number or vector.
#' @param domain a vector includes minimum and maximum values of the domain of \code{x}.
#' @param range a vector includes minimum and maximum values of the range of \code{x}.
#' @param bounded a logical scalar. Default is FALSE. The output vector of scaled \code{x} can not less or greater than minimum or maximum value of the range of \code{x}.
#' @param na.replace a numeric or character that missing values is replaced by. \code{na.replace} is missing at default. 
#' @return the scaled values of \code{x} in the \code{range} for the given \code{domain}.
#' @examples 
#' rescaleWrapper(c(1, 2, 3), range = c(0, 1)) 
#' rescaleWrapper(c(0, 1, NA), range = c(0, 1))
#' rescaleWrapper(c(0, 1, 5, -3, NA), domain = c(0, 10), range = c(0, 1), bounded = FALSE, na.replace = NA)
#' rescaleWrapper(c(0, 1, 5, -3, NA), domain = c(0, 10), range = c(0, 1), bounded = TRUE, na.replace = NA)
rescaleWrapper <- function(x, domain = NULL, range = NULL, bounded = FALSE, na.replace = NA) {

  if (is.null(domain)) {
    dmax <- max(x, na.rm = TRUE)
    dmin <- min(x, na.rm = TRUE)
  } else {
    dmax <- domain[2]
    dmin <- domain[1]
  }

  if (is.null(range)) {
    stop("The mapping requires the range of x.")
  } else {
    rmax <- range[2]
    rmin <- range[1]
  }
  
  x <- rmin + (x - dmin) / (dmax - dmin) * (rmax - rmin)
  
  if (bounded) {
    x[x < rmin] <- rmin
    x[x > rmax] <- rmax 
  }
  x[is.na(x)] <- na.replace
  x 
}

#' Find an index of a maximum value in a numeric vector.
#' @aliases maxWrapper max maximum
#' @param x a numeric vector 
#' @return an index of a maximum value in \code{x}
#' @examples
#' maxWrapper(c(1, 3, 2))
#' maxWrapper(c(0.1, 0.5, 0.7))
maxWrapper <- function(x){
  idx <- which(x == max(x))
  if (length(idx) > 1) idx <- idx[sample(1:length(idx), 1)]
  idx 
}


#' Calcualte a relative confidence index between a winner and other candidates.
#' @aliases RCIMeasure RCI rci 
#' @param x a numerical vector of the winning ratings for each party.
#' @param partyNames a character vector of party names in the election race.
#' @return a vector of relative confidence indexes by each party. 
#' @examples 
#' RCIMeasure(c(1, 10, 3))
#' RCIMeasure(c(1, 10, 3), c("Labor", "Greens", "LibNat"))
RCIMeasure <- function(x, partyNames = NULL) {
  nLeaders <- length(x)
  winnerIdx <- maxWrapper(x)
  rci <- x[1:nLeaders] - x[winnerIdx]
  rci[winnerIdx] <- x[winnerIdx] - sort(x, decreasing = TRUE)[2]
  if (is.null(partyNames)) partyNames <- paste0("party", 1:nLeaders)
  names(rci) <- partyNames
  list(rci = rci, rciWinner2ndRunner = as.numeric(rci[winnerIdx]), winner = partyNames[winnerIdx])
}

RCIMatrix <- function(Data, rci.vars, partyNames = NULL, transform = FALSE, ...) {
  if (!is.data.frame(Data)) stop("Data has a wrong format. It must be a data.frame.")
  Rate <- Data[ , rci.vars]
  args <- match.call()

  if (transform) { 
    if (!"domain" %in% names(args)) domain = c(0, 10)
    if (!"range" %in% names(args)) range = c(0, 1)
    if (!"bounded" %in% names(args)) bounded = TRUE
    if (!"na.replace" %in% names(args)) na.replace = 0
    Rate <- data.frame(apply(Rate, 2, rescaleWrapper, domain = domain, range = range, bounded = bounded, na.replace = na.replace))
  }
  RCI <- apply(Rate, 1, function(x) RCIMeasure(x, partyNames))
  RCI <- data.frame(do.call(rbind, lapply(RCI, unlist)), stringsAsFactors = FALSE)    
  RCI[ , -ncol(RCI)] <- apply(RCI[ , -ncol(RCI)], 2, as.numeric)       
  
  RCI

}


#' Calulate a people Prediction at electorate levels using relative confidence index 
#' @aliases peoplePrediction pplpredict peoplepredict
#' @param Data a data frame of VoteCompass datasets 
#' @param rci.vars a character vector or a numeric vector of index of variable names of the winning ratings for each party.
#' @param partyNames a character vector of full names for each party.
#' @param transfrom a logical scalar to indicate if RCI variables are needed to rescale. Its default is FALSE. 
#' @param ... other arguments passed to \code{rescaleWrapper} when \code{transfrom} is set to TRUE
#' @return a data frame of aggregated relative confidence index for each party by electorates/ridings, the winner and the secondplace runner by RCI. 
#' @examples 
#' peoplePrediction(Queensland, paste0("party", 1:4, "B"), partyNames = c("Labor", "Greens", "LibNat", "Katter"))
#' peoplePrediction(Queensland, paste0("party", 1:4, "B"), partyNames = c("Labor ","Greens" ,"LibNat", "Katter"), transform = TRUE)
#' peoplePrediction(Queensland, paste0("party", 1:3, "B"), partyNames = c("Labor", "Greens", "LibNat"))

peoplePrediction <- function(Data, rci.vars, partyNames = NULL, transform = FALSE, ridingVar = c("riding", "electorate", "district"), ...) {

  # if (!is.data.frame(Data)) stop("The data has a wrong format. It must be a data.frame.")
  # Rate <- Data[ , rci.vars]
  # args <- match.call()

  # if (transform) { 
  #   if (!"domain" %in% names(args)) domain = c(0, 10)
  #   if (!"range" %in% names(args)) range = c(0, 1)
  #   if (!"bounded" %in% names(args)) bounded = TRUE
  #   if (!"na.replace" %in% names(args)) na.replace = 0
  #   Rate <- data.frame(apply(Rate, 2, rescaleWrapper, domain = domain, range = range, bounded = bounded, na.replace = na.replace))
  # }

  # RCI <- apply(Rate, 1, function(x) RCIMeasure(x, partyNames))
  # RCI <- data.frame(do.call(rbind, lapply(RCI, unlist)), stringsAsFactors = FALSE)    
  # RCI[ , -ncol(RCI)] <- apply(RCI[ , -ncol(RCI)], 2, as.numeric)    
  RCI <- RCIMatrix(Data = Data, rci.vars = rci.vars, partyNames = partyNames, transform = transform, ...)

  if (!("weight" %in% names(Data))) { 
    warnings("The weight is set to 1.")
    Data$weight <- 1    

  }
  if (all(! ridingVar %in% names(Data))) {
    stop("The data has no riding/ electorate variables.")
  } else {
    ridingVar <- ridingVar[ridingVar %in% names(Data)][1]
  } 

  rciWinner2ndRunner <- vplTable(RCI$rciWinner2ndRunner, Data[ , ridingVar], Data$weight)    
  
  rciParties <- grep("rci\\.", names(RCI), value = TRUE)
  rciPredict <- vplTable(RCI[ , rciParties], Data[ , ridingVar], Data$weight)
  rciPredict <- reshape(rciPredict[ , -ncol(rciPredict)], v.names = "value", timevar = "category1", idvar = "group1", direction = "wide")    
  
  winnerIdx <- apply(rciPredict[ , grep("value", names(rciPredict), value = TRUE)], 1, maxWrapper)
  rciPredict$winner <- partyNames[winnerIdx]
  rciPredict$secondPlace <- partyNames[apply(rciPredict[ ,grep("value", names(rciPredict), value = TRUE)], 1, function(x) which(x == sort(x, decreasing = TRUE)[2]))]    
  names(rciPredict)[1:(length(rciPredict)-2)] <- c(ridingVar, gsub("value\\.", "", rciParties))
  winnerRCI <- plyr::ddply(rciPredict, plyr::as.quoted(ridingVar), function(D) D[,paste0("rci.", D$winner)])[,2]
  secondRunnerRCI <- plyr::ddply(rciPredict, plyr::as.quoted(ridingVar), function(D) D[,paste0("rci.", D$secondPlace)])[,2]
  rciPredict$certainty <- winnerRCI - secondRunnerRCI
  rciPredict$normalizedCertainty <- (rciPredict$certainty - min(rciPredict$certainty))/(max(rciPredict$certainty) - min(rciPredict$certainty)) 
  rciPredict
}

