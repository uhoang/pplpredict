#' Rescale a number from 0-10 to 0-1 scale.
#'
#' @param x A numeric number or vector.
#' @return The rescaled number or vector of \code{x} 
#' @examples 
#' rescaleWrapper(c(1, 2, 3))
#' rescaleWrapper(c(0, 1, NA))
rescaleWrapper <- function(x){
  x[is.na(x)] <- 0
  x <- x/10
  x[x > 1] <- 1
  x[x < 0 ] <- 0
  x 
}

#' Find an index of a maximum value in a numeric vector.
#'
#' @param x A numeric vector 
#' @return A index of a maximum value in \code{x}
#' @examples
#' maxWrapper(c(1, 3, 2))
#' maxWrapper(c(0.1, 0.5, 0.7))
maxWrapper <- function(x){
  idx <- which(x == max(x))
  if(length(idx) > 1) idx <- idx[sample(1:length(idx), 1)]
  idx 
}

# #' Calculate a relative confidence index between a winner and other candidates.
# #'
# #' @param x A matrix or data frame with n first columns are the winning ratings for n parties and the last column is the column index of a winner.
# #' @param nLeaders A number of parties in the election race.
# #' @return A matrix or data frame with n columns of relative confidence index for each party.
# #' @examples 
# #' RCIMeasure(cbind(matrix(1:9, 3,3), c(1,1,3)), 3)
# #' RCIMeasure(data.frame(matrix(1:18, 6,3), c(1,1,2,2,2,1)), 3)
# RCIMeasure <- function(x, nLeaders) {
#   winnerIdx <- nLeaders + 1
#   rci <- x[1:nLeaders] - x[x[winnerIdx]]
#   rci[x[winnerIdx]] <- x[x[winnerIdx]] - sort(x[-x[winnerIdx]], decreasing = TRUE)[2]
#   rci  
# }

#' Calcualte a relative confidence indexn between a winner and other candidates.
#'
#' @param x A numerical vector of the winning ratings for each party.
#' @param partyNames A character vector of party names in the election race.
#' @examples 
#' RCIMeasure(c(1, 10, 3))
#' RCIMeasure(c(1, 10, 3), c("Labor","Greens","LibNat"))
RCIMeasure <- function(x, partyNames = NULL){
  nLeaders <- length(x)
  winnerIdx <- maxWrapper(x)
  rci <- x[1:nLeaders] - x[winnerIdx]
  rci[winnerIdx] <- x[winnerIdx] - sort(x, decreasing = TRUE)[2]
  if(is.null(partyNames)) partyNames <- paste0("party",1:nLeaders)
  names(rci) <- partyNames
  list(rci = rci, rciWinner2ndRunner = as.numeric(rci[winnerIdx]), winner = partyNames[winnerIdx])
}



#' Calulate a people Prediction at electorate levels using relative confidence index 
#' 
#' @param Data A data frame of VoteCompass datasets 
#' @param rci.vars A character vector or a numeric vector of index of variable names of the winning ratings for each party.
#' @param partyNames A character vector of full names for each party.
#' @param transfrom A logical value to indicate if RCI variables are needed to rescale. Its default is TRUE. 
#' @examples 
#' peoplePrediction(Queensland, paste0("party", 1:4, "B"), partyNames = c("Labor","Greens","LibNat","Katter"))
#' peoplePrediction(Queensland, paste0("party", 1:3, "B"), partyNames = c("Labor","Greens","LibNat"))
Data <- Queensland
rci.vars <- paste0("party",1:4,"B")
partyNames = c("Labor","Greens","LibNat","Katter")
transform = TRUE

peoplePrediction <- function(Data, rci.vars, partyNames = NULL, transform = TRUE, ...){

    if(!is.data.frame(Data)) stop("Data has a wrong format. It must be a data.frame.")
    Rate <- Data[, rci.vars]
    
    if(transform) Rate <- data.frame(apply(Rate, 2, rescaleWrapper))
    
    RCI <- apply(Rate, 1, function(x) RCIMeasure(x, partyNames))
    RCI <- data.frame(do.call(rbind, lapply(RCI,unlist)), stringsAsFactors = FALSE)
    
    RCI[, -ncol(RCI)] <- apply(RCI[, -ncol(RCI)], 2, as.numeric)
    
    if(!("weight" %in% names(Data))) Data$weight <- 1
    
    rciWinner2ndRunner <- vplTable(RCI$rciWinner2ndRunner, Data$electorate, Data$weight)
    
    rciParties <- grep("rci\\.", names(RCI), value = TRUE)
    rciPredict <- vplTable(RCI[, rciParties], Data$electorate, Data$weight)
    rciPredict <- reshape(rciPredict[, -ncol(rciPredict)], v.names = "value", timevar = "category1", idvar = "group1", direction = "wide")
    rciPredict$winner <- partyNames[apply(rciPredict[, grep("value", names(rciPredict), value = TRUE)], 1, maxWrapper)]
    rciPredict$secondPlace <- partyNames[apply(rciPredict[,grep("value", names(rciPredict), value = TRUE)], 1, function(x) which(x == sort(x, decreasing = TRUE)[2]))]
    names(rciPredict)[1:(length(rciPredict)-2)] <- c("electorate", gsub("value\\.", "",rciParties))
    
    rciPredict
}

