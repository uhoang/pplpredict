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

#' Calculate a relative confidence index between a winner and other candidates.
#'
#' @param x A matrix or data frame with n first columns are the winning ratings for n parties and the last column is the column index of a winner.
#' @param nLeaders A number of parties in the election race.
#' @return A matrix or data frame with n columns of relative confidence index for each party.
#' @examples 
#' RCIMeasure(cbind(matrix(1:9, 3,3), c(1,1,3)), 3)
#' RCIMeasure(data.frame(matrix(1:18, 6,3), c(1,1,2,2,2,1)), 3)
RCIMeasure <- function(x, nLeaders) {
  winnerIdx <- nLeaders + 1
  rci <- x[1:nLeaders] - x[x[winnerIdx]]
  rci[x[winnerIdx]] <- x[x[winnerIdx]] - sort(x[-x[winnerIdx]], decreasing = TRUE)[2]
  rci  
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
peoplePrediction <- function(Data, rci.vars, partyNames = NULL, transform = TRUE, ...){

    if(!is.data.frame(Data)) stop("Data has a wrong format. It must be a data.frame.")
    Rate <- Data[, rci.vars]
    
    if(transform) Rate <- data.frame(apply(Rate, 2, rescaleWrapper))
    nLeaders <- ncol(Rate)
    Rate$winnerIdx <- apply(Rate, 1, maxWrapper)
    
    names(Rate)[1:nLeaders] <- paste0("rate", 1:nLeaders)
    
    RCI <- data.frame(t(apply(Rate[, 1:(nLeaders+1)] , 1, RCIMeasure, nLeaders = nLeaders)))
    
    names(RCI) <- paste0("rci", 1:nLeaders)
    
    Data <- data.frame(Data, RCI, winnerIdx = Rate$winnerIdx)
    
    Data$rciWinner2ndRunner <- NA
    Data$rciWinner2ndRunner <- apply(Data[, c(paste0("rci", 1:nLeaders), "winnerIdx")], 1, 
                        function(x) x[-length(x)][x[length(x)]])
    
    if(!("weight" %in% names(Data))) Data$weight <- 1
    
    rciWinner2ndRunner <- vplTable(Data$rciWinner2ndRunner, Data$electorate, Data$weight)
    Predict <- data.frame(electorate = rciWinner2ndRunner$group1, rciWinner2ndRunner = rciWinner2ndRunner$value)
    
    for(i in 1:nLeaders){
      Predict[, paste0("rci", i)] <- vplTable(Data[, paste0("rci", i)], Data$electorate, Data$weight)$value 
    }
    
    Predict$winner <- paste0("rci", 1:nLeaders)[apply(Predict[, paste0("rci", 1:nLeaders)], 1, maxWrapper)]
    Predict$secondPlace <- paste0("rci", 1:nLeaders)[apply(Predict[, paste0("rci", 1:nLeaders)], 1, function(x) which(x == sort(x, decreasing = TRUE)[2]))]
    
    
    if(!is.null(partyNames)){
      Predict$winner <- partyNames[1:nLeaders][as.numeric(gsub("rci","",Predict$winner))] 
      Predict$secondPlace <- partyNames[1:nLeaders][as.numeric(gsub("rci","",Predict$secondPlace))] 
    }
    
    pplPrediction <- Predict[, c("electorate","winner", "secondPlace", "rciWinner2ndRunner", paste0("rci",1:nLeaders))]
    names(pplPrediction)[names(pplPrediction) %in% paste0("rci", 1:nLeaders)] <- paste0("rci",partyNames[1:nLeaders])
    pplPrediction
}

