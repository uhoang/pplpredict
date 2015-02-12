#' Recategorize a character or numerical vector.
#' @aliases recategorize recate 
#'
#' @param x a character or numerical vector.
#' @param x.new a character/numerical vector of the new values for \code{x}. \code{x.new} can also take a form of a matrix or data frame, 
#'              where the first column is the original value of \code{x} and the second is the new value of \code{x}.
#' @return a character or numerical vector is recategorized.
#' @examples
#' recategorize(c(1,2,5,3,4), c("1" = 1, "2" = 1, "3" = 0))
#' recategorize(c(1,2,3), cbind(c(1,2,3), c(1,1,0), c(1,-1,0)))
#' recategorize(c(1,4,2,5,3,NA), data.frame(c(1,2,3), c(1,1,0), c(1, -1, 0)) )
recategorize <- function(x, x.new, labels = NA){

	if(is.vector(x.new)){
		if(is.null(names(x.new))) stop("Each new value in x.new must be corresponding to a value in the original x.")
		new.names <- as.matrix(x.new)
		rownames(new.names) <- names(x.new) 
	}else if(!is.matrix(x.new) & !is.data.frame(x.new) ){ 
		stop("The type of x.new is not correct. x.new must be a vector, a matrix or a data.frame, 
			 where the 1st column is the original value of x and the 2nd is the new value of x.")
	}else{
		new.names <- data.frame(x.new[,-1])
		rownames(new.names) <- x.new[,1]
		x.new <- new.names
	} 
	uniqueVals <- unique(x[!is.na(x)])
	if(any(!uniqueVals %in% rownames(new.names)) ) {
		x[x %in% uniqueVals[!uniqueVals %in% rownames(new.names)]] <- NA
		msg <- paste0("A list of unique values of x does not contain ",paste0(uniqueVals[!uniqueVals %in% rownames(new.names)], collapse = ", "), 
							 ". These values are replaced by missing values.")
		warning(msg)
	}
	if(any(is.na(labels))) labels <- paste0("X",1:ncol(new.names))
	D <- data.frame(new.names[x, ], stringsAsFactors = FALSE)
	names(D) <- labels
	D
}

#' Rename categories of one or more variables in the dataset using an external match file
#' @aliases cleanData clean rename
#'
#' @param Data data frame to be processed.
#' @param varsClean a character vector of variables to be regrouped.
#' @param matchFileName a character string naming a match file.
#' @param ... other arguments passed on to \code{read.xlsx} for reading the match file.
#' @return a data frame of regrouped variables.
#' @examples
#' cleanData(QueenslandRaw, c("genderRaw","studentRaw","educationRaw"), "~/Desktop/R Packages/QueenslandMatch.xlsx")
#' cleanData(QueenslandRaw, c("genderRaw","studentRaw","educationRaw"), "QueenslandMatch.xlsx")
cleanData <- function(Data, varsClean, matchFileName, ...){

	if(!basename(matchFileName) %in% list.files(dirname(matchFileName))) { 
		stop(paste0(matchFileName, " does not exist."))
	}else{ 
		if(!grep("\\.xlsx", matchFileName, ignore.case = TRUE)) stop("The matchFileName must have a xlsx extension.") 
		if(varClean == "codeBook"){
			if(!"header" %in% names(args)) header = FALSE
			matchFile <- xlsx::read.xlsx2(matchFileName, sheetName = varsClean[i], stringsAsFactors = FALSE, header = header)
			names(Data[,matchFile[,1]]) <- recategorize(matchFile[,1], matchFile) 
		}
		for(i in 1:length(varsClean)){
			args <- match.call()
			if(!"header" %in% names(args)) header = FALSE
			matchFile <- xlsx::read.xlsx2(matchFileName, sheetName = varsClean[i], stringsAsFactors = FALSE, header = header)
			Data <- data.frame(Data, recategorize(Data[,varsClean[i]], matchFile, labels = paste0(gsub("Raw","", varsClean[i]),1:(ncol(matchFile)-1))))
		}
	}
	Data
}


# Data <- QueenslandRaw
# varsClean <- c("genderRaw","studentRaw","educationRaw")
# matchFileName <- "~/Desktop/R Packages/QueenslandMatch.xlsx"
# header <- FALSE
# NewData <- cleanData(QueenslandRaw, c("genderRaw","studentRaw","educationRaw"), "~/Desktop/R Packages/QueenslandMatch.xlsx")
