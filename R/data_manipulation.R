#' Queensland VoteCompass.
#'
#' A dataset containing the responses to 30 VoteCompass public opinion questions and other demographics of almost 5,000 
#' VoteCompass users.
#'
#' @format A data frame with 5000 rows and 166 variables:
#' \describe{
#'   \item{q1}{How much should Queensland spend to help drought-affected farmers?}
#'   \item{q2}{Queensland should pay down its debt by leasing public assets.}
#'   \item{q3}{Teachers' salaries should be tied to student outcomes, rather than seniority.}
#'   \item{q4}{Religion has no place in state schools.}
#'   \item{q5}{How much should Queensland do to reduce its greenhouse gas emissions?}
#'   \item{q6}{Environmental protections are an obstacle to economic growth.}
#'   \item{q7}{When there is an economic problem, government spending usually makes it worse.}
#'   \item{q8}{How much of Queensland's public hospital service should be contracted to the private sector?}
#'   \item{q9}{The government should pay for surgery in a private hospital if wait times in public hospitals are too long.}
#'   \item{q10}{How many temporary foreign workers should Queensland admit?}
#'   \item{q11}{How much should the government do to help Aborigines and Torres Strait Islanders find employment?}
#'   \item{q12}{Indigenous Queenslanders should be able to set their own alcohol laws.}
#'   \item{q13}{How tough should penalties be for domestic violence be in Queensland?}
#'   \item{q14}{A member of a bikie gang should receive a tougher sentence than someone else convicted of the same crime.}
#'   \item{q15}{How strict should Queensland's anti-bikie laws be?}
#'   \item{q16}{Clubs and bars should not be allowed to sell alcohol after midnight.}
#'   \item{q17}{How many frontline police officers does Queensland need?}
#'   \item{q18}{Marijuana should be legal when used for medicinal purposes.}
#'   \item{q19}{Terminally ill patients should be able to legally end their own lives with medical assistance.}
#'   \item{q20}{Same-sex couples should have the right to adopt children.}
#'   \item{q21}{How much should be done to accommodate religious minorities in Queensland?}
#'   \item{q22}{All immigrants can retain their cultural values without being any less Australian.}
#'   \item{q23}{How much mining activity should be permitted in the waters around the Great Barrier Reef?}
#'   \item{q24}{Landowners should be able to prevent mining companies from operating on their property.}
#'   \item{q25}{How many government workers should Queensland employ?}
#'   \item{q26}{How much should Queenslanders pay in state taxes?}
#'   \item{q27}{Rural and regional areas should offer the same government services as cities, even if it means higher taxes for everyone.}
#'   \item{q28}{Queensland should reduce its debt by cutting services rather than raising taxes.}
#'   \item{q29}{Traffic congestion in Queensland is best resolved by investing in roads, not in public transport.}
#'   \item{q30}{How much influence should unions have in Queensland?}
#'   ...
#' }
#' @source \url{http://www.queensland.votecompass.com/}
"QueenslandRaw"


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
recategorize <- function(x, x.new, labels = NA) {

	if (is.vector(x.new)) {
		if (is.null(names(x.new)) ) stop("Each new value in x.new must be corresponding to a value in the original x.")
		new.names <- data.frame(x.new, stringsAsFactors = FALSE)
		rownames(new.names) <- names(x.new) 
	} else if (!is.matrix(x.new) & !is.data.frame(x.new)) { 
		stop("The type of x.new is not correct. x.new must be a vector, a matrix or a data.frame, 
			 where the 1st column is the original value of x and the 2nd is the new value of x.")
	} else {
		new.names <- data.frame(x.new[ ,-1], stringsAsFactors = FALSE)
		rownames(new.names) <- x.new[ ,1]
	} 
	uniqueVals <- unique(x[!is.na(x)])
	if (any(!uniqueVals %in% rownames(new.names)) ) {
		x[x %in% uniqueVals[!uniqueVals %in% rownames(new.names)]] <- NA
		msg <- paste0("A list of unique values of x does not contain ", paste0(uniqueVals[!uniqueVals %in% rownames(new.names)], collapse = ", "), 
					  ". These values are replaced by missing values.")
		warning(msg)
	}
	if (any(is.na(labels))) labels <- paste0("X", 1:ncol(new.names))
	D <- data.frame(new.names[as.character(x), ], stringsAsFactors = FALSE)
	names(D) <- labels
	D
}

#' Rename categories of one or more variables in the dataset using an external match file
#' @aliases cleanData clean rename
#'
#' @param Data data frame to be processed.
#' @param varsClean a character vector of variables to be regrouped.
#' @param matchFileName a character string naming a match file. The match file is an excel file with one or multiple sheets. 
#' 		  Sheetname must be corresponding to variable names in \code{varsClean}. 
#' 		  Each sheet must contain the original value of the variable in the 1st column, and their 
#' 		  new values in those next columns. If the sheetname is \code{"varNames"}, rename the vairables of the dataset given in the sheet.
#' @param ... other arguments passed on to \code{read.xlsx} for reading the match file.
#' @return a data frame of regrouped variables.
#' @examples
#' data(QueenslandRaw)
#' 
#' matchFileName <- system.file("extdata","QueenslandMatch.xlsx", package = "pplpredict")
#' # View the first six lines of the sheet varNames in "QueenslandMatch.xlsx"
#' head(xlsx::read.xlsx(matchFileName, sheetName = "varNames", stringsAsFactors = FALSE, header = FALSE))
#'
#' cleanData(QueenslandRaw, c("genderRaw","studentRaw","educationRaw"), matchFileName)
#' cleanData(QueenslandRaw, c("genderRaw","studentRaw","educationRaw"), matchFileName)
#'
#' # Rename variables in a dataset
#' cleanData(QueenslandRaw, "varNames", matchFileName)
#' 
#' varsClean <- c("varNames", "genderRaw", "educationRaw", "studentRaw", "birthYearRaw", "industryRaw", 
#'				  "polInterestRaw", "polConsumptionRaw", "religionRaw", "selfPlacementRaw", "birthplaceRaw", 
#'				  "incomeRaw", "voteChoiceRaw", "voteChoiceLeaningRaw")
#' cleanData(QueenslandRaw, varsClean, "QueenslandMatch.xlsx") 

cleanData <- function (Data, varsClean, matchFileName, labels = NULL, trailingSpace = TRUE, na.strings = NULL, ...) {
    if (!basename(matchFileName) %in% list.files(dirname(matchFileName))) {
        stop(paste0(matchFileName, " does not exist."))
    }
    else {
        if (!grep("\\.xlsx", matchFileName, ignore.case = TRUE)) 
            stop("The matchFileName must have a xlsx extension.")
        if (any(varsClean == "varNames")) {
            matchFile <- xlsx::read.xlsx(matchFileName, sheetName = "varNames", 
                stringsAsFactors = FALSE, header = FALSE)
            if (any(!matchFile[,1] %in% names(Data))) {
            	msg <- paste0("There are no variables: ", paste0(matchFile[,1][!matchFile[,1] %in% names(Data)], collapse = ", "), 
            				  " in the dataset. These variables will be removed automatically.")
            	warning(msg)
            	matchFile <- matchFile[matchFile[,1] %in% names(Data), ]
            }
            matchData <- Data[, matchFile[, 1]]
            names(matchData) <- as.character(recategorize(matchFile[, 
                1], matchFile)[, 1])
            Data <- data.frame(Data[, !names(Data) %in% matchFile[, 
                1]], matchData)
        }
        varsClean <- varsClean[varsClean != "varNames"]
        if ( length(varsClean) > 0 ) {
            for (i in 1:length(varsClean)) {
            cat("Cleaning", varsClean[i], "\n")
            matchFile <- xlsx::read.xlsx(matchFileName, sheetName = varsClean[i], 
                stringsAsFactors = FALSE, header = FALSE)
            if (is.null(matchFile)) stop(paste0("The sheet ", varsClean[i], " in the matchFileName is empty."))
            matchFile <- matchFile[!duplicated(matchFile[, 1]), ]
            if (is.null(labels)) { label_i <- gsub("Raw", "", varsClean[i])
            } else { label_i <- labels[i] }
            if ((ncol(matchFile) - 1) > 1) { 
                if (!label_i %in% names(Data)) { label_i <- paste0(label_i, c("", 2:(ncol(matchFile) - 1)))
                } else { label_i <- paste0(label_i, c(1:(ncol(matchFile) - 1))) }   
            }

            var <- Data[ , varsClean[i]]
            if (trailingSpace & is.character(var)) var <- gsub("^\\s+|\\s+$", "", var)
            if (!is.null(na.strings)) var[grep(paste0(na.strings,collapse="|"), var)] <- NA
            Data <- data.frame(Data, recategorize(var, matchFile, labels = label_i))
            }
        }
        
    }
    return(Data)
}


# cleanData <- function(Data, varsClean, matchFileName, ...) {

# 	if (!basename(matchFileName) %in% list.files(dirname(matchFileName))) { 
# 		stop(paste0(matchFileName, " does not exist."))
# 	} else { 
# 		if (!grep("\\.xlsx", matchFileName, ignore.case = TRUE)) stop("The matchFileName must have a xlsx extension.") 
# 		if (any(varsClean == "varNames")) {
# 			matchFile <- xlsx::read.xlsx(matchFileName, sheetName = "varNames", stringsAsFactors = FALSE, header = FALSE)
# 			matchData <- Data[ , matchFile[ ,1]]
# 			names(matchData) <- as.character(recategorize(matchFile[ ,1], matchFile)[ ,1])
# 			Data <- data.frame(Data[ , !names(Data) %in% matchFile[ ,1]], matchData)
# 		} 
# 		varsClean <- varsClean[varsClean != "varNames"]
# 		for (i in 1:length(varsClean)) {
# 			cat("Cleaning", varsClean[i], "\n")
# 			matchFile <- xlsx::read.xlsx(matchFileName, sheetName = varsClean[i], stringsAsFactors = FALSE, header = FALSE)
# 			matchFile <- matchFile[!duplicated(matchFile[ ,1]), ]
# 			Data <- data.frame(Data, recategorize(Data[ , varsClean[i]], matchFile, labels = paste0(gsub("Raw", "", varsClean[i]), 1:(ncol(matchFile) - 1))))
# 		}
		
		
# 	}
# 	return(Data)
# }

#' Make indicator variables for each levels in a categorical variable.
#'
#' @param x a character vector of a categorical variable.
#' @param levels a character vector of unique levels in \code{x}.
#' @return a data frame of indicator variables of each level in the categorical variable \code{x}.
#' @examples 
#' data(Queensland)
#' makeIndicator(Queensland[,polActivitiesRaw], c("boycot","union","demonstration","internet","paper","volunteer"))
#' makeIndicator(Queensland[,"previousVoteRaw"], c("Liberal","Greens","Katter","Labor"))
makeIndicator <- function(x, levels) {

	Data <- data.frame(apply(matrix(levels, nrow = 1, byrow = TRUE), 2, function(l) { 
        vec <- as.numeric(grepl(l, x, ignore.case = TRUE))
        vec[is.na(x)] <- NA
        return(vec)
    }))
	names(Data) <- levels
	Data
}
