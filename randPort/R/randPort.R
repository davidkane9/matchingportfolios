#' Makes a randPort object

#' Generates random portfolios for the evaluation of portfolio management

#' @param data A dataframe containing data about the universe of stocks
#' @param match.var Variables to match on
#' @param weight.var Weights for the stocks
#' @param n Number of generated portfolios
#' @param type Type of random sampling (shuffle, resample, reflect)

#' @keywords Random-Portfolio

#' @export
#' @examples
#'
#' rP = randPort(data = jan, match.var = "value",exposures = 0, n = 1000 )

randPort <- function(data, match.var=NULL, weight.var=NULL, exposures=NULL, n,
                     verbose = FALSE) {


    stopifnot(all(match.var %in% names(data)))
    if(!is.null(weight.var)) {
        stopifnot(length(weight.var) == 1)
        stopifnot(weight.var %in% names(data))
        if(all(data[[weight.var]] == 0)) {
            stop("All portfolio weights are set to zero")
        }
    }

    Amat = matrix(1, ncol = nrow(data), nrow = 1)
    for(v in match.var) {
        Amat = rbind(Amat, data[[v]])
    }

    if(!is.null(weight.var)) {
        if(!is.null(exposures)){
           warning("exposures overrides weight.var to determine constraints")
           b = c(1, exposures)
       } else {
           b =  Amat %*% data[[weight.var]]
       }
    } else {
        if(!is.null(exposures)) {
            b = c(1, exposures)
        } else {
            stop("One of 'weight.var' or 'exposures' must be provided")
        }
    }

    mW = hitandrun(A = Amat, b=b, n=n, verbose = verbose)

    if(is.null(match.var)) {
        match.var = character(0)
    }
    if(is.null(weight.var)){
        weight.var = character(0)
    }
    if(is.null(exposures)) {
        exposures = numeric(0)
    }

    rP = new("randPort", match.var = match.var, weight.var =
    weight.var, exposures = exposures, matched.weights = mW, universe = data, x0provided = !is.null(weight.var))
    return(rP)
}
