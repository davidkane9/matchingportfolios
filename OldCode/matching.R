## generates weights for a random, long-only portfolio

matching <- function(x, date.var, match.var, ret.var, port.weight, number = 100, type = "matchingUniform", positionConstraint = .005) {

    stopifnot(c(date.var, match.var, ret.var, port.weight) %in% names(x))

    if(length(unique(x[[date.var]])) != 1) {
        multi = TRUE
    } else {
        multi = FALSE
    }

    ## save the number of stocks in the portfolio for later
    numstocks = length(which(x[[port.weight]] != 0))

    if(type == "xsample") {

        ## use xsample to generate the weights based on basic
        ## constraints, could take a minute
        writeLines("Please wait as the random portfolios are generated...")
        sol = xsample(E = rbind(NULL,rep(1, numstocks)), F = 1, G = diag(numstocks), H = rep(0, numstocks), iter = 9+number, jmp = .02, x0 = x[["port.weight"]])

        ## assigns the weights to 'numstocks' randomly chosen stocks
        weights.list = list()
        for(i in 1:number) {
            indexes = sample(1:nrow(x), numstocks)
            newrow = matrix(rep(0, nrow(x)), nrow = nrow(x), ncol = 1)
            newrow[indexes,] = sol$X[9+i,] # I skip the first 9, they
                                           # are not as random
            weights.list = c(weights.list, list(w = newrow))
        }

        weights.mat = do.call(cbind, weights.list)

    } else if(type == "shuffle") {
        weights.list = list()
        weights = x[[port.weight]][which(x[[port.weight]] > 0)]

        ## randomly sample a new list of indexes to set to our already
        ## established portfolio weights
        for(i in 1:number) {
            indexes = sample(1:nrow(x), numstocks)
            newrow = matrix(rep(0, nrow(x)), nrow = nrow(x), ncol = 1)
            newrow[indexes,] = weights
            weights.list = c(weights.list, list(w = newrow))
        }

        weights.mat = do.call(cbind, weights.list)
    } else if(type == "equalWeighted") {
        weights.list = list()

        for(i in 1:number) {
            indexes = sample(1:nrow(x), numstocks)
            newrow = matrix(rep(0, nrow(x)), nrow = nrow(x), ncol = 1)
            newrow[indexes,] = 1/numstocks
            weights.list = c(weights.list, list(w = newrow))
        }

        weights.mat = do.call(cbind, weights.list)
    } else if(type == "resampling") {
        weights.mat = matrix(sample(x[[port.weight]], nrow(x)*number, replace = TRUE), ncol = number)
        weights.mat = apply(weights.mat, 2, function(c) c/sum(c))
    } else if(type == "Zoonekynd") {
        writeLines("Note: this type of sampling is meant to be biased")
        weights.mat = matrix(rexp(nrow(x)*number), ncol = number)
        weights.mat = weights.mat^3
        weights.mat = apply(weights.mat, 2, function(c) c/sum(c))
    } else if(type == "matchingUniform") {

        weights.list = list()

        ## index varable will help keep track of things later
        x$index = 1:nrow(x)

        ## seperate variables into discrete and continuous
        match.var.c = NULL
        match.var.d = NULL
        for(v in match.var) {
            if(class(jan[[v]]) == "numeric") {
                match.var.c = append(match.var.c, v)
            } else {
                match.var.d = append(match.var.d, v)
            }
        }

        ## Store the stocks that have weight in the original
        invested = x[which(x[[port.weight]] != 0), ]

        ## intialize a null linear programming solution, because
        ## xsample's intial LP sucks, we'll initialize it in the loop
        ## and use it as x0 for the rest of xsample
        xInit = NULL

        cat("Done with: 0")
        i = 1
        while(i <= number) {
            ## there will be errors sometimes because we just don't
            ## pick stocks compatable with the constraints, handle
            ## these
            tryCatch({
                ## Select stocks to put money in:
                ## we want to randomize the stocks as well, however, we need
                ## to make sure to not accidentally leave out a category, like
                ## not sampling any stocks in the United States, or we will
                ## have trouble filling the constraints
                selected = ddply(x, match.var.d, function(df) {
                    num = length(which(df[[port.weight]] != 0))
                    return(df[sample(1:nrow(df), num),])
                })

                ## Intialize the first row of Emat
                Emat = matrix(rep(1, nrow(selected)), nrow = 1)
                fvec = 1

                ## Now do the continuous variables
                for(v in match.var.c) {
                    Emat = rbind(Emat, selected[[v]])
                    fvec = append(fvec, invested[[v]] %*% invested[[port.weight]])
                }

                ## Now the discrete variables
                for(v in match.var.d) {
                    dummy.mat = .dummy(selected[[v]])
                    Emat = rbind(Emat, dummy.mat)
                    dummy.to.match = .dummy(invested[[v]])
                    fvec = append(fvec, dummy.to.match %*% invested[[port.weight]])
                }

                ## now sample the space!
                ## writeLines("got to xsample... this could take a while...")
                sol = xsample(E = Emat, F = fvec, G = diag(nrow(invested)), H = rep(0, nrow(invested)), iter = 5, jmp = .002)
                weights = rep(0, nrow(x))
                weights[selected$index] = sol$X[5,]

                weights.list = c(weights.list, list(col = weights))
            }, error = function(e) {
                i = i - 1
            })
            if(i > 1000) {
                cat("\b\b\b\b")
            } else if( i > 100) {
                cat("\b\b\b")
            } else if( i > 10) {
                cat("\b\b")
            } else {
                cat("\b")
            }
            cat(i)
            i = i + 1
        }
        weights.mat = do.call(cbind, weights.list)
    } else if(type == "randomOptimization") {
        weights.list = list()

        match.var.d = NULL
        match.var.c = NULL
        for(v in match.var) {
            if(class(jan[[v]]) == "numeric") {
                match.var.c = append(match.var.c, v)
            } else {
                match.var.d = append(match.var.d, v)
            }
        }

        ## Intialize the first row of Emat
        Emat = matrix(rep(1, nrow(x)), nrow = 1)

        ## Now do the continuous variables
        for(v in match.var.c) {
            Emat = rbind(Emat, x[[v]])
        }

        ## Now the discrete variables
        for(v in match.var.d) {
            dummy.mat = .dummy(x[[v]])
            Emat = rbind(Emat, dummy.mat)
        }

        fvec = Emat %*% x[[port.weight]]

        i = 1
        cat("Done with: 0")
        while(i <= number){
            alpha = rnorm(ncol(Emat), 0, 5)
            upperbounds = rep(positionConstraint, ncol(Emat))
            tryCatch({sol = Rglpk_solve_LP(alpha, Emat, dir = rep("==", nrow(Emat)), rhs = fvec, max = TRUE, bounds = list(lower = list(ind = 1:ncol(Emat), val = rep(0, ncol(Emat))), upper = list(ind = 1:ncol(Emat), val =  upperbounds)))## rep(2*max(x[[port.weight]]), ncol(Emat)))))
                      weights.list = c(weights.list, list(w = sol$solution))

                  }, error = function(e) {
                      i = i - 1
                  })
            if(i > 1000) {
                cat("\b\b\b\b")
            } else if( i > 100) {
                cat("\b\b\b")
            } else if( i > 10) {
                cat("\b\b")
            } else {
                cat("\b")
            }
            cat(i)
            i = i + 1
        }
        cat("\n")
        weights.mat = do.call(cbind, weights.list)
    } else{
        stop(paste("Invalid type: ", type, sep = ""))
        weights.mat = NULL
    }
    colnames(weights.mat) = NULL
    m = new("matching", date.var = date.var, ret.var = ret.var, portfolio.weights = port.weight, number = number,  matching.weights = weights.mat, universe = x)
    return(m)
}
