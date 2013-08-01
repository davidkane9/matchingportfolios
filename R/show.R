setMethod("show",
          signature(object = "randPort"),
          function(object){
            cat("A Collection of Random Portfolios\n")
            cat("Size of Universe:              ", paste(nrow(object@universe)))
            cat("Number of Random Portfolios:   ", paste(ncol(object@matched.weights)))
            cat("\n")
          }
          )
