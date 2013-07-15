
setMethod("overlap", signature = signature(object = "randPort"),
          function(object) {
            if(object@x0provided) {
                port.weights = object@universe[[object@weight.var]]
                cat("Positions in orignal portfolio: ", paste(length(which(port.weights != 0))), "\n")
                pos = aaply(object@matched.weights, 2, function(x) {
                    return(length(which(x != 0)))
                })
                cat("Average number of positions:    ", paste(mean(pos)), "\n")

                cat("\n")

                cat("From original portfolio:\n")
                Eds = aaply(object@matched.weights, 2, function(x) {
                    return(sqrt(sum( (x-port.weights)^2)))
                })
                cat("Average Euclidean distance: ", paste(mean(Eds)), "\n")

                cat("Maximum Euclidean distance: ", paste(max(Eds)), "\n")
                sameNames = aaply(object@matched.weights, 2, function(x) {
                    l = length( which( which(x !=0 ) %in% which(port.weights !=0)))
                    return(l)
                })
                cat("Average Same-Names:         ", paste(mean(sameNames)), "\n")
                percOverlap = aaply(object@matched.weights, 2, function(x) {
                    sN = length( which( which(x !=0 ) %in% which(port.weights !=0)))
                    len = length(which(x!= 0))
                    return(sN/len)
                })
                cat("Average Percent Same Name:  ", paste(mean(percOverlap)), "\n")
            }

            ## Calculate average correlation
            correlations = cor(object@matched.weights)
            cat("Random Portfolios:\n");
            cat("Mean Correlaton:  ", paste(mean(correlations[lower.tri(correlations)])), "\n")
            cat("Sd Correlations:  ", paste(sd(correlations[lower.tri(correlations)])), "\n")


        })
