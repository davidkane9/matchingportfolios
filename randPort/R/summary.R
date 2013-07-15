
setMethod("summary",
          signature(object = "randPort"),
          function(object) {
              show(object)

              if(length(ret.var) > 0) {
                m = returns(object)$matching
                o = returns(object)$original
                cat(sprintf("Return of original portfolio:          %-s",
                            paste(o)
                            ), "\n",
                    sprintf("Average return of matching portfolios: %-s",
                            paste(sum(m)/length(m))), "\n",
                    sprintf("Standard deviation:                    %-s",
                            paste(sd(m))), "\n",
                    sprintf("Outperformance Rate:                   %-s",
                            paste(length(which(m < o))/length(m))), "\n\n",
                    sprintf("Summary of matching portfolio returns:"), "\n",
                    sep = ""
                    )
                print(summary(m))
              }

      })
