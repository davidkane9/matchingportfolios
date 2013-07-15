
## randPort class ######################
setClass("randPort",
         representation = representation(
         match.var = "character",
         weight.var = "character",
         exposures = "numeric",
         matched.weights = "matrix",
         universe = "data.frame",
         n = "numeric",
         x0provided = "logical"
         ),
         prototype = prototype(
         match.var = character(),
         weight.var = character(),
         exposures = numeric(),
         matched.weights = matrix(),
         universe = data.frame(),
         n = numeric(),
         x0provided = logical()
         ),
         validity = function(object){})
