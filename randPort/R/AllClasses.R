

setClass("randPort",
         representation = representation(
         match.var = "character",
         weight.var = "character",
         matched.weights = "matrix",
         universe = "data.frame",
         n = "numeric"
         ),
         prototype = prototype(
         match.var = character(),
         weight.var = character(),
         matched.weights = matrix(),
         universe = data.frame(),
         n = numeric()
         ),
         validity = function(object){})
