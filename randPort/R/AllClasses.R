
## randPort class ######################
setClass("randPort",
         representation = representation(
         match.var = "character",
         weight.var = "character",
         matched.weights = "matrix",
         universe = "data.frame",
         n = "numeric",
         type = "character"
         ),
         prototype = prototype(
         match.var = character(),
         weight.var = character(),
         matched.weights = matrix(),
         universe = data.frame(),
         n = numeric(),
         type = character()
         ),
         validity = function(object){})
