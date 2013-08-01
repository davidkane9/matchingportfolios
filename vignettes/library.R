formatpercent <- function(vec, digits = 1, escape = 2) {
  rst <- paste(formatC(vec * 100, digits = digits, format = "f"),
               paste(c(rep("\\", escape), "%"), collapse = ""), sep = "")
  dim(rst) <- dim(vec)
  dimnames(rst) <- dimnames(vec)
  rst
}

Sweave.go <- function(f) {
  Sweave(paste(f, ".Rnw", sep = ""))
  system(paste("pdflatex ", f, ".tex; latex ", f, ".tex; evince ", f, ".pdf &", sep = ""))
}

scale1 <- function(vec) vec / sum(vec)

uni.expo2 <- function(x, ws, cols) {
  mf <- do.call("rbind", sapply(seq(along = cols), function(i) {
    do.call("rbind", sapply(seq(along = ws), function(j) {
      data.frame(por = names(ws)[j],
                 var = names(cols)[i], 
                 lev = names(tapply(x[[ws[j]]], x[[cols[i]]], sum)), 
                 wei = tapply(x[[ws[j]]], x[[cols[i]]], sum))
    }, simplify = FALSE))
  }, simplify = FALSE))
  mf$lev <- factor(mf$lev, levels = rev(unique(mf$lev)))
  mf$var <- factor(mf$var, levels = names(cols))
  mf$por <- factor(mf$por, levels = rev(names(ws)))
  print(barchart(lev ~ wei | var, mf, groups = por, 
                 as.table = TRUE, xlab = "Weight",
                 scales = list(alternating = 1, y = list(relation = "free")),
                 xlim = c(min(mf$wei) - .01, max(mf$wei) + .01),
                 auto.key = list(reverse.rows = TRUE, columns = length(ws)),
                 par.settings = my.theme(),
                 main = "Exposures of Portfolio, Universe",
                 strip = strip.custom(par.strip.text = list(cex = .7)),
                 origin = 0))
}

abs.bias <- function(w.m, w.t, v, d) {
  e.m <- sapply(v, function(cur.v) tapply(d[[w.m]], d[[cur.v]], sum))
  e.t <- sapply(v, function(cur.v) tapply(d[[w.t]], d[[cur.v]], sum))
  sapply(v, function(cur.v) sum(abs(e.m[[cur.v]] - e.t[[cur.v]])))
}

abs.bias.pm <- function(obj) {
  mean(sapply(names(obj@exposures.orig), function(cur.v) {
    sum(abs(obj@exposures.match[[cur.v]][[1]]$total -
            obj@exposures.orig[[cur.v]][[1]]$total))
  }))
}

weight.dan <- function(d, char.var, weight.var, cap.var) {
  cell <- merge(aggregate(list(tweight = d[[weight.var]]), d[char.var], sum),
                aggregate(list(tcap = d[[cap.var]]), d[char.var], sum))
  cell <- merge(cell, aggregate(list(tn = d[[cap.var]]), d[char.var], length))
  cell <- merge(d[c("id", char.var, cap.var, weight.var)], cell)
  cell <- cell[match(d$id, cell$id),]
  cell$tweight * cell[[cap.var]] / cell$tcap
}

portfolio.match2 <- function(d, ..., reps = 10) {
  best.obj <- portfolio.match(d, ..., thresh = 0.00)
  best.bias <- abs.bias.pm(best.obj)
  for (i in 1:reps) {
    cur.obj  <- portfolio.match(d, ..., thresh = 0.01)
    cur.bias <- abs.bias.pm(cur.obj)
    if (cur.bias < best.bias) {
      best.obj  <- cur.obj
      best.bias <- cur.bias
    }
  }
  best.obj
}

factor.collapse <- function(vec, top = 5, other.name = "Other") {
  vec <- paste(vec)
  level.use <- names(sort(table(vec), decreasing = TRUE))
  level.use <- level.use[level.use != "Other"][1:top]
  rst <- factor(replace(paste(vec), !(vec %in% level.use), other.name),
                levels = c(level.use, other.name))
  rst
}

globalcex <- function(pl, mult) {
  for (curn1 in grep("text", names(pl)))
    for (curn2 in grep("cex", names(pl[[curn1]])))
      pl[[curn1]][[curn2]] <- mult * pl[[curn1]][[curn2]]
  pl
}

axis.at.log <- function(vec, at = c(1)) {
  lvec <- log10(vec)
  raw  <- seq(floor(min(lvec)), ceiling(max(lvec)))
  raw  <- as.vector(t(outer(raw, log10(at), "+")))
  raw  <- raw[seq(which(raw >= min(lvec))[1] - 1, which(raw >= max(lvec))[1])]
  raw
}

axis.lab.log <- function(vec, money = TRUE) {
  at <- axis.at.log(vec)
  lab <- formatC(round(10^at), digits = 15)
  lab <- gsub("000000000000$", "T", lab)
  lab <- gsub("000000000$", "B", lab)
  lab <- gsub("000000$", "M", lab)
  lab <- gsub("000$", "K", lab)
  lab <- gsub(" ", "", lab)
  if (money)
    lab <- paste("$", lab, sep = "")
  lab
}

winsorize <- function(vec, min = NULL, max = NULL, eps = 1e-4) {
  if (!is.null(min))
    vec[vec <= min] <- min - eps
  if (!is.null(max))
    vec[vec >= max] <- max + eps
  vec
}

hist.constructor <-
    function(x, breaks,
             include.lowest = TRUE,
             right = TRUE, ...)
{
  breaks <- lf.breaks[[which.max(colSums(sapply(lf, function(vec) x %in% vec)))]]
  if (is.numeric(breaks) && length(breaks) > 1)
    hist(as.numeric(x), breaks = breaks, plot = FALSE,
         include.lowest = include.lowest,
         right = right)
  else
    hist(as.numeric(x), breaks = breaks, plot = FALSE)
}

panel.grouped.histogram <-
    function(x,
             groups = stop("groups must be specified"), subscripts,
             breaks, equal.widths = TRUE, type = "density",
             nint = round(log2(length(x)) + 1),
             alpha = plot.polygon$alpha,
             col = plot.polygon$col,
             border = plot.polygon$border,
             lty = plot.polygon$lty,
             lwd = plot.polygon$lwd, ...)
{
    plot.polygon <- trellis.par.get("superpose.polygon")
    if (length(x) < 1) return()
    if (is.null(breaks)) {
        breaks <- if (is.factor(x))
            seq_len(1 + nlevels(x)) - 0.5
        else if (equal.widths)
            do.breaks(range(x, finite = TRUE), nint)
        else quantile(x, 0:nint/nint, na.rm = TRUE)
    }
    h <- hist.constructor(x, breaks = breaks, ...)
    y <-
        switch(type,
               count = h$counts,
               percent = 100 * h$counts/length(x),
               density = h$intensities)
    breaks <- h$breaks
    stopifnot((nb <- length(breaks)) > 1)

    ## support for groups
    vals <-
        if (is.factor(groups)) levels(groups)
        else sort(unique(groups))
    nvals <- length(vals)
    subg <- groups[subscripts]
    ok <- !is.na(subg)

    alpha <- rep(alpha, nvals)
    col <- rep(col, nvals)
    border <- rep(border, nvals)
    lty <- rep(lty, nvals)
    lwd <- rep(lwd, nvals)

    props.group <-
        sapply(vals,
               function(v) {
                   id <- subg == v
                   hv <- hist.constructor(x[id], breaks = breaks, ...)
                   if (type == "density") hv$intensities
                   else hv$counts
               })
    props.group <- prop.table(props.group, margin = 1)
    y.group <-
        sapply(seq_along(y),
               function(i) y[i] * c(0, cumsum(props.group[i, ])))
    for (i in seq_len(nvals))
    {
        panel.rect(x = breaks[-nb],
                   y = y.group[i, ],
                   height = y.group[i + 1, ] - y.group[i, ],
                   width = diff(breaks),
                   col = col[i], alpha = alpha[i],
                   border = border[i], lty = lty[i],
                   lwd = lwd[i],
                   just = c("left", "bottom"))
    }
}

my.theme <- function(multiple = FALSE, ...) {
  theme <- canonical.theme(color = FALSE)
  theme0 <- simpleTheme(...)
  for (i in names(theme0))
    for (j in names(theme0[[i]]))
      theme[[i]][[j]] <- theme0[[i]][[j]]

  # Background color
  theme$background$col <- "white"

  # Pack in the right and top axes; remove ticks
  theme$axis.components$right$tck <- 0
  theme$axis.components$right$pad1 <- 0
  theme$axis.components$right$pad2 <- 0
  theme$axis.components$top$tck <- 0
  theme$axis.components$top$pad1 <- 0
  theme$axis.components$top$pad2 <- 0

  # Pack in bottom and left axes
  theme$axis.components$bottom$pad1 <- 0.5
  theme$axis.components$bottom$pad2 <- if (multiple) 0.5 else 0
  theme$axis.components$left$pad1 <- 0.5
  theme$axis.components$left$pad2 <- if (multiple) 0.5 else 0

  # Reduce font sizes
  cex <- 0.75
  theme$par.main.text$cex <- cex^0
  theme$par.xlab.text$cex <- cex^1
  theme$par.ylab.text$cex <- cex^1
  theme$add.text$cex      <- cex^1
  theme$axis.text$cex     <- cex^2

  # Remove all extra padding except enough to be able to see 
  theme$layout.widths$left.padding <- 0
  theme$layout.widths$right.padding <- 0
  theme$layout.heights$top.padding <- 0
  theme$layout.heights$bottom.padding <- 0.5

  # Put sensible axis padding
  theme$layout.widths$ylab.padding <- 0
  theme$layout.widths$ylab.axis.padding <- 0.5
  theme$layout.heights$xlab.padding <- 0
  theme$layout.heights$axis.xlab.padding <- 0.5

  # Key padding
  theme$layout.widths$key.ylab.padding <- 0
  theme$layout.heights$main.key.padding <- 0.5
  theme$layout.heights$key.sub.padding <- 0
#  theme$layout.heights$key.axis.padding <- 0
  theme$layout.heights$key.axis.padding <- 0.5
  theme$layout.widths$axis.key.padding <- 0
  theme$layout.heights$xlab.key.padding <- 0
  theme$layout.widths$key.ylab.padding <- 0

  theme
}

rp.hist2 <- function(object, adjust = 0, ...) {
  
  ## Check whether the number of matches is greater than 1.
  stopifnot(ncol(object@weights.match) > 1)

  ## Get returns from the object.
  match.ret <- sapply(object@return.match, function(x) x$total)
  perf.port <- object@return.orig[[1]]$total

  ## Print histogram.
  print(histogram(~ I(match.ret + adjust), nint = 10, type = "count",
                  xlab = "Return on Random Matching Portfolio", ylab = "Count",
                  main = "Distribution of Random Matching Portfolio Returns",
                  par.settings = my.theme(),
                  breaks = seq(.01, .06, by = .005),
                  scales = list(x = list(at = c(.02, .03, .04, .05, .06),
                                  labels = c("2%", "3%", "4%", "5%", "6%"))),
                  panel = function(...) {
                    panel.histogram(...)
                    larrows(x0 = perf.port + 2*0.01/3,
                            x1 = perf.port + 0.01/5,
                            y0 = 16, y1 = 14, length = 0.1, lwd = 1)
                    ltext(x = perf.port + .01, y = 17,
                          labels = "Target Portfolio\n4.5%", adj = c(0.49, 1))
                  }))
}
