### R code from vignette source 'matching.Rnw'

###################################################
### code chunk number 1: matching.Rnw:42-48
###################################################
op <- options(width = 80, digits = 2, scipen = 5)

require(MatchingPortfolios)
trellis.par.set(canonical.theme(color = FALSE))

set.seed(0)


###################################################
### code chunk number 2: matching.Rnw:345-354
###################################################
data(starmine)
s1.all <- subset(starmine, date == "1995-01-31" & !is.na(cap.usd) &
                 !is.na(sector) & !is.na(country) & !is.na(fwd.ret.1m))
s1.all$sector  <- factor(s1.all$sector)
s1.all$country <- factor(s1.all$country)
s1.all$cap.usd.bucket <- cut(s1.all$cap.usd, c(0, 250e6, 2e9, 10e9, Inf),
                             labels = c("Micro", "Small", "Mid", "Large"))
s1.all$weight.uni <- scale1(rep(1, nrow(s1.all)))
s1.all <- s1.all[order(s1.all$cap.usd, decreasing = TRUE),]


###################################################
### code chunk number 3: matching.Rnw:380-390
###################################################
s1.out <- s1.all[sample(nrow(s1.all), 6),]
rownames(s1.out) <- s1.out$name
s1.out$cap.usd   <- round(s1.out$cap.usd / 1e6)
s1.out$smi[is.na(s1.out$smi)] <- "NA"
xtable(structure(s1.out[c("country", "sector", "cap.usd", "smi", "fwd.ret.1m")],
                 names  = c("Coun.", "Sector", "Cap. (M)", "SMI",
                   "1-mo. Ret.")),
       digits  = c(NA, NA, NA, 1, 0, 3),
       caption = "Sample records from the \\texttt{starmine} data.",
       label   = "TableStarMine")


###################################################
### code chunk number 4: matching.Rnw:396-420
###################################################

require(lattice)
s1.summ.lf <- list()
s1.summ.lf$str <-
  rbind(data.frame(category = "Sector",  table(s1.all$sector)),
        data.frame(category = "Country", table(s1.all$country)))
s1.summ.lf$num <-
  rbind(data.frame(category = "Return",     val = s1.all$fwd.ret.1m),
        data.frame(category = "Score",      val = s1.all$smi),
        data.frame(category = "Log10(Market Cap)", val = log10(s1.all$cap.usd)))
         
s1.summ.lf$l1 <-
  barchart(Var1 ~ Freq | category, s1.summ.lf$str,
           xlab = "Number of Stocks",
           scales = list(relation = "free"),
           main = "Characteristics of Universe Stocks")
s1.summ.lf$l2 <-
  histogram(~ val | category, s1.summ.lf$num, breaks = NULL, as.table = TRUE,
            type = "count", ylab = "Number of Stocks",
            xlab = "Value",
            scales = list(relation = "free"))
print(s1.summ.lf$l1, split = c(1, 1, 1, 2), more = TRUE)
print(s1.summ.lf$l2, split = c(1, 2, 1, 2))



###################################################
### code chunk number 5: matching.Rnw:430-453
###################################################
# Define the universe
s1 <- subset(s1.all, !is.na(smi))
# Break down to nice number of rows
n.uni <- round(nrow(s1) - 10^2, -2)
s1 <- s1[1:n.uni,]

# Weights for SMI portfolio
smi.new       <- s1$smi +
  (s1$sector %in% c("Manuf", "NoDur")) * -10 +
  (s1$country %in% c("USA", "JPN")) * -5 +
  (s1$cap.usd.bucket %in% "Micro-cap (0-250M)") * -100
n.port        <- 100
s1$weight.smi <-
  scale1(replace(rep(0, nrow(s1)),
                 order(smi.new, decreasing = TRUE)[1:n.port], 1))

# Weights for equal-weighted universe portfolio
s1$weight.uni <- scale1(rep(1, nrow(s1)))

# Performance
perf.port <- sum(s1$fwd.ret.1m * s1$weight.smi)
perf.uni  <- sum(s1$fwd.ret.1m * s1$weight.uni)
perf.uni.all <- sum(s1.all$fwd.ret.1m * s1.all$weight.uni)


###################################################
### code chunk number 6: matching.Rnw:470-491
###################################################

p.char   <- c("sector", "country", "cap.usd.bucket")
p.char.n <- c("Sector", "Country", "Cap")

p.exp.lf <-
  do.call("rbind", sapply(seq(along = p.char), function(i) {
    rbind(data.frame(por = "Portfolio",
                     var = p.char.n[i],
                     lev = names(tapply(s1$weight.smi, s1[[p.char[i]]], sum)),
                     wei = tapply(s1$weight.smi, s1[[p.char[i]]], sum)),
          data.frame(por = "Universe",
                     var = p.char.n[i],
                     lev = names(tapply(s1$weight.uni, s1[[p.char[i]]], sum)),
                     wei = tapply(s1$weight.uni, s1[[p.char[i]]], sum)))
  }, simplify = FALSE))

print(barchart(lev ~ wei | var, p.exp.lf, groups = por, as.table = TRUE,
               xlab = "Weight",
               scales = list(relation = "free"), auto.key = TRUE,
               main = "Exposures of Portfolio, Universe"))



###################################################
### code chunk number 7: matching.Rnw:509-519
###################################################

p.perf.lf <-
  structure(matrix(c(0.036, perf.port, perf.uni, perf.uni.all) * 100),
            dimnames = list(c("S&P 500", "Portfolio", "Rated Universe",
              "Complete Universe"),
              c("One-month Forward Return (%)")))

print(barchart(p.perf.lf, xlab = "One-month Forward Return (%)",
               main = "Forward Returns by Portfolio Type"))



###################################################
### code chunk number 8: matching.Rnw:572-583
###################################################

country.use <- names(sort(table(s1$country), decreasing = TRUE)[1:4])
print(stripplot(sector ~ cap.usd | country,
                subset(s1, country %in% country.use),
                jitter = TRUE, col = c("grey", "black"), pch = 20,
                scales =  list(x = list(log = TRUE, at = 10^(8:11),
                                 labels = c("$100M", "$1B", "$10B", "$100B"))),
                xlab = "Market Cap in USD", ylab = "Sector",
                main = "Universe: Holdings and Non-holdings",
                groups = weight.smi > 0))



###################################################
### code chunk number 9: matching.Rnw:656-673
###################################################
# Fit the propensity score
prop.glm <- glm(weight.smi > 0 ~ country + sector + cap.usd.bucket, s1,
                family = quasibinomial)

# Stupid gaming of the summary output to allow renaming variables
prop.glm.s <- summary(prop.glm)$coef
rownames(prop.glm.s) <-
  gsub("(country|sector|cap.usd.bucket)(.*)", "1(\\1 = \\2)",
       rownames(prop.glm.s))
rownames(prop.glm.s) <-
  gsub("\\-cap \\(.*?\\)", "", gsub("cap.usd.bucket", "cap",
                                    rownames(prop.glm.s)), perl = TRUE)

xtable:::xtable.summary.lm(list(coef =
                                prop.glm.s[prop.glm.s[,"Std. Error"] < 1,]),
                           label = "TablePropensityScore",
                           caption = "Coefficients from the fitted logistic regression for propensity score using indicator variables from the StarMine portfolio.")


###################################################
### code chunk number 10: matching.Rnw:719-723
###################################################
# Matching using propensity score
rr.port <- as.vector(which(prop.glm$y))
rr.prop <- prop.match(prop.glm, 0)
expo.plot.all(s1, "weight.smi", ind1 = rr.port, ind2 = rr.prop)


###################################################
### code chunk number 11: matching.Rnw:735-737
###################################################
# Matching portfolio performance
perf.match <- sum(s1$fwd.ret.1m[rr.prop] * s1$weight.smi[rr.port])


###################################################
### code chunk number 12: matching.Rnw:790-793
###################################################
# Generate random portfolios
rp.1  <- sapply(seq(1, 100), function(i) prop.match(prop.glm, .005))
rp.10 <- sapply(seq(1, 100), function(i) prop.match(prop.glm, .100))


###################################################
### code chunk number 13: matching.Rnw:850-856
###################################################
opar <- par(mfrow = c(2, 1), mar = c(2, 2, 2, 0))
rp.plot(rp.1, rr.port, s1, "sector", "weight.smi",
        main = "Sector Exposures of Random Portfolios, Thresh = .005")
rp.plot(rp.10, rr.port, s1, "sector", "weight.smi",
        main = "Sector Exposures of Random Portfolios, Thresh = .05")
par(opar)


###################################################
### code chunk number 14: matching.Rnw:874-876
###################################################
# Calculate returns on random portfolios
rp.ret <- rp.return(rp.1, rr.port, s1, "fwd.ret.1m", "weight.smi")


###################################################
### code chunk number 15: matching.Rnw:890-902
###################################################

print(histogram(~ rp.ret, nint = 12, type = "count",
                xlab = "Matched Portfolio Return", ylab = "Number",
                main = "Distribution of Random Portfolios' Returns",
                panel = function(...) {
                  panel.histogram(...)
                  llines(x = rep(perf.port, 2), y = c(-100, 100),
                         lwd = 2)
                  ltext(x = perf.port, y = 15,
                        labels = "Original Portfolio", adj = c(0.49, 1))
                }))



###################################################
### code chunk number 16: matching.Rnw:933-936
###################################################
s1$weight.smi.ls <- s1$weight.smi * 1.0
s1$weight.smi.ls[order(smi.new)[1:n.port]] <- -.75 / n.port
perf.ls <- sum(s1$weight.smi.ls * s1$fwd.ret.1m)


###################################################
### code chunk number 17: matching.Rnw:949-953
###################################################
prop.lm <- lm(weight.smi.ls ~ country + sector + cap.usd.bucket, s1)
rr.ls   <- which(s1$weight.smi.ls != 0)
rr.ls.m <- prop.match(prop.lm, 0)
perf.ls.m <- sum(s1$weight.smi.ls[rr.ls] * s1$fwd.ret.1m[rr.ls.m])


###################################################
### code chunk number 18: matching.Rnw:1020-1021
###################################################
expo.plot.all(s1, "weight.smi.ls", ind1 = rr.ls, ind2 = rr.ls.m, style = "ls")


###################################################
### code chunk number 19: matching.Rnw:1039-1041
###################################################
rp.ls <- sapply(seq(1, 100), function(i) prop.match(prop.lm, .0001))
rp.ls.ret <- rp.return(rp.ls, rr.ls, s1, "fwd.ret.1m", "weight.smi.ls")


###################################################
### code chunk number 20: matching.Rnw:1053-1066
###################################################

print(histogram(~ rp.ls.ret, nint = 12, type = "count",
                xlab = "Matched Portfolio Return", ylab = "Number",
                main = "Distribution of Random Long-short Portfolios' Returns",
                panel = function(...) {
                  panel.histogram(...)
                  llines(x = rep(perf.ls, 2), y = c(-100, 100),
                         lwd = 2)
                  ltext(x = perf.ls, y = 15,
                        labels = "Original L-S Portfolio",
                        adj = c(0.49, 1))
                }))



###################################################
### code chunk number 21: matching.Rnw:1081-1094
###################################################

p.perf.lf2 <-
  structure(matrix(c(perf.port, 0.036, perf.uni,
                     mean(rp.ret), perf.ls, mean(rp.ls.ret)) * 100),
            dimnames = list(c("Portfolio", "S&P 500", "Universe",
              "Matching Portfolio Average",
              "Long-short Portfolio", "Matching Long-short Average"),
              c("One-month Forward Return (%)")))
p.perf.lf2 <- p.perf.lf2[seq(nrow(p.perf.lf2), 1),,drop=FALSE]

print(barchart(p.perf.lf2, xlab = "One-month Forward Return (%)",
               main = "Forward Return of All Portfolios"))



###################################################
### code chunk number 22: matching.Rnw:1215-1217
###################################################
print(histogram(~ cap.usd + log(cap.usd), s1,
                scales = list(relation = "free"), breaks = NULL))


###################################################
### code chunk number 23: matching.Rnw:1325-1326
###################################################
options(op)


