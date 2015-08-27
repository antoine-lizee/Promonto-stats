## Analysis for the promonto Paper (Daphne Lizee)
# Full Analysis
#
# Copyright Antoine Lizee @ UCSF the 08/12/2014 antoine.lizee@ucsf.com
# Example Snippet:

# for (debug.b in c(TRUE, FALSE)) {
#   for (transform.b in c(TRUE, FALSE)) {
#     source("Analysis.R") }}



# Prepare the script ------------------------------------------------------

# rm(list=ls())

load("/media/FD/Dropbox/promonto/Pre-Processed//Pre-processed_data.RData")

if (!exists("transform.b"))
  transform.b <- T
if (!exists("debug.b"))
  debug.b <- F
if (!exists("NSimul"))
  NSimul <- 50000

# Analysis ----------------------------------------------------------------

simulate <- function(f, values, pop, folder = "test", inf = F, N=NSimul) {
  pvalue <- f(values, pop)
  permutedDistribution <- replicate(N, f(values, pop[sample(length(pop))]))
  
  if (inf) {
    simulPval <- sum(pvalue > permutedDistribution) / length(permutedDistribution)
  } else {
    simulPval <- sum(pvalue < permutedDistribution) / length(permutedDistribution)
  }
  
  #   plotSimul(t$statistic, simulChiFinal[1,], inf = F, "p-value for the simulated statistic test")
  #   plotSimul(t$p.value, simulChiFinal[2,], inf = T, "p-value for the simulated p-values")
}

analyse <- function(vec, colName, popFac, type) {
  switch(type,
         "DISC" = analyseDISC(vec, colName, popFac),
         "FAC2" = analyseFAC2(vec, colName, popFac),
         "FAC" = analyseFAC2(vec, colName, popFac),
         "NUM" = analyseNUM(vec, colName, popFac),
         "LABEL" = cat("Has been detected as a LABEL (not analysed)\n")
  )
}

outputMeasures <- function(vec, popFac) {
  aggregatedValues <- function(x) { 
    c(median = median(x, na.rm = T), mean = mean(x, na.rm = T), missing = sum(is.na(x)))
  }
  if (debug.b)
    cat("# Aggregated measures:\n")
  print(
    cbind(
      simplify2array(tapply(vec, popFac, aggregatedValues)),
      TOTAL = aggregatedValues(vec)),
    digits = 3) #, higher = F))
}

outputTable <- function(vec, popFac) {
  if(debug.b)
    cat("# Table of values:\n")
  at <- table(popFac, vec, useNA = "ifany")
  names(attributes(at)$dimnames) <- c(NULL, NULL)
  print(rbind(at, TOTAL = table(vec, useNA = "ifany")))
}

analyseChisq <- function(vec, popFac) {
  
  if (length(unique(na.omit(vec))) == 1) {
    cat("# Impossible #")
    return("yeah")
  }
  
  ## Function to compute chi-square
  fchisq <- function(vec, popFac) {
    x <- table(vec,popFac)
    n <- sum(x)
    nr <- as.integer(nrow(x))
    nc <- as.integer(ncol(x))
    if (is.na(nr) || is.na(nc) || is.na(nr * nc))
      stop("invalid nrow(x) or ncol(x)", domain = NA)
    sr <- rowSums(x)
    sc <- colSums(x)
    E <- outer(sr, sc, "*")/n
    return(sum((abs(x - E)^2/E)))
  }
  ## Normal chi-square
  if (debug.b)
    cat("# Attached chi-squared p-value (NON-SIMULATED YET):\n")
  pVal <- chisq.test(vec, popFac, simulate.p.value = T)$p.value
  cat("##", pVal, ifelse(pVal<0.10, " -- ! p < 0.10 !", "")," ## [NORMAL]\n")
  ## Simulated
  if (debug.b)
    cat("# Attached chi-squared SIMULATED p-value :\n")
  pValSimul <- simulate(fchisq, vec, popFac)
  cat("## ", pValSimul, ifelse(pValSimul<0.10, " -- ! p < 0.10 !", "")," ## [SIMULATED]\n")
}

analyseTtest <- function(vec, popFac) {
  ## Function to compute t-test
  fttest <- function(vec, popFac) {
    t <- split(vec, popFac)
    res <- sapply(t, function(v) c(mean(v, na.rm = T), sqrt(var(v, na.rm = T)/length(v))))
    stderr <- sqrt(sum(res[2,]^2))
    return(abs(diff(res[1,]))/stderr)
  }
  ## Normal t-test
  if (debug.b)
    cat("# Attached t-test p-value (NON-SIMULATED YET):\n")
  pVal <- t.test(vec~popFac)$p.value
  cat("##", pVal, ifelse(pVal<0.10, " -- ! p < 0.10 !", "")," ## [NORMAL]\n")
  ## Simulated
  if (debug.b)
    cat("# Attached t-test SIMULATED p-value :\n")
  pValSimul <- simulate(fttest, vec, popFac)
  cat("## ", pValSimul, ifelse(pValSimul<0.10, " -- ! p < 0.10 !", "")," ## [SIMULATED]\n")
  
}

analyseDISC <- function(vec, colName, popFac) {
  if (debug.b)
    cat("Has been detected as a DISCRETE variable...\n")
  else
    cat("DISCRETE\n")
  if (debug.b)
    cat("This is the classical numerical variable analysis:\n")
  outputMeasures(vec, popFac)
  analyseTtest(vec,popFac)
  if (debug.b)
    cat("\nThis is the classical analysis for unordered categories:\n")
  outputTable(vec, popFac)
  analyseChisq(vec, popFac)
  cat("-------------------------\n")
} 

analyseNUM <- function(vec, colName, popFac) {
  if (debug.b)
    cat("Has been detected as a NUMERIC variable...\n")
  else
    cat("NUMERIC\n")
  if (debug.b)
    cat("This is the classical numerical variable analysis:\n")
  outputMeasures(vec, popFac)
  analyseTtest(vec,popFac)
  cat("-------------------------\n")
} 

analyseFAC <- function(vec, colName, popFac) {
  if (debug.b)
    cat("Has been detected as a CATEGORICAL variable...\n")
  else
    cat("CATEGORICAL\n")
  if (debug.b)
    cat("This is the classical analysis for unordered categories:\n")
  outputTable(vec, popFac)
  analyseChisq(vec, popFac)
  cat("-------------------------\n")
} 

analyseFAC2 <- function(vec, colName, popFac) {
  analyseFAC(vec, colName, popFac)
}

fullAnalyse <- function(table, types, popFac) {
  for (colName in colnames(table)) {
    if (colName == "N.DOSSIER") {
      next
    }
    cat("\n### ANALYSIS OF", colName, "---\n")
    analyse(table[[colName]], colName, popFac, types[[colName]])
    #     patternChecks <- sapply(patterns, function(pattern) grepl(pattern, ))
  }
}


# Transform data ----------------------------------------------------------

if (transform.b) {
  transformPOPQ <- function(df) {
    df[[1]] <- factor(df[[1]] >= 2, levels = c(F, T), labels = c("0-1", "2-4"))
    return(df)
  }
  dumb <- sapply(14:16, function(i) {AD[i] <<- transformPOPQ(AD[i])
                                     TYPES$AD[i] <<- "FAC2"})
  dumb <- sapply(3:5, function(i) {RESULTS[i] <<- transformPOPQ(RESULTS[i])
                                   TYPES$RESULTS[i] <<- "FAC2"})
  
  transformThre <- function(df, thre=4000) {
    df[[1]] <- factor(df[[1]] >= thre, levels = c(F, T), labels = paste(c("<", ">="), thre))
    return(df)
  }
  AD["POIDS.PLUS.GROS"] <- transformThre(AD["POIDS.PLUS.GROS"])
  TYPES$AD["POIDS.PLUS.GROS"] <- "FAC2"
  AD$IMC <- cut(AD$IMC, breaks=c(0,25,30,max(AD$IMC)))
  TYPES$AD["IMC"] <- "FAC2"
  
  temp <- factor(AD$NB.AVB)
  levels(temp) <- c("0", "1", "2", "3", ">=4", ">=4", ">=4")
  TYPES$AD["NB.AVB"]
  
}

# Launch the Analysis -----------------------------------------------------

analysisFileName <- paste0("/media/FD/Dropbox/promonto/Analysis", 
                           ifelse(transform.b, "", "_raw"), 
                           ifelse(debug.b, "_verbose", ""),
                           ".txt")
cat("\n## Launching analysi to output file", analysisFileName, "\n")
sink(analysisFileName, append=FALSE, split = T)
cat('Fichier de sortie pour les analyses "promonto"\n')
cat("Date of creation:", Sys.time(), "\n")
cat("NSimul: ", NSimul, "\n")


cat("\n\n#### Launching analysis for the Demographic Analysis ####")
fullAnalyse(AD, TYPES$AD, pop$pop2)  

cat("\n\n#### Launching analysis for the Outcomes ####")
fullAnalyse(RESULTS, TYPES$RESULTS, pop$pop2)  

sink()
