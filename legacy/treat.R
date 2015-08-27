###Get the fucntions
source('/media/FD/BIOSCREEN/R/DB_function.R')

### Get the TABLE file and the other tables from the database.
load(file='./DATA/DBs.RData')

# require(mgcv)
# mgcv::gam #(general additive models)
# fit <- gam(Y~s(X))
# require(Hmisc)


check_num <- function(x) {
  
}

check_discr <- qwe

check_fac <- function() {
  
}

test_fac <- function(x,pop) {
  
}

checkOutliers <- function(x) {
  return(error_detect(x,1.5)[['bool']])
}

checkDistrib <- function(x) {
  
}

## Useless because too sensitive to new additions
getEntropy <- function (x, base2=F){
  counts <- table(x)
  p <- counts/sum(counts)
  f <- if (base2) log2 else log
  E <- - sum(p*f(p))
  return(E)
}



### DEMOGRAPHICS -----------------

## Check for outliers
## Check for consistency of the data (more than X% of the data in at least two.)

atcd.ttest <- lapply(atcd[sapply(atcd, is.numeric)], function(x,fac) t.test(x~fac), pop[match(atcd$N.DOSSIER, pop$N.DOSSIER), 'pop1'] )
atcd.pvalues <- sapply(atcd.ttest, function(x) x['p.value'])
names(atcd.pvalues) <- names(atcd.ttest)
atcd.bordernames <- names(atcd.pvalues)[atcd.pvalues<0.15]
# require(ggplot)
# g1.atcd <- ggplot(atcd[atcd.bordernames]) + 
for ( name in atcd.bordernames ) {
  boxplot( atcd[[name]] ~ pop[match(atcd$N.DOSSIER, pop$N.DOSSIER), 'pop1'] )
}

atcd.anova <- lapply(atcd[sapply(atcd, is.numeric)], function(x,fac) aov(x~fac), pop[match(atcd$N.DOSSIER, pop$N.DOSSIER), 'pop1'] )
atcd.pvalues <- sapply(atcd.ttest, function(x) x['p.value'])
names(atcd.pvalues) <- names(atcd.ttest)
atcd.bordernames <- names(atcd.pvalues)[atcd.pvalues<0.15]

atcdfull <- merge(atcd, pop[c('N.DOSSIER', 'pop1','pop2')])




