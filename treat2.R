## Analysis for the promonto Paper (Daphne Lizee)
# Preliminary stuff
#
# Copyright Antoine Lizee @ UCSF the 08/12/2014 antoine.lizee@ucsf.com

source(./analysis_functions.R)

#####  Demographic Analysis  ----------------
summary(pop$N.DOSSIER == atcd$N.DOSSIER)

# lapply(AD[which(TYPES$AD == 'FAC2')], chisq.test, pop$pop2, simulate.p.value=T)
results <- data.frame(t(sapply(AD[which(TYPES$AD == 'FAC2')], chisq.test, pop$pop2, simulate.p.value=T)))[c(1,3,8,9)]
results[results$p.value < 0.1,]

# lapply(AD[which(TYPES$AD == 'FAC')], chisq.test, pop$pop2, simulate.p.value=T)
results <- data.frame(t(sapply(AD[which(TYPES$AD == 'FAC')], chisq.test, pop$pop2, simulate.p.value=T)))[c(1,3,8,9)]
results[results$p.value < 0.1,]

# lapply(AD[which(TYPES$AD == 'DISC')], chisq.test, pop$pop2, simulate.p.value=T)
results <- data.frame(t(sapply(AD[which(TYPES$AD == 'DISC')], chisq.test, pop$pop2, simulate.p.value=T)))[c(1,3,8,9)]
results[results$p.value < 0.1,]


# lapply(AD[which(TYPES$AD == 'DISC')], function(x,fac) { p1 <- x[as.numeric(fac)==1]
#                                                         p2 <- x[as.numeric(fac)==2]
#                                                         wilcox.test(p1,p2)},
#        pop$pop2)

# lapply(AD[which(TYPES$AD == 'DISC')], function(x,fac) { t.test(x~fac)},pop$pop2)
results <- data.frame(t(sapply(AD[which(TYPES$AD == 'DISC')], function(x,fac) { t.test(x~fac)},pop$pop2)))[c(1,3,8,9)]
results[results$p.value < 0.1,]

# lapply(AD[which(TYPES$AD == 'DISC')], function(x,fac) { wilcox.test(x~fac)},pop$pop2)
results <- data.frame(t(sapply(AD[which(TYPES$AD == 'DISC')], function(x,fac) { wilcox.test(x~fac)},pop$pop2)))
results[results$p.value < 0.1,]


# lapply(AD[which(TYPES$AD == 'NUM')], function(x,fac) { t.test(x~fac)},pop$pop2)
results <- data.frame(t(sapply(AD[which(TYPES$AD == 'NUM')], function(x,fac) { t.test(x~fac)},pop$pop2)))[c(1,3,8,9)]
results[results$p.value < 0.1,]

# lapply(AD[which(TYPES$AD == 'NUM')], function(x,fac) { wilcox.test(x~fac)},pop$pop2)
results <- data.frame(t(sapply(AD[which(TYPES$AD == 'NUM')], function(x,fac) { wilcox.test(x~fac)},pop$pop2)))
results[results$p.value < 0.1,]

## PB ?
table(AD$IU.mixte, pop$pop2)
table(AD$TVL, pop$pop2)
table(AD$POPQ.médian..C, pop$pop2)
table(AD$ATCD.MED, pop$pop2)
table(AD$POPQ.médian..C, pop$pop2)

pdf('/home/alizee/Desktop/Daphne.pdf')
ggplot() + geom_histogram(aes(x=as.numeric(AD$IU.mixte), fill=pop$pop2, y=..density..), position='dodge', binwidth = 0.1)
ggplot() + geom_histogram(aes(x=AD$POPQ.médian..C, fill=pop$pop2, y=..density..), position='dodge', binwidth = 0.1)
dev.off()


