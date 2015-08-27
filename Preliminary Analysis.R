
# Quick analysis of the important outcomes --------------------------------
POPQ <- RESULTS$POPQ.POST.PO
label <- fullRESULTS$pop2

(t1 <- chisq.test(x = POPQ, label, simulate.p.value = T, B = 10000))
(t2 <- chisq.test(x = POPQ, label, simulate.p.value = F))

simulChi <- replicate(10000, chisq.test(POPQ, label[sample(length(label))])[c("statistic", "p.value")])
simulChiFinal <- matrix(unlist(simulChi), nr =2)

plotSimul <- function(value, permutedDistribution, inf = T, main = "Simulated p-value") {
  hist(permutedDistribution,100, main = main)
  abline(v=value, lwd = 4, col = "red")
  if (inf) {
    simulPval <- sum(value > permutedDistribution) / length(permutedDistribution)
  } else {
    simulPval <- sum(value < permutedDistribution) / length(permutedDistribution)
  }
  box <- par("usr")
  text(x = mean(box[1:2]), y = mean(box[3:4]), simulPval, cex = 5)
}

plotSimul(t2$statistic, simulChiFinal[1,], inf = F, "p-value for the simulated statistic test")
plotSimul(t2$p.value, simulChiFinal[2,], inf = T, "p-value for the simulated p-values")

RECTO <- RESULTS$Rectocele.PO

(t1 <- chisq.test(x = RECTO, label, simulate.p.value = T, B = 10000))
(t2 <- chisq.test(x = RECTO, label, simulate.p.value = F))

simulChi <- replicate(10000, chisq.test(RECTO, label[sample(length(label))])[c("statistic", "p.value")])
simulChiFinal <- matrix(unlist(simulChi), nr =2)

plotSimul(t2$statistic, simulChiFinal[1,], inf = F, "p-value for the simulated statistic test")
plotSimul(t2$p.value, simulChiFinal[2,], inf = T, "p-value for the simulated p-values")

