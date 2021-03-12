load("~/Downloads/mediation RData/mediation-president.RData")
# load("~/Downloads/mediation RData/mediation-senate.RData")

ade_sum <- 0
signif_count <- 0

for (result in rslt) {
  if (result[[3]][["z0.p"]] <= 0.05) {
    ade_sum <- ade_sum + print(result[[3]][["z0"]])
    signif_count <- signif_count + 1
  }
}

ade_sum / length(rslt)
signif_count


acme_sum <- 0
signif_count <- 0

for (result in rslt) {
  if (result[[3]][["d0.p"]] <= 0.05 & result[[3]][["z0.p"]] <= 0.05) {
    acme_sum <- acme_sum + print(result[[3]][["d0"]])
    signif_count <- signif_count + 1
  }
}

acme_sum / length(rslt)
signif_count
