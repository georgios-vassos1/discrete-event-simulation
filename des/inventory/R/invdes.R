rm(list=ls())
setwd("~/solutions/des/inventory/R")
source("utils.R")
# Choose configuration
source("conf-4.R")

## Inventory system
nruns   <- 10L
inv.sys <- new.env()
input.conf(inv.sys)
result  <- matrix(0.0, nrow = nruns*inv.sys$num_policies, ncol = 3)
monthly.cost <- matrix(0.0, nrow = inv.sys$num_policies, ncol = inv.sys$num_months)
# Policy (s,S) or (small_s,big_s) index
for (irun in seq(nruns)) {
  print(irun)
  for (i in seq(inv.sys$num_policies)) {
    init.env(inv.sys)
    while (1) {
      # Determine the next event
      timing(inv.sys)
      # Update statistical counters
      update_stats(inv.sys)
      # Invoke the appropriate event routine
      j <- inv.sys$next_event_type
      switch (j,
        order_arrival(inv.sys),
        demand(inv.sys),
        break,
        evaluate.d(inv.sys, i=i)
      )
    }
    idx <- (irun-1)*inv.sys$num_policies + i
    result[idx,] <- c(inv.sys$total_ordering_cost,
                      inv.sys$area_holding[2]  * inv.sys$holding_cost,
                      inv.sys$area_shortage[2] * inv.sys$shortage_cost) / inv.sys$num_months
    monthly.cost[i,] <- monthly.cost[i,] + inv.sys$total_month_cost
  }
}
monthly.cost <- monthly.cost / nruns

## Plot 
matplot(t(monthly.cost), type='l')
## Summarise results
m  <- inv.sys$num_policies
DT <- data.table::data.table(idx=rep(seq(m),nruns),result)[,lapply(.SD,mean),idx]
DT[,avg_total_cost:=Reduce(`+`,.SD),.SDcols=c("V1","V2","V3")]
DT
## Contour plot (conf-4) only
opt.data <- as.matrix(unname(reshape2::dcast(data.frame(inv.sys$opt.grid,X3=DT$avg_total_cost),X1~X2)[,-1]))
contour(opt.data, col = hcl.colors(10, "YlOrRd"))
## PLot (conf-2) only
# matplot(cbind(t(monthly.cost),112.11), type='l')
