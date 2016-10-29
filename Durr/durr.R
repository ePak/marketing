setwd("~/MBusA/Module 4/Marketing/week 2")
library(data.table)

durr <- fread("./Durr for SAS updated.csv", sep=",")

competitors <- c("WasteWatch", "Thermatrix", "AdvancedAir")
ours <- c("Servair", "Premier", "Base")
products <- data.frame(
  efficiency = c("C12_5%plus", "C11_9%plus", "C13_0%plus", "C13_0%plus", "C11_9%plus", "C12_5%plus"),
  delivery.time = c("C16_9monthsDel", "C17_12monthsDel", "C16_9monthsDel", "C18_15monthsDel", "C17_12monthsDel", "C17_12monthsDel"),
  price = c("C19_$600000", "C23_$900000", "C19_$600000", "C23_$900000", "C23_$900000", "C21_$700000"),
  delivery.term = c("C27_FOBServiceContract", "C27_FOBServiceContract", "C26_InstalledServContract", "C24_Installed2yrWarr", "C26_InstalledServContract", "C26_InstalledServContract"),
  row.names = c(competitors, ours)
  )

product.utilities <- durr[, "Customer", with=F]
for (name in row.names(products)) {
  product.utilities[, (name):=durr[, rowSums(.SD), .SDcols = as.matrix(products[name,])]]
}


first.choice <- function(utilities, columns) {
  columns <- as.matrix(columns)
  max.utils <- apply(utilities[, columns, with=F], 1, max)
  choices <- as.data.table(utilities[, (.SD == max.utils) + 0, .SDcols = columns])
  choices[, .SD/apply(.SD, 1, sum), .SDcols = columns]
}

single1 <- first.choice(product.utilities, c(competitors, "Servair"))
colSums(single1)

single2 <- first.choice(product.utilities, c(competitors, "Premier"))
colSums(single2)

single3 <- first.choice(product.utilities, c(competitors, "Base"))
colSums(single3)

singles <- lapply(ours, function(name) first.choice(product.utilities, c(competitors, name)))
lapply(singles, colSums)
lapply(singles, function(choices) {
  cs <- colSums(choices)
  cs/sum(cs)
})

library(gtools)
doubles <- apply(combinations(3, 2, ours), 1, function(row) first.choice(product.utilities, c(competitors, row)))
lapply(doubles, colSums)
lapply(doubles, function(choices) {
  cs <- colSums(choices)
  tmp <- cs/sum(cs)
  print(str(tmp))
  # sapply(tmp, function(x) {print(x)})
  for(i in 1:length(tmp)) {
    print(sum(tmp[[i]]))
  }
})
