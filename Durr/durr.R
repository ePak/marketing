# setwd("~/MBusA/Module 4/Marketing/week 2")
library(data.table)

durr <- fread("./Durr for SAS updated.csv", sep=",")

competitors <- c("WasteWatch", "Thermatrix", "AdvancedAir")
ours <- c("Servair", "Premier", "Base")

products <- data.table(
  efficiency = c("C12_5%plus", "C11_9%plus", "C13_0%plus", "C13_0%plus", "C11_9%plus", "C12_5%plus"),
  delivery.time = c("C16_9monthsDel", "C17_12monthsDel", "C16_9monthsDel", "C18_15monthsDel", "C17_12monthsDel", "C17_12monthsDel"),
  price = c("C19_$600000", "C23_$900000", "C19_$600000", "C23_$900000", "C23_$900000", "C21_$700000"),
  delivery.term = c("C27_FOBServiceContract", "C27_FOBServiceContract", "C26_InstalledServContract", "C24_Installed2yrWarr", "C26_InstalledServContract", "C26_InstalledServContract"),
  name = c(competitors, ours)
  )
setkey(products, name)

# products2 <- data.table(
#   WasteWatch = c("C12_5%plus", "C16_9monthsDel", "C19_$600000", "C27_FOBServiceContract"),
#   Thermatrix = c("C11_9%plus", "C17_12monthsDel", "C23_$900000", "C27_FOBServiceContract"),
#   AdvancedAir = c("C13_0%plus", "C16_9monthsDel", "C19_$600000", "C26_InstalledServContract"),
#   Servair = c("C13_0%plus", "C18_15monthsDel", "C23_$900000", "C24_Installed2yrWarr"),
#   Premier = c("C11_9%plus", "C17_12monthsDel", "C23_$900000", "C26_InstalledServContract"),
#   Base = c("C12_5%plus", "C17_12monthsDel", "C21_$700000", "C26_InstalledServContract")
# )

product.utilities <- durr[, "Customer", with=F]
for (name in products$name) {
  product.utilities[, (name):=durr[, rowSums(.SD), .SDcols = unlist(products[name, !"name", with=F])]]
}
product.utilities

first.choice <- function(utilities, columns) {
  columns <- as.matrix(columns)
  max.utils <- apply(utilities[, columns, with=F], 1, max)
  choices <- as.data.table(utilities[, (.SD == max.utils) + 0, .SDcols = columns])
  choices[, .SD/apply(.SD, 1, sum), .SDcols = columns]
}
product.choice <- function(func, utilities, competitors, ours, n = 1, weights = NULL) {
  k <- length(ours)
  if (n < 0 | n > k) stop("n must be between 0 and length(ours)")
  
  if (is.null(weights)) weights <- rep(1, nrow(utilities))
  if (n > 0) {
    counts <- apply(gtools::combinations(k, n, ours), 1, function(row) {
      func(utilities, c(competitors, row))
    })
  } else {
    counts <- list(func(utilities, competitors))
  }
  
  lapply(counts, function(col) col * weights)
}
market.shares <- function(func, utilities, competitors, ours, n = 1, weights = NULL) {
  counts <- product.choice(func, utilities, competitors, ours, n, weights)
  
  lapply(counts, function(row) {
    cs <- colSums(row)
    tmp <- cs/sum(cs)
    if (n > 1) {
      name <- paste0('sum(',paste(tail(names(tmp),n), collapse = ','),')')
      tmp[name] <- sum(tail(tmp,n)) 
    }
    tmp
  })
}

market.shares(first.choice, product.utilities, competitors, ours, 0, durr$Weights)
market.shares(first.choice, product.utilities, competitors, ours, 1, durr$Weights)
market.shares(first.choice, product.utilities, competitors, ours, 2, durr$Weights)

# investigate who changes their choice
choice.single <- product.choice(first.choice, product.utilities, competitors, c('Premier', 'Servair'))
choice.double <- product.choice(first.choice, product.utilities, competitors, c('Premier', 'Servair'), 2)
shifters.Premier <- choice.single[[1]]$Premier - choice.double[[1]]$Premier
names(shifters.Premier) <- durr$Customer
shifters.Premier[shifters.Premier!=0]

# segmentation

seg.cols <- grep('^C[0-9]', names(durr))
clusters <- hclust(dist(durr[, seg.cols, with=F]), method="ward.D2")

disc.cols <- grep('^D[0-9]', names(durr))
clusters.disc <- hclust(dist(durr[, disc.cols, with=F]), method="ward.D2")
for (i in 2) {
  clusterCut <- cutree(clusters, i)
  clusterCut.disc <- cutree(clusters.disc, i)
  print(paste(i, 'clusters accuracy:', sum(clusterCut == clusterCut.disc)/length(clusterCut)))
}

market.shares(first.choice, product.utilities[clusterCut==1,], competitors, ours, 1, durr$Weights)
market.shares(first.choice, product.utilities[clusterCut==2,], competitors, ours, 1, durr$Weights)
