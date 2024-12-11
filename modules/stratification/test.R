source("modules/stratification/new.R")

strat_layers <- list(
  layer1 <- list(
    name = "A",
    col = rep(c("1", "2", "3", "2"), 4000),
    ratios = list("1" = 0.3, "2" = 0.6, "3" = 0.1)
  ),
  layer2 <- list(
    name = "B",
    col = rep(c("B", "C", "B", "B"), 4000),
    ratios = list("B" = 0.3, "C" = 0.7)
  )
)

data <- data.frame(lapply(strat_layers, function(l) l$col))
cat_counts <- lapply(strat_layers, function(l) table(l$col))
ratios <- lapply(strat_layers, function(l) l$ratios)
names <- lapply(strat_layers, function(l) l$name)
colnames(data) <- names
names(cat_counts) <- names
names(ratios) <- names

o <- strata_sizes(data, ratios, cat_counts, 1, 50)
print(o)
