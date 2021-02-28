#### meng et al 2014 price elasticities

library(data.table)

product <- c("off_beer","off_cider","off_wine","off_spirits","off_rtds",
             "on_beer","on_cider","on_wine","on_spirits","on_rtds")

elasticity <- c(-0.98,-1.27,-0.38,-0.08,-0.59,-0.79,-0.59,-0.87,-0.89,-0.19)

elasticities <- data.table(cbind(product,elasticity))
elasticities <- setDT(elasticities)

elasticities$elasticity <- as.numeric(elasticities$elasticity)


usethis::use_data(elasticities,overwrite=TRUE)

