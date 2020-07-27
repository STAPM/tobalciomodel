### create dataset of elasticities
elasticity_ontrade_meng14 <- c(-0.79,-0.59,-0.87,-0.89,-0.19)
elasticity_offtrade_meng14 <- c(-0.98,-1.27,-0.38,-0.08,-0.59)

elasticities_meng14 <- matrix(c("Beer","Cider","Wine","Spirits","RTDs",
                                elasticity_ontrade_meng14,
                                elasticity_offtrade_meng14,
                                rep("Meng et al. 2014",5)),
                              ncol=4,
                              byrow=FALSE,
                              dimnames = list(NULL,
                                              c("product","On-Trade","Off-Trade","Source"))
)

elasticity_ontrade_collis10 <- c(-0.77,-0.85,-0.46,-1.15,-0.91)
elasticity_offtrade_collis10 <- c(-1.10,-1.13,-0.54,-0.9,-0.93)

elasticities_collis10 <- matrix(c("Beer","Cider","Wine","Spirits","RTDs",
                                elasticity_ontrade_collis10,
                                elasticity_offtrade_collis10,
                                rep("Collis et al. 2010",5)),
                              ncol=4,
                              byrow=FALSE,
                              dimnames = list(NULL,
                                              c("product","On-Trade","Off-Trade","Source"))
)


elasticities <- data.frame(rbind(elasticities_meng14,elasticities_collis10))

elasticities$On.Trade  <- as.numeric(as.character(elasticities$On.Trade))
elasticities$Off.Trade <- as.numeric(as.character(elasticities$Off.Trade))

usethis::use_data(elasticities,overwrite=TRUE)
