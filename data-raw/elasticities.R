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


elasticities_alc <- data.frame(rbind(elasticities_meng14,elasticities_collis10))

elasticities_alc$On.Trade  <- as.numeric(as.character(elasticities_alc$On.Trade))
elasticities_alc$Off.Trade <- as.numeric(as.character(elasticities_alc$Off.Trade))

usethis::use_data(elasticities_alc,overwrite=TRUE)

######## TOBACCO ELASTICITIES

#Gallus S, Schiaffino A, La Vecchia C, et al
#Price and cigarette consumption in Europe
#Tobacco Control 2006;15:114-119.

gallus06 <- -0.46

## An HMRC study - select their preferred model
## http://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.307.8455&rep=rep1&type=pdf

czubeck10 <- -1.05

elasticity <- c(gallus06,czubeck10)
product <- c("Cigarettes","Cigarettes")
source <- c("Gallus et al. 2006","Czubeck & Johal 2010")

elasticities_tob <- matrix(c(product,elasticity,source),
                                ncol=3,
                                byrow=FALSE,
                                dimnames = list(NULL,
                                                c("product","Elasticity","Source"))
)
elasticities_tob <- data.frame(elasticities_tob)
elasticities_tob$Elasticity <- as.numeric(as.character(elasticities_tob$Elasticity))

usethis::use_data(elasticities_tob,overwrite=TRUE)
