#' Select Scenario Parameters
#'
#' Choose the scenario from the pre-loaded data frame of scenarios in `tobalciomodel::scenarios`
#' and extract the relevant parameters to put into the IO model.
#'
#' @param num.scenario the row number of the data frame `tobalciomodel::scenarios` corresponding to the chosen scenario to model.
#'
#' @export
select_scenario <- function(num.scenario = NULL) {

  data <- tobalciomodel::scenarios[num.scenario,]

  year <-        as.vector(as.matrix(data[,"year"]))
  scotland <-    as.logical(data[,"scotland"])
  elasticity <-  as.character(data[,"elasticity"])
  emp.measure <- as.character(data[,"emp.measure"])
  alc.policy <-  as.vector(as.matrix(ifelse(is.na(data[,"alc.policy"]),NA,as.character(data[,"alc.policy"])) ))
  if (alc.policy == "NA") {
    alc.policy <- NULL
  }
  tob.policy <-  as.vector(as.matrix(ifelse(is.na(data[,"tob.policy"]),NA,as.character(data[,"tob.policy"])) ))
  if (is.na(tob.policy)) {
    tob.policy <- NULL
  }

  #### alcohol specific policy parameters
  alc.mup <- c(data[,"alc.mup"])
  alc.tax <- as.vector(as.matrix(c(
               as.numeric(data[,"beer.tax"]),
               as.numeric(data[,"cider.tax"]),
               as.numeric(data[,"wine.tax"]),
               as.numeric(data[,"spirit.tax"]),
               as.numeric(data[,"rtd.tax"])
               )))
  alc.on  <- as.vector(as.matrix(c(
    as.numeric(data[,"on.beer.ex"]),
    as.numeric(data[,"on.cider.ex"]),
    as.numeric(data[,"on.wine.ex"]),
    as.numeric(data[,"on.spirit.ex"]),
    as.numeric(data[,"on.rtd.ex"])
    )))
  alc.off <- as.vector(as.matrix(c(
    as.numeric(data[,"off.beer.ex"]),
    as.numeric(data[,"off.cider.ex"]),
    as.numeric(data[,"off.wine.ex"]),
    as.numeric(data[,"off.spirit.ex"]),
    as.numeric(data[,"off.rtd.ex"])
  )))
  #### tobacco specific policy parameters
  tob.mup <- c(data[,"tob.mup"])


  return(list(year = year,
              scotland = scotland,
              elasticity = elasticity,
              emp.measure = emp.measure,
              alc.policy = alc.policy,
              alc.mup = alc.mup,
              alc.tax = alc.tax,
              alc.on.ex = alc.on,
              alc.off.ex = alc.off,
              tob.policy = tob.policy,
              tob.mup = tob.mup))
}
