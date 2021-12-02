#' Generate Model Outputs
#'
#' Take the output of the model and generate summary outputs
#'
#' @param data data table. Output of the input-output modelling
#' @param macro data table. Time series data of output, gva, and employment by broad industry group.
#' @param sort character. Variable on which to sort sectors by effect size. Inputs are one from
#' c("output","empl","gva","inctaxes","earn")
#' @param year Integer. Select the base year for the analysis.
#' @param FAI Logical. If TRUE, uses the Fraser of Allender Institute (FAI) table. If FALSE, uses the
#'            ONS supply and use tables. Defaults to FALSE
#'
#' @return
#' @export
#'
#' @examples
#'
#' \dontrun{
#'
#' }
ProcessOutputs <- function(data,
                           macro = tobalciomodel::macro_data,
                           sort = NULL,
                           year = 2019,
                           FAI = FALSE) {

  output <- copy(data)

  ### Assign broad industry grouping to the individual sectors according to SIC-2007

  if (isTRUE(FAI)) {
    output[1:3   , Industry := "Agriculture"]
    output[4:57  , Industry := "Production"]
    output[58    , Industry := "Construction"]
    output[59:71 , Industry := "Distribution, transport, hotels and restaurants"]
    output[72:76 , Industry := "Information and communication"]
    output[77:79 , Industry := "Finance and insurance"]
    output[80:81 , Industry := "Real estate"]
    output[82:95 , Industry := "Professional and support activities"]
    output[96:99 , Industry := "Government, education and health"]
    output[100:106 , Industry := "Other services"]

  } else if (!isTRUE(FAI)) {
    output[1:3   , Industry := "Agriculture"]
    output[4:56  , Industry := "Production"]
    output[57    , Industry := "Construction"]
    output[58:69 , Industry := "Distribution, transport, hotels and restaurants"]
    output[70:73 , Industry := "Information and communication"]
    output[74:76 , Industry := "Finance and insurance"]
    output[77:79 , Industry := "Real estate"]
    output[80:93 , Industry := "Professional and support activities"]
    output[94:97 , Industry := "Government, education and health"]
    output[98:105 , Industry := "Other services"]
  }

  ####################################################
  ## INDUSTRY LEVEL FIGURES - total effects only #####

  output[, Industry := factor(Industry,
                              levels = c("Agriculture", "Production", "Construction",
                                         "Distribution, transport, hotels and restaurants",
                                         "Information and communication", "Finance and insurance",
                                         "Real estate", "Professional and support activities",
                                         "Government, education and health", "Other services"))]

  output_ind <- output[, .(out_1 = sum(out_effects_t1_p),
                           gva_1 = sum(gva_effects_t1_p),
                           emp_1 = sum(emp_effects_t1_p),
                           net_1 = sum(netearn_effects_t1_p),
                           tax_1 = sum(emptax_effects_t1_p)), by = "Industry"]

  ### Calculate percentage effects

  y <- copy(year)
  macro <- macro[year %in% y,]

  output_ind_perc <- merge(output_ind, macro, by = "Industry", sort = F)

  output_ind_perc[, output_perc := out_1/output]
  output_ind_perc[, gva_perc := gva_1/gva]
  output_ind_perc[, emp_perc := emp_1/tot_fte]

  output_ind_perc <- output_ind_perc[, c("Industry","output_perc","gva_perc","emp_perc")]

  #########

  industry <- merge(output_ind, output_ind_perc, by = "Industry", sort = F)
  industry <- industry[,c("Industry", "out_1","output_perc", "gva_1","gva_perc", "emp_1","emp_perc")]
  industry[, out_1 := round(out_1, 3)]
  industry[, gva_1 := round(gva_1, 3)]
  industry[, emp_1 := round(emp_1, 0)]
  industry[, output_perc := output_perc]
  industry[, gva_perc    := gva_perc]
  industry[, emp_perc    := emp_perc]

  setnames(industry, names(industry), c("Industry", "out","out_perc", "gva","gva_perc", "emp","emp_perc"))


  ##############################################################################
  ## AGGREGATE FIGURES - split by direct, indirect, and total effects ##########

  ## generate total type 0 and type 1 effects for output, GVA, employment,
  ## earnings, and income tax on earnings

  out_0 <- sum(output$out_effects_t0_p)
  out_1 <- sum(output$out_effects_t1_p) - out_0
  out_t <- out_0 + out_1

  gva_0 <- sum(output$gva_effects_t0_p)
  gva_1 <- sum(output$gva_effects_t1_p) - gva_0
  gva_t <- gva_0 + gva_1

  emp_0 <- round(sum(output$emp_effects_t0_p))
  emp_1 <- round(sum(output$emp_effects_t1_p)) - emp_0
  emp_t <- round(emp_0 + emp_1)

  net_0 <- sum(output$netearn_effects_t0_p)
  net_1 <- sum(output$netearn_effects_t1_p) - net_0
  net_t <- net_0 + net_1

  tax_0 <- sum(output$emptax_effects_t0_p)
  tax_1 <- sum(output$emptax_effects_t1_p) - tax_0
  tax_t <- tax_0 + tax_1

  ### put results into a matrix

  output_agg <- matrix(c(out_0, out_1, out_t,
                         gva_0, gva_1, gva_t,
                         emp_0, emp_1, emp_t,
                         net_0, net_1, net_t,
                         tax_0, tax_1, tax_t),
                       ncol = 5,
                       byrow = FALSE,
                       dimnames = list(NULL,
                                       c("Output", "GVA", "Employment",
                                         "Net Earn", "Income Tax")))

  output_agg <- data.table(output_agg)

  output_agg[, Output     := round(Output, 3)]
  output_agg[, GVA        := round(GVA, 3)]
  output_agg[, Employment := round(Employment, 0)]

  macro_agg <- macro[, .(output = sum(output),
                         gva = sum(gva),
                         tot_fte = sum(tot_fte))]

  macro_agg_exp <- rbindlist(list(macro_agg,macro_agg,macro_agg))

  aggregate <- cbind(output_agg, macro_agg_exp)

  aggregate[, output_perc := Output/output]
  aggregate[, gva_perc := GVA/gva]
  aggregate[, emp_perc :=  Employment/tot_fte]

  aggregate <- aggregate[, c("Output", "output_perc", "GVA", "gva_perc",
                                         "Employment", "emp_perc")]

  setnames(aggregate,
           names(aggregate),
           c("out", "out_perc", "gva", "gva_perc", "emp", "emp_perc"))

  Effects <- c("Direct", "Indirect", "Total")
  aggregate <- cbind(Effects, aggregate)

  ###########################
  ### OUTPUT THE RESULTS

  return(list(aggregate = aggregate,
              industry = industry))
}
