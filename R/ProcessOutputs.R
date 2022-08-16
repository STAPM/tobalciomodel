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
                           FAI = TRUE) {

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

  output_ind <- output[, .(out_2 = sum(out_effects_t2_p),
                           gva_2 = sum(gva_effects_t2_p),
                           emp_2 = sum(emp_effects_t2_p),
                           net_2 = sum(netearn_effects_t2_p),
                           tax_2 = sum(emptax_effects_t2_p)), by = "Industry"]

  ######################################
  ### Obtain the distribution of economic outcomes across industries based on
  ### the most recent year

  macro <- macro[year == max(year),]
  macro[, output_prop := output/sum(output)]
  macro[, gva_prop := gva/sum(gva)]
  macro[, tot_fte_prop := tot_fte/sum(output)]

  macro[, c("year","output","gva","tot_emp","tot_fte") := NULL]

  industry <- merge(output_ind, macro, by = "Industry", sort = F)


  ##############################################################################
  ## AGGREGATE FIGURES - split by direct, indirect, and total effects ##########

  ## generate total type 0 and type 1 effects for output, GVA, employment,
  ## earnings, and income tax on earnings

  out_0 <- sum(output$out_effects_t0_p)
  out_1 <- sum(output$out_effects_t1_p) - out_0
  out_2 <- sum(output$out_effects_t2_p) - out_1 - out_0

  out_t <- out_0 + out_1 + out_2

  gva_0 <- sum(output$gva_effects_t0_p)
  gva_1 <- sum(output$gva_effects_t1_p) - gva_0
  gva_2 <- sum(output$gva_effects_t2_p) - gva_1 - gva_0
  gva_t <- gva_0 + gva_1 + gva_2

  emp_0 <- round(sum(output$emp_effects_t0_p))
  emp_1 <- round(sum(output$emp_effects_t1_p)) - emp_0
  emp_2 <- round(sum(output$emp_effects_t2_p)) - emp_1 - emp_0
  emp_t <- round(emp_0 + emp_1 + emp_2)

  net_0 <- sum(output$netearn_effects_t0_p)
  net_1 <- sum(output$netearn_effects_t1_p) - net_0
  net_2 <- sum(output$netearn_effects_t2_p) - net_1 - net_0
  net_t <- net_0 + net_1 + net_2

  tax_0 <- sum(output$emptax_effects_t0_p)
  tax_1 <- sum(output$emptax_effects_t1_p) - tax_0
  tax_2 <- sum(output$emptax_effects_t2_p) - tax_1 - tax_0

  tax_t <- tax_0 + tax_1 + tax_2


  output_agg <- matrix(c(out_0, out_1, out_2, out_t,
                         gva_0, gva_1, gva_2, gva_t,
                         emp_0, emp_1, emp_2, emp_t,
                         net_0, net_1, net_2, net_t,
                         tax_0, tax_1, tax_2, tax_t),
                       ncol = 5,
                       byrow = FALSE,
                       dimnames = list(c("Direct Effect", "Indirect Effect","Induced Effect", "Total"),
                                       c("Output", "GVA", "Employment",
                                         "Net Earn", "Income Tax")))

  aggregate <- data.table(output_agg)

  ###########################
  ### OUTPUT THE RESULTS

  return(list(aggregate = aggregate,
              industry = industry))
}
