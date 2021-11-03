#' Generate Model Outputs
#'
#' Take the output of the model and generate summary outputs
#'
#' @param data data table. Output of the input-output modelling
#' @param sort character. Variable on which to sort sectors by effect size. Inputs are one from
#' c("output","empl","gva","inctaxes","earn")
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
                           sort = NULL,
                           FAI = FALSE) {

  output <- copy(data)

  ### Assign broad industry grouping to the individual sectors according to SIC-2007

  if (isTRUE(FAI)) {
    output[1:7   , Industry := "Agriculture and Mining"]
    output[8:51  , Industry := "Manufacturing"]
    output[52:57 , Industry := "Utilities"]
    output[58    , Industry := "Construction"]
    output[59:61 , Industry := "Wholesale and Retail"]
    output[62:67 , Industry := "Transport and Storage"]
    output[68:71 , Industry := "Accommodation and Food Services"]
    output[72:76 , Industry := "Information and Communication"]
    output[77:106, Industry := "Other Services"]
  } else if (!isTRUE(FAI)) {
    output[1:7   , Industry := "Agriculture and Mining"]
    output[8:50  , Industry := "Manufacturing"]
    output[51:56 , Industry := "Utilities"]
    output[57    , Industry := "Construction"]
    output[58:60 , Industry := "Wholesale and Retail"]
    output[61:66 , Industry := "Transport and Storage"]
    output[67:68 , Industry := "Accommodation and Food Services"]
    output[69:73 , Industry := "Information and Communication"]
    output[74:105, Industry := "Other Services"]
  }

  ####################################################
  ## INDUSTRY LEVEL FIGURES - total effects only #####

  output[, Industry := factor(Industry, levels = c("Agriculture and Mining","Manufacturing","Utilities",
                                                   "Construction","Wholesale and Retail","Transport and Storage",
                                                   "Accommodation and Food Services","Information and Communication","Other Services"))]

  output_ind <- output[, .(out_1 = sum(out_effects_t1_p),
                           gva_1 = sum(gva_effects_t1_p),
                           emp_1 = sum(emp_effects_t1_p),
                           net_1 = sum(netearn_effects_t1_p),
                           tax_1 = sum(emptax_effects_t1_p)), by = "Industry"]

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

  tab <- matrix(c(out_0, out_1, out_t,
                  gva_0, gva_1, gva_t,
                  emp_0, emp_1, emp_t,
                  net_0, net_1, net_t,
                  tax_0, tax_1, tax_t),
                ncol = 5,
                byrow = FALSE,
                dimnames = list(c("Direct Effect","Indirect Effect","Total Effect"),
                                c("Output", "GVA", "Employment",
                                  "Net Earn", "Income Tax")))

  ### output the results

  return(list(aggregate = tab,
              industry = output_ind))
}
