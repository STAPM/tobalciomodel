#' Generate Model Outputs
#'
#' Take the output of the model and generate summary outputs
#'
#' @param data data table. Output of the input-output modelling
#' @param sort character. Variable on which to sort sectors by effect size. Inputs are one from
#' c("output","empl","gva","inctaxes","earn")
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
                           sort) {

  output <- copy(data)

  ## generate total type 0 and type 1 effects for output, GVA, employment,
  ## earnings, and income tax on earnings

  out_0 <- sum(output$out_effects_t0_p)
  out_1 <- sum(output$out_effects_t1_p)
  out_t <- out_0 + out_1

  gva_0 <- sum(output$gva_effects_t0_p)
  gva_1 <- sum(output$gva_effects_t1_p)
  gva_t <- gva_0 + gva_1

  emp_0 <- round(sum(output$emp_effects_t0_p))
  emp_1 <- round(sum(output$emp_effects_t1_p))
  emp_t <- round(emp_0 + emp_1)

  net_0 <- sum(output$netearn_effects_t0_p)
  net_1 <- sum(output$netearn_effects_t1_p)
  net_t <- net_0 + net_1

  tax_0 <- sum(output$emptax_effects_t0_p)
  tax_1 <- sum(output$emptax_effects_t1_p)
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

  return(tab)
}
