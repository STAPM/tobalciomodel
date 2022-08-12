#' Generate IO Inputs from STAPM Models
#'
#' Function to read in and further process the outputs of STAPM price policy
#' modelling economic outcomes. Produces the change in demand for tobacco, on-trade
#' alcohol and off-trade alcohol measured in basic prices and also the net change
#' in government revenue.
#'
#' @param root Character - the file path to the folder storing STAPM raw output
#' @param label Character - the label used to identify the set of model runs.
#' @param label2 Character vector - vector of treatment labels identifying specific model runs.
#' @param min_age Numeric - Youngest age included in the model.
#' @param index_year Numeric - Model index year.
#' @param policy_effect_year Numeric - Year in which new price policy is implemented in STAPM.
#' @param n_years Numeric - Number of years including the policy effect year for which to generate inputs
#' (defaults to 1 - policy effect year only).
#'
#' @return
#' @export
#'
#' @examples
#'
#' \dontrun{
#'
#' }
GenInputsSTAPM <- function(root = NULL,
                           label = NULL,
                           label2 = NULL,
                           min_age = NULL,
                           index_year = NULL,
                           policy_effect_year = NULL,
                           n_years = 1){

  ################################################
  #### Generate the population scaling factor ####

  # The simulated individual-level consumption data
  sim_data <- stapmr::ReadSim(root = root,
                              label = label,
                              label2 = label2[1],
                              two_arms = TRUE,
                              keep_vars = c("ran_id", "sex", "imd_quintile", "income5cat", "age", "time_since_quit", "year",
                                            "smk.state", "cigs_per_day", "units_FM_cigs", "units_RYO_tob", "weekmean",
                                            "units_off_beer", "units_off_cider", "units_off_wine", "units_off_spirits",
                                            "units_off_rtds", "units_on_beer", "units_on_cider", "units_on_wine",
                                            "units_on_spirits", "units_on_rtds"))
  # Population size
  run_summary <- readxl::read_excel(paste0(root, "run_info_", label, ".xlsx"), "Summary") %>% setDT
  baseline_population_size <- as.numeric(run_summary[Item == "Population size", Value])

  # Scaling factor to years lived to scale back up to actual population size
  act_pop_size <- sum(stapmr::pop_counts[year == index_year & age %in% min_age:89, N])
  pop_prop <- act_pop_size / baseline_population_size

  rm(sim_data) ; gc()

  #############################################################################
  # Loop over model runs - use econcalc and then clean to get IO model inputs #

  for (i in 1:length(label2)) {

    ##### Calculate economic outcomes

    econ_calcs  <- econcalc::EconCalc(root = root,
                                      iter = NULL,
                                      label = label,
                                      label2 = label2[i],
                                      two_arms = TRUE,
                                      n_tob_prods = 1 ,
                                      n_alc_prods = 2,
                                      strat_vars = NULL,
                                      pop_prop = pop_prop)

    ### Convert economic outcomes into IO model inputs

    inputs <- tobalciomodel::ReadInputs(econ_calcs,
                                        policy_effect_year = policy_effect_year,
                                        n_years = n_years)

    ### Label and combine

    inputs[, model := label2[i]]

    if (i == 1){

      input_data <- copy(inputs)

    } else if (i > 1) {

      input_data <- rbindlist(list(input_data, inputs))

    }

  }

return(input_data)

}

