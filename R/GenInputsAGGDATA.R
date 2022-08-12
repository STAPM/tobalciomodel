#' Generate IO Inputs from Aggregate Data
#'
#' Calculate changes in demand based on aggregate spending data for alcohol and tobacco for
#' specified years. Input proportionate changes to demand to create a data table of changes
#' in final demand.
#'
#' @param scenario_list List object. List of numeric vectors of length four of proportionate changes to demand to model. In order;
#' off-trade alcohol, on-trade alcohol, factory-made cigarettes, and hand-rolled tobacco.
#' @param year Numeric. Year of consumption and economic data to use
#' @param input_labels Character vector - vector of labels identifying the scenarios modelled.
#' @param write_inputs Character - file path to the folder to which the input data will be written.
#' @param name Character - file name for the input data.
#'
#' @return
#' @export
#'
#' @examples
#'
#' \dontrun{
#'
#' }
GenInputsAGGDATA <- function(scenario_list,
                             year = 2019,
                             input_labels,
                             write_inputs,
                             name){


  for (i in 1:length(scenario_list)) {

    changes <- scenario_list[[i]]

    input <- tobalciomodel::CalcInputs(year = year,
                                       prop_alc_off = changes[1],
                                       prop_alc_on  = changes[2],
                                       prop_tob_fm =  changes[3],
                                       prop_tob_ryo = changes[4] )

    ### Label and combine

    inputs[, model := input_labels[i]]

    if (i == 1){

      input_data <- copy(inputs)

    } else if (i > 1) {

      input_data <- rbindlist(list(input_data, inputs))

    }
  }
  saveRDS(input_data, paste0(here::here(write_inputs),"/",name,".rds"))

}
