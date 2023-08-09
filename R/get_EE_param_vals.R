#' get_EE_param_vals: Prepare random draws for inputs in the SIRS ODE equations.
#'
#' @param data Dataset containing random draws for parameters estimate with
#' expert elicitation.
#' @param my_param Character string for parameters estimated with expert
#' elicitation.
#'
#' @details This function requires that draw_elicitation_samples() be run prior.
#' The output of draw_elicitation_samples is the input argument for this
#' function (my_param).  Options for parameters include: "Temporary Immunity",
#' "Wastewater Infections","Fomite Infections","Viral Load","Dose-Response",
#' "Deer Proximity Duration (minutes)", "Direct Contact Probability",
#' "Proximity rate with baiting (17 events without baiting)",
#' "Deer-Human Proximity Rate, Rural (per 120 days)",
#' "Deer-Human Proximity Duration, Rural (minutes)", "Deer-Human Proximity Rate,
#' Suburban (per 120 days)", "Deer-Human Proximity Duration, Suburban (minutes)"
#' , "Deer-Human Proximity Rate, Captive (per 120 days)", "Deer-Human Proximity
#' Duration, Captive (minutes)", "Deer-Deer Proximity Rate, Captive (per day)".
#' Only one parameter may be retrieved in a get_EE_param_vals() call.
#'
#' @return vector of random samples from error distributions estimated with
#' expert elicitation, with a length determined by the number of iterations run
#' for a simulation (nsamples argument, draw_elicitation_samples.R).
#' @export
#'
#' @importFrom dplyr filter pull
#' @importFrom tidyr unnest
#' @importFrom magrittr %>%
#'
#' @seealso draw_elicitation_samples.R
#'
#' @examples
#' \dontrun{
#' draw_elicitation_samples(nsamples = 50)
#' get_EE_param_vals(my_param = "Temporary Immunity")}
#'
get_EE_param_vals <- function(data, my_param){
   data %>%
      filter(parameter == my_param) %>%
      unnest(cols = c(my_sample)) %>%
      pull(my_sample)
}
