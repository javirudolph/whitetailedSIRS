#' Run ODE Solver for SIRS model
#'
#' @param iter Number of iterations, often referred to as 'nsamples' in package
#' examples
#' @param initial_compartments Initial compartment sizes, including
#' (proportions, stored as list)
#' @param initial_compartments_steady Initial compartment sizes for steady state
#' calculation (proportions, stored as list)
#' @param params SIRS parameters, stored as a list
#' @param times Length of projection (days)
#' @param name Name of context being simulated
#'
#' @return List containing iteration details, including initial compartment
#' sizes for ODE solver and steady state calculation, parameters, context name,
#' and daily, proportional sizes of each SIRS compartment and cumulative
#' infections during the 120 day projection, and whether the SARS-CoV-2
#' persists at equilibrium.
#' @export
#'
#' @importFrom purrr map map_dbl pmap
#' @importFrom tibble tibble
#' @importFrom deSolve ode
#' @importFrom rootSolve runsteady
#' @importFrom magrittr %>%
#' @importFrom dplyr mutate
#'
#' @examples
#' \dontrun{
#' nsamples = 10
#'
#' example_inits <- c(S_wild = 1, I_wild = 0,
#'                    R_wild = 0, I_wild_cumulative = 0,
#'                    S_captive = 1, I_captive = 0,
#'                    R_captive = 0, I_captive_cumulative = 0)
#'
#' example_inits_steady <- c(S_wild = 1, I_wild = 0,
#'                    R_wild = 0, S_captive = 1,
#'                    I_captive = 0, R_captive = 0)
#'
#' # set the time to run
#' example_times <-  seq(0, 365, by = 1)
#' # Set parameters of transmission, immunity, recovery
#'
#' example_params <- c(alpha_immunity = 0.03,
#'                     beta_aero_ww = 0.01,
#'                     beta_aero_cw = 0.01,
#'                     beta_aero_cc = 0.02,
#'                     beta_aero_hw = 0.01,
#'                     beta_aero_hc = 0.2,
#'                     beta_dc_ww = 0.01,
#'                     beta_dc_cw = 0.01,
#'                     beta_dc_cc = 0.01,
#'                     gamma_recov = 0.01,
#'                     I_human = 0.05,
#'                     boost = 0)
#'
#' run(iter = nsamples,
#' initial_compartments = example_inits,
#' initial_compartments_steady = example_inits_steady,
#' params = example_params,
#' times = example_times,
#' name = Test)}
#'
run <- function(iter = NULL, initial_compartments = NULL, initial_compartments_steady = NULL, params = NULL, times = NULL, name = NULL)

{  if(is.null(iter)) {stop("User must specify number of iterations - often stored as nsamples object")}
   if(is.null(initial_compartments)) {stop("User must specify initial compartment sizes, either by hand or with initial_compartments function")}
   if(is.null(params)) {stop("User must specify all parameters for ODE equation, either by hand or with alternative function")}
   if(is.null(name)) {stop("User must name context for data storage")}

   tibble(run_id = 1:iter, inits.fall = map(.data$run_id, ~ initial_compartments %>% map_dbl(., .x)), inits.steady = map(.data$run_id, ~ initial_compartments_steady %>% map_dbl(., .x)), params = map(.data$run_id, function(x) params %>% map_dbl(., x)))%>%
      mutate(ode_proj = pmap(list(y = .data$inits.fall, parms = params), ode, times = times, func = simple_sirs_with_cumulative)) %>%
      mutate(steady_state = pmap(list(y = .data$inits.steady, parms = params), runsteady, func = simple_sirs, mf = 10)) %>%
      mutate(Context = rep(name, nsamples))}
