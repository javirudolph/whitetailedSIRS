#' Simple function to generate SIRS model projections
#'
#' `Epi_sirs` is written to work alongside the `deSolve::ode()` function, and
#' it will return an object with the proportion of individuals found in each of
#' the SIR compartments at each of the specified time points. `Epi_sirs` is also written to work with `rootSolve::run_steady()` to estimate stable, equilibrium sizes for the SIR compartments.
#'
#' @param time vector of values for which to generate the projection (length in
#'   days).
#' @param state initial state for compartment populations. This should be a
#'   named vector for starting values of S_wild, I_wild, R_wild, S_captive,
#'   I_captive, R_captive.
#' @param parameters list of parameters to generate projection. The parameters
#'   should include transmission parameters, immunity and recovery rates, and
#'   proportion of infected humans.
#'
#'   @details Use `Epi_sirs` to calculate persistence (via `rootSolve::run_steady()`), along with how compartment sizes change through a projection. If cumulative infections are desired, use `Epi_sir_with_cumulative` instead.
#'
#' @return when used with the `deSolve::ode()` function, it will return a
#'   dataframe with the proportion of individuals in each of the SIR
#'   compartments at each time point.
#' @export
#'
#' @examples
#' # prepare the input parameters:
#' example_inits <- c(S_wild = 1, I_wild = 0,
#'                    R_wild = 0, S_captive = 1,
#'                    I_captive = 0, R_captive = 0)
#'
#' # set the time to run
#' example_times <-  seq(0, 500, by = 1)
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
#' # run the ode function:
#'
#' deSolve::ode(y = example_inits, times = example_times, parms = example_params, func = whitetailedSIRS::Epi_sirs)
#'
#'
Epi_sirs <- function(time, state, parameters){
   with(as.list(c(state, parameters)), {

      # wild deer functions

      dS_wild <- alpha_immunity * R_wild -
         (S_wild * ((beta_aero_ww * I_wild) + (beta_dc_ww * I_wild) +
                       (beta_aero_cw * I_captive) + (beta_dc_cw * I_captive) +
                       (beta_aero_hw * I_human)))

      dI_wild <- (S_wild * ((beta_aero_ww * I_wild) + (beta_dc_ww * I_wild) +
                               (beta_aero_cw * I_captive) + (beta_dc_cw * I_captive) +
                               (beta_aero_hw * I_human))) -
         (gamma_recov * I_wild)

      dR_wild <- (gamma_recov * I_wild) -
         (alpha_immunity * R_wild)


      # captive deer functions

      dS_captive <- alpha_immunity * R_captive -
         (S_captive * ((beta_aero_cc * I_captive) + (beta_dc_cc *I_captive) +
                          (beta_aero_cw * I_wild) + (beta_dc_cw * I_wild) +
                          (beta_aero_hc * I_human))) - (S_captive*boost)

      dI_captive <- (S_captive * ((beta_aero_cc * I_captive) + (beta_dc_cc *I_captive) +
                                     (beta_aero_cw*I_wild) + (beta_dc_cw * I_wild) +
                                     (beta_aero_hc * I_human))) -
         (gamma_recov * I_captive)

      dR_captive <- (gamma_recov * I_captive) -
         (alpha_immunity * R_captive) + (S_captive*boost)

      return(list(c(dS_wild, dI_wild, dR_wild, dS_captive, dI_captive, dR_captive)))
   })
}
