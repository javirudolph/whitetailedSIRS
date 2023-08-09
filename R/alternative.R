#' @title Define parameters for solving white-tailed deer SIRS ODE equations
#' under different management alternatives
#'
#' @description `alternative()` helps to prepare parameters used to solve SIRS
#' ODE equations for both wild and captive deer, to be fed into the params
#' argument of the `run()` function.
#'
#' @param alpha_immunity Inverse of duration of temporary immunity after
#' entering recovered compartment (per day, 0-1).
#' @param omega_ww Deer-deer proximity rate in a wild setting (proximity
#' events per day).
#' @param omega_cw Captive and wild deer proximity rate along fence lines
#' demarking captive and wild populations (proximity events per day).
#' @param omega_cc Deer-deer proximity rate in a captive setting (proximity
#' events per day).
#' @param omega_hw Human-deer proximity rate in a wild setting (proximity
#' events per day).
#' @param omega_hc Human-deer proximity rate in a captive setting(proximity
#' events per day).
#' @param sigma_aero_deer_deer_wild Probability of infection via aerosol
#' transmission between wild deer. Derived using the calc_sigma_aero() function.
#' @param sigma_aero_deer_deer_captive Probability of infection via aerosol
#' transmission in captive deer. Derived using the calc_sigma_aero() function.
#' @param sigma_aero_deer_human_wild Probability of infection via aerosol
#' transmission from humans to wild deer. Derived using the calc_sigma_aero()
#' function.
#' @param sigma_aero_deer_human_capt Probability of infection via aerosol
#' transmission from humans to captive deer. Derived using the calc_sigma_aero()
#' function.
#' @param epsilon_dc Probability of direct contact between deer, given
#' proximity.
#' @param sigma_dc_deer_deer Probability of infection via fluid transmission
#' between deer. Derived using the calc_sigma_dc() function.
#' @param gamma_recov Inverse of duration for recovery from infection (per day,
#' 0-1).
#' @param I_human Prevalence in human population (proportion).
#' @param boost Proportion of susceptible deer in captivity receiving vaccine
#' boosters, per day.
#'
#' @details alternative() creates a list of parameter values that will be fed
#' into a SIRS ODE solver. The length of each item in the output is determined
#' by the length of all arguments, which must be equal. If argument(s) is not
#' filled with outputs from another function in this package (e.g.
#' calc_contact_rate, calc_sigma_aero, calc_sigma_dc, draw_elicitation_samples),
#' the user must fill argument with vector of values of the same length as the
#' other arguments.
#'
#' Parameter values defined with this function remained fixed during SIRS ODE
#' solves. While these parameter values can be derived through random processes,
#' projections are deterministic.
#'
#' Proximity, by default with the calc_sigma_aero() function, is defined as two
#' individuals entering within 1.5m of each other. This proximity limit can be
#' modified in V_air argument in the calc_sigma_aero() function, which defines a
#' half-sphere volume into which aerosolized virus is exhaled from an infectious
#' individual.
#'
#' @return Returns a list of vectors for each parameter
#' @export
#'
#' @seealso `run()`, `calc_contact_rate()`, `calc_sigma_aero()`, `calc_sigma_dc()`, `draw_elicitation_samples()`
#'
#'
#' @examples
#' \dontrun{
#' alternative(alpha_immunity = c((1/30),(1/60),(1/90),(1/120),(1/150)), omega_ww = rep(10,5), omega_cw = rep(1,5), omega_cc = rep(20,5), omega_hw = rep(0.01,5), omega_hc = rep(0.25,5), sigma_aero_deer_deer_wild = rep(0.01,5), sigma_aero_deer_deer_captive = rep(0.1,5), sigma_aero_deer_human_wild = rep(0.001,5), sigma_aero_deer_human_capt = rep(0.05,5), epsilon_dc = rep(0.2,5), sigma_dc_deer_deer = rep(0.02,5), gamma_recov = rep(1/6,5), I_human = rep(0.05,5), boost = rep(0,5))}
#'
alternative <- function(alpha_immunity = NULL, omega_ww = NULL, omega_cw = NULL, omega_cc = NULL, omega_hw = NULL, omega_hc = NULL, sigma_aero_deer_deer_wild = NULL, sigma_aero_deer_deer_captive = NULL, sigma_aero_deer_human_wild = NULL, sigma_aero_deer_human_capt = NULL, epsilon_dc = NULL, sigma_dc_deer_deer = NULL, gamma_recov = NULL, I_human = NULL, boost = NULL)

{
   if(is.null(alpha_immunity)) {stop("Values for alpha_immunity (duration of temporary immunity to SARS-CoV-2) is required") }
   if(is.null(omega_ww)) {stop("Values for omega_ww (proximity rate between wild deer) is required") }
   if(is.null(omega_cw)) {stop("Values for omega_cw (fenceline proximity rate between captive and wild deer) is required") }
   if(is.null(omega_cc)) {stop("Values for omega_cc (proximity rate between captive deer) is required") }
   if(is.null(omega_hw)) {stop("Values for omega_hw (proximity rate between humans and wild deer) is required") }
   if(is.null(omega_hc)) {stop("Values for omega_hc (proximity rate between humans and captive deer) is required") }
   if(is.null(sigma_aero_deer_deer_wild)) {stop("Values for derived nu_aero_deer_deer_wild (probability of infection via aerosol transmission in wild deer) is required") }
   if(is.null(sigma_aero_deer_deer_captive)) {stop("Values for derived nu_aero_deer_deer_captive (probability of infection via aerosol transmission in captive deer) is required") }
   if(is.null(sigma_aero_deer_human_wild)) {stop("Values for derived nu_aero_deer_human_wild (probability of infection via aerosol transmission from humans to wild deer) is required") }
   if(is.null(sigma_aero_deer_human_capt)) {stop("Values for derived nu_aero_deer_human_capt (probability of infection via aerosol transmission from humans to captive deer) is required") }
   if(is.null(epsilon_dc)) {stop("Values for epsilon_dc (probability of direct contact between deer, given proximity) is required") }
   if(is.null(sigma_dc_deer_deer)) {stop("Values for nu_domega_deer_deer (probability of infection via fluid transmission between deer) is required") }
   if(is.null(gamma_recov)) {stop("Values for gamma_recov (duration of recovery from infection) is required") }
   if(is.null(I_human)) {stop("Values for I_human (prevalence in human population) is required") }
   if(is.null(boost)) {stop("Values for boost (boosting rate in captive deer) is required") }

   list(
      alpha_immunity = alpha_immunity,
      beta_aero_ww = omega_ww * sigma_aero_deer_deer_wild,
      beta_aero_cw = omega_cw * sigma_aero_deer_deer_wild, #assumes outdoor fenceline environment
      beta_aero_cc = omega_cc * sigma_aero_deer_deer_captive,
      beta_aero_hw = omega_hw * sigma_aero_deer_human_wild,
      beta_aero_hc = omega_hc * sigma_aero_deer_human_capt,
      beta_dc_ww = omega_ww * epsilon_dc * sigma_dc_deer_deer,
      beta_dc_cw = omega_cw * epsilon_dc * sigma_dc_deer_deer,
      beta_dc_cc = omega_cc * epsilon_dc * sigma_dc_deer_deer,
      gamma_recov = gamma_recov,
      I_human = I_human,
      boost = boost)}
