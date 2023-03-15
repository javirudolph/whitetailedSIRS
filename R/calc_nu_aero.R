#' Calculate probability of infection via aerosol \eqn{\nu^{AERO}}
#'
#' `calc_nu_aero` will calculate one value for probability of infection using defaults if no arguments are provided. Defaults are described in the sir_model_description document and are sourced from the literature or expert elicitation.
#'
#' @param AER air exchange in \eqn{hr^{-1}}
#' @param s settling rate; \eqn{hr^{-1}}
#' @param lambda innactivation rates \eqn{hr^{-1}}
#' @param C_nu viral load in sputum; RNA copies/ml
#' @param C_i conversion factor for quanta/RNA copy
#' @param IR inhalation rate; \eqn{m^3/hr}
#' @param ER exhalation rate; \eqn{m^3/hr}
#' @param V_d exhaled droplet volume concentration; ml exhaled droplets/ \eqn{m^3}
#' @param V_air fixed volume; \eqn(m^3)
#' @param t_contact time of contact with contaminated airspace (hr)
#' @param r species-specific probability of infection from 1 quantum. Default is for r_deer with expert elicited values
#' @param nsamples default to 1, but if specified > 1 will draw `nsamples` from the default distributions of parameters
#' @param print_pars logical. If TRUE, will return a list with all parameters used to make calculations
#' @param seed if setting a seed, specify number
#'
#' @section Mathematical background for this calculation
#' An infected individual emits viral particles at a particular rate \eqn{ER_q} in quanta/hr as the product of the arguments described above:
#' \deqn{ER_q = C_{\nu} \cdot C_i \cdot IR \cdot V_d}
#' This is used to model the instantaneous concentration of viral particles (C) in well-mixed air space (quanta/\eqn{m^3}) around an infected individual as follows:
#' \deqn{C = \frac{ER_q}{IVRR \cdot V_air}}
#' where the loss rate (IVRR) is given by:
#' \deqn{AER + s + \lambda}
#' When a susceptible individual enters the contaminated airspace surrounding an infected individual, the dose (\eqn(Q_A)) is the product of inhalation rate, concentration of viral particles, and time of contact:
#' \deqn{Q_A = IR \cdot C \cdot t_{contact}}
#' The dose \eqn{Q_A} is converted into a probability of infection using the Wells-Riley infection model as a function of the dose received and a species-specific probability of infection from 1 quantum.
#' \deqn{\nu^{AERO} = 1 - e^{-rQ}}
#'
#' @return returns a numeric vector of length equal to nsamples or length of vectors provided in arguments
#' @export
#'
#' @examples
#' calc_nu_aero() # will return a single value using defaults
calc_nu_aero <- function(AER = NULL,
                         s = NULL,
                         lambda = NULL,
                         C_nu = NULL,
                         C_i = NULL,
                         IR = NULL,
                         ER = NULL,
                         V_d = NULL,
                         V_air = NULL,
                         t_contact = NULL,
                         r = NULL,
                         nsamples = NULL,
                         print_pars = FALSE,
                         seed = NULL) {

   if(!is_null(seed)) set.seed(seed)
   if(is_null(nsamples)) nsamples = 1

   #### ----------------------------------------------------- ###
   # If any of the parameters are set to NULL we use the defaults
   if (is_null(C_nu)) C_nu = rlnorm(nsamples, 0.216, 0.344) * 10^5.6
   if (is_null(C_i))  C_i = rep(0.0014, nsamples)
   if (is_null(IR)) IR = rep(0.846, nsamples)
   if (is_null(ER)) ER = IR
   if (is_null(V_d)) V_d = rep(0.009, nsamples)
   if (is_null(V_air)) V_air = rep(7.07, nsamples)
   if (is_null(t_contact)) t_contact = rlnorm(nsamples, 1.553, 1.272)
   if (is_null(r)) r = rlnorm(nsamples, 0.2775, 0.272)
   if (is_null(AER)) AER = rep(4, nsamples)
   if (is_null(s)) s = rep(0.24, nsamples)
   if (is_null(lambda)) lambda = rep(0.63, nsamples)


   ### BODY of function ---------------------------##

   # Calculate the viral particle loss rate as the sum
   # of air exchange, settling rate, and inactivation
   IVRR <- AER + s + lambda

   # calculate emission rate of viral particles
   ER_q <- C_nu * C_i * ER * V_d

   # Calculate instantaneous concentration of viral particles.
   C_inst <- ER_q / (IVRR * V_air) #V_air is airspace volume

   # Calculate viral dose in quanta as product of
   # inhalation rate, instantaneous concentration of viral particles
   # and the duration of the contact
   Q <- IR * C_inst * t_contact

   # Use Wells-Riley model to transform amount of virus inhaled
   # into a probability of infection via aerosol
   nu_aero <- 1 - exp(-r * Q)

   if (print_pars == TRUE) {
      list(nu_aero = nu_aero,
           AER = AER,
           s = s,
           lambda = lambda,
           C_nu = C_nu,
           C_i = C_i,
           IR = IR,
           V_d = V_d,
           V_air = V_air,
           t_contact = t_contact,
           r = r,
           seed = seed)
   } else (
      nu_aero
   )
}
