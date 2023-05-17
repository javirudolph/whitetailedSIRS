
#' In progress Run SIRS projection
#'
#' This function is still in development but it is a wrapper for other functions in the package and will generate a projection for SIRS model using defaults for a specified number of samples
#'
#' @param nsamples number of samples to return
#' @param seed if specified will set a seed
#' @param wild_scenario deafult to 'rural' but can also calculate 'suburban'
#' @param ndays number of days for projection specified as 120
#' @param ...
#'
#' @return returns a list column tibble with parameters and sirs projection.
#' @export
#'
#' @examples
#' run_sirs_projection(nsamples = 1, seed = 63)
run_sirs_projection <- function(nsamples = 100, seed = 123, wild_scenario = "rural", ndays = 120,
                                type_contact = "low", ...){

   # This is a big function to we divide it in several parts

   # 1. Draw samples from elicitation data --------
   elicitation_samples <- draw_elicitation_samples(nsamples = nsamples, seed = seed)

   # 2. Helper function to extract the parameter -------
   get_param_val <- function(my_param){
      elicitation_samples |>
         dplyr::filter(parameter == my_param) |>
         tidyr::unnest(cols = c(my_sample)) |>
         dplyr::pull(my_sample)
   }

   # 3. Extract values from elicitation sample --------

   alpha_immunity <- 1 / get_param_val('Temporary Immunity')

   # Calculate contact rate between wild deer using the contact_rate fx using defaults:
   c_ww <- whitetailedSIRS::calc_contact_rate(nsamples = nsamples, type_contact = type_contact)

   # The rest of the contact rates were elicited and then modified
   c_cw <- 0.00072 / get_param_val("Direct Contact Probability")
   c_cc <- get_param_val("Deer-Deer Proximity Rate, Captive (per day)")
   c_hc <- get_param_val("Deer-Human Proximity Rate, Captive (per 120 days)") /120

   # time or duration of these contacts was also expert elicited:
   t_contact_deer_deer <- get_param_val("Deer Proximity Duration (minutes)")
   t_contact_deer_human_capt <- get_param_val("Deer-Human Proximity Duration, Captive (minutes)")

   # Here we separate between rural or suburban options:

   if (wild_scenario == "rural"){
      t_contact_deer_human_wild <- get_param_val("Deer-Human Proximity Duration, Rural (minutes)")
      c_hw <- get_param_val("Deer-Human Proximity Rate, Rural (per 120 days)") / 120
   } else {
      t_contact_deer_human_wild <- get_param_val("Deer-Human Proximity Duration, Suburban (minutes)")
      c_hw <- get_param_val("Deer-Human Proximity Rate, Suburban (per 120 days)") / 120
   }

   # Viral load proportional to human value:
   C_nu <- 10^5.6 * get_param_val("Viral Load")

   # Dose response
   r_deer <- get_param_val("Dose-Response")

   # Direct contact probability:
   sigma_dc_wild <- get_param_val("Direct Contact Probability")
   sigma_dc_captive <- get_param_val("Direct Contact Probability")

   # Wondering if we might want to keep track of all of these parameters. Seems annoying but might be a thing...



   # 4. Calculate derived parameters ---------

   nu_aero_deer_deer <- calc_nu_aero(C_nu = C_nu,
                                     t_contact = t_contact_deer_deer / 60,
                                     r = r_deer)

   nu_aero_deer_human_wild <- calc_nu_aero(ER = rep(0.53, nsamples), C_nu = C_nu, t_contact = t_contact_deer_human_wild/60, r = r_deer)
   nu_aero_deer_human_capt <- calc_nu_aero(ER = rep(0.53, nsamples), C_nu = C_nu, t_contact = t_contact_deer_human_capt/60, r = r_deer)

   # direct contact
   nu_dc_deer_deer <- calc_nu_dc(C_nu = C_nu)


   # 5. Additional set parameters

   # Additional parameters just set:
   gamma_recov <- rep(1/6, nsamples)
   I_human <- rep(0.05, nsamples)

   # Calculate values:
   r0_deer <- ((c_ww * nu_aero_deer_deer) + (c_ww * nu_dc_deer_deer * sigma_dc_wild)) * 1/gamma_recov
   foi <- c_hw * nu_aero_deer_human_wild * I_human

   # 5. ODE parameters --------

   list_inits <- list(
      S_wild = rep(1, nsamples),
      I_wild = rep(0, nsamples),
      R_wild = rep(0, nsamples),
      S_captive = rep(1, nsamples),
      I_captive = rep(0, nsamples),
      R_captive = rep(0, nsamples)
   )

   list_params <- list(
      alpha_immunity = alpha_immunity,
      beta_aero_ww = c_ww * nu_aero_deer_deer,
      beta_aero_cw = c_cw * nu_aero_deer_deer,
      beta_aero_cc = c_cc * nu_aero_deer_deer,
      beta_aero_hw = c_hw * nu_aero_deer_human_wild,
      beta_aero_hc = c_hc * nu_aero_deer_human_capt,
      beta_dc_ww = c_ww * sigma_dc_wild * nu_dc_deer_deer,
      beta_dc_cw = c_cw * sigma_dc_wild * nu_dc_deer_deer,
      beta_dc_cc = c_cc * sigma_dc_captive * nu_dc_deer_deer,
      phi_cw = rep(0, nsamples),
      phi_wc = rep(0, nsamples),
      gamma_recov = gamma_recov,
      I_human = I_human
   )

   times <- seq(0, ndays, by = 1)

   # 6. Run ODE --------------

   # create list column to store inits and params
   mytibble <- tibble::tibble(run_id = 1:nsamples,
                      inits = purrr::map(run_id, ~ list_inits |>  purrr:::map_dbl(.x)),
                      params = purrr::map(run_id, function(x) list_params |>  purrr::map_dbl(x)),
                      R0 = r0_deer,
                      FOI = foi)

   # create a new column that stores the results from the ode() projection

   mytibble |>
      dplyr::mutate(ode_proj = purrr::pmap(list(y = inits, parms = params), deSolve::ode, times = seq(0, 120, by = 1), func = simple_sirs)) |>
      # now create another column that stores the equilibrium for these:
      dplyr::mutate(steady_state = purrr::pmap(list(y = inits, parms = params), rootSolve::runsteady, func = simple_sirs)) -> sirs_results # save as new object

   return(sirs_results)

}

