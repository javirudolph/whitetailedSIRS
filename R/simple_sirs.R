#' Simple function to generate SIRS model projections
#'
#' @param time vector of values for which to generate the projection (length in days)
#' @param state initial state for compartment populations
#' @param parameters list of parameters to generate projection
#'
#' @return list of values at each time step
#' @export
#'
#' @examples
simple_sirs <- function(time, state, parameters){
   with(as.list(c(state, parameters)), {

      # wild deer functions

      dS_wild <- alpha_immunity * R_wild -
         (S_wild * ((beta_aero_ww * I_wild) + (beta_dc_ww * I_wild) +
                       (beta_aero_cw * I_captive) + (beta_dc_cw * I_captive) +
                       (beta_aero_hw * I_human))) +
         (phi_cw * S_captive) -
         (phi_wc * S_wild)

      dI_wild <- (S_wild * ((beta_aero_ww * I_wild) + (beta_dc_ww * I_wild) +
                               (beta_aero_cw * I_captive) + (beta_dc_cw * I_captive) +
                               (beta_aero_hw * I_human))) +
         (phi_cw * I_captive) -
         (phi_wc * I_wild) -
         (gamma_recov * I_wild)

      dR_wild <- (phi_cw * R_captive) -
         (phi_wc * R_wild) +
         (gamma_recov * I_wild) -
         (alpha_immunity * R_wild)


      # captive deer functions

      dS_captive <- alpha_immunity * R_captive -
         (S_captive * ((beta_aero_cc * I_captive) + (beta_dc_cc *I_captive) +
                          (beta_aero_cw * I_wild) + (beta_dc_cw * I_wild) +
                          (beta_aero_hc * I_human))) -
         (phi_cw * S_captive) +
         (phi_wc * S_wild)

      dI_captive <- (S_captive * ((beta_aero_cc * I_captive) + (beta_dc_cc *I_captive) +
                                     (beta_aero_cw*I_wild) + (beta_dc_cw * I_wild) +
                                     (beta_aero_hc * I_human))) -
         (phi_cw * I_captive) +
         (phi_wc * I_wild) -
         (gamma_recov * I_captive)

      dR_captive <- (gamma_recov * I_captive) -
         (alpha_immunity * R_captive) -
         (phi_cw * R_captive) +
         (phi_wc * R_wild)

      return(list(c(dS_wild, dI_wild, dR_wild, dS_captive, dI_captive, dR_captive)))
   })
}
