calc_beta_aero <- function(nu_aero = NULL,
                           nu_aero_human = NULL,
                           c_ww = NULL,
                           c_cc = NULL,
                           c_cw = NULL,
                           c_hw = NULL,
                           c_hc = NULL,
                           nsamples = NULL,
                           return_beta = NULL,
                           seed = NULL){

   if(!is_null(seed)) set.seed(seed)
   if(is_null(nsamples)) nsamples = 1

   # Function will return all or one of the betas

   ####
   # If any of the parameters are set to NULL we use the defaults
   if (is_null(nu_aero)) nu_aero = rep(0.5318192, nsamples) #from calc_nu_fx defaults w.seed = 123
   if (is_null(nu_aero_human)) nu_aero_human = nu_aero
   if (is_null(c_ww)) c_ww = calc_contact_rate_wild(nsamples = nsamples)
   if (is_null(c_cc)) c_cc = rlnorm(nsamples, 3.470, 0.913)
   if (is_null(c_cw)) c_cw = 0.00072 / greybox::rlogitnorm(nsamples, -1.457, 0.708)
   if (is_null(c_hw)) c_hw = rlnorm(nsamples, -1.589, 1.7) / 120
   if (is_null(c_hc)) c_hc = rlnorm(nsamples, 2.521, 1.132) / 120

   ## BODY - calculating the betas
   # Equation 17
   beta_aero_ww <- c_ww * nu_aero

   # EQ 18
   beta_aero_cc <- c_cc * nu_aero

   # EQ 19
   beta_aero_cw <- c_cw * nu_aero

   # EQ 20
   beta_aero_hw <- c_hw * nu_aero_human

   # EQ 21
   beta_aero_hc <- c_hc * nu_aero_human

   ## What to return depending on argument
   # default to ALL
   if (is_null(return_beta)) return_beta = "all"

   if(return_beta == "all"){
      # list for all objects:
      list_betas <- list(beta_aero_ww = beta_aero_ww,
                         beta_aero_cc = beta_aero_cc,
                         beta_aero_cw = beta_aero_cw,
                         beta_aero_hw = beta_aero_hw,
                         beta_aero_hc = beta_aero_hc)
      return(list_betas)
   }

   if(return_beta == "ww"){
      return(beta_aero_ww)
   }

   if(return_beta == "cc"){
      return(beta_aero_cc)
   }

   if(return_beta == "cw"){
      return(beta_aero_cw)
   }

   if(return_beta == "hw"){
      return(beta_aero_hw)
   }

   if(return_beta == "hc"){
      return(beta_aero_hc)
   }

}
