calc_beta_dc <- function(sigma_DC = NULL,
                         nu_DC = NULL,
                         c_ww = NULL,
                         c_cc = NULL,
                         c_cw = NULL,
                         nsamples = NULL,
                         return_beta = NULL,
                         seed = NULL){

   if(!is.null(seed)) set.seed(seed)
   if(is.null(nsamples)) nsamples = 1

   # Function will return all or one of the betas

   #### ----------------------------------------------------- ###
   # If any of the parameters are set to NULL we use the defaults
   if (is.null(sigma_DC)) sigma_DC = greybox::rlogitnorm(nsamples, -1.457, 0.708)
   if (is.null(nu_DC)) nu_DC = rep(0.4658, nsamples) #from calc_nu_fx defaults w.seed = 123
   if (is.null(c_ww)) c_ww = calc_contact_rate(nsamples = nsamples)
   if (is.null(c_cc)) c_cc = rlnorm(nsamples, 3.470, 0.913)
   if (is.null(c_cw)) c_cw = 0.00072 / greybox::rlogitnorm(nsamples, -1.457, 0.708)


   # BODY

   # Equation 24
   beta_dc_ww <- c_ww * sigma_DC * nu_DC

   # EQ 25
   beta_dc_cc <- c_cc * sigma_DC * nu_DC

   # EQ 26
   beta_dc_cw <- c_cw * sigma_DC * nu_DC

   # non in doc
   beta_dc_hc <- c_hc * sigma_DC * nu_DC

   ## What to return depending on argument
   # default to ALL
   if (is.null(return_beta)) return_beta = "all"

   if(return_beta == "all"){
      # list for all objects:
      list_betas <- list(beta_dc_ww = beta_dc_ww,
                         beta_dc_cc = beta_dc_cc,
                         beta_dc_cw = beta_dc_cw)
      return(list_betas)
   }


   ## IF statements to return only one of the betas

   if(return_beta == "ww"){
      return(beta_aero_ww)
   }

   if(return_beta == "cc"){
      return(beta_aero_cc)
   }

   if(return_beta == "cw"){
      return(beta_aero_cw)
   }

}
