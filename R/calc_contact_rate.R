#' Contact rate calculation for wild deer
#'
#' @param sigma_season value for season scaling (derived from Williams et al 2014; Appendix A)
#' @param scaling_c value for contact scaling constant (influences slope of density-contact relationship; reported in Habib et al. 2011; Appendix A)
#' @param N_w total population size of deer
#' @param q value for concavity scaling constant (0-1, with 0 equating to density dependence and 1 equating to frequency dependence; reported in Habib et al. 2011, Appendix A)
#' @param A_w area inhabited by N
#' @param nsamples number of values to be returned. Default is 1, but will return a vector if > 2
#' @param type_contact set to "c_ww" as is the only calculation done
#'
#' @return returns a number or numeric vector if nsamples > 1
#' @export
#'
#' @examples
#' calc_contact_rate()
#' calc_contact_rate(1, 16.37, 1000, 0.53, 100)
#' calc_contact_rate(c(1,1), c(16.37, 16.37), rpois(2, 1000), c(0.53, 0.53), c(100, 100))
calc_contact_rate <- function(sigma_season = NULL,
                              scaling_c = NULL,
                              N_w = NULL,
                              q = NULL,
                              A_w = NULL,
                              nsamples = NULL,
                              type_contact = "c_ww"){

   if(type_contact != "c_ww") {
      return(print("calculation is only setup for c_ww"))
   }

   if(is_null(nsamples)) nsamples = 1

   if (is_null(sigma_season)) sigma_season = rep(1, nsamples)
   if (is_null(scaling_c))  scaling_c = rep(16.37, nsamples)
   if (is_null(N_w)) N_w = rpois(nsamples,1000)
   if (is_null(q)) q = rep(0.53, nsamples)
   if (is_null(A_w)) A_w = rep(100, nsamples)

   c_ww <- sigma_season * scaling_c * (N_w^(1-q)/A_w)
   return(c_ww)
}
