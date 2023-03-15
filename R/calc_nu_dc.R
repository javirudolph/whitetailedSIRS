#' Calculate transmission for direct contact
#'
#' @param V_DC
#' @param C_nu
#' @param pfu_conv
#' @param k
#' @param nsamples
#' @param seed
#'
#' @return
#' @export
#'
#' @examples
calc_nu_dc <- function(V_DC = NULL,
                      C_nu = NULL,
                      pfu_conv = NULL,
                      k = NULL,
                      nsamples = NULL,
                      seed = NULL){

   if(!is.null(seed)) set.seed(seed)
   if(is.null(nsamples)) nsamples = 1

   # Set defaults
   if (is.null(V_DC)) V_DC = rep(0.1, nsamples)
   if (is.null(C_nu)) C_nu = rlnorm(nsamples, 0.216, 0.344) * 10^5.6
   if (is.null(pfu_conv)) pfu_conv = rep(1/10^5.2, nsamples)
   if (is.null(k)) k = rep(410, nsamples)


   # calculate dose received as function of volume of sputum and viral particles concentration
   d_DC <- V_DC * C_nu * pfu_conv

   # calculate infection probability
   nu_DC <- 1 - exp(-d_DC/k)

   return(nu_DC)
}
