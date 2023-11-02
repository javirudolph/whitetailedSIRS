#'@title Calculate probability of infection given physical contact
#'\eqn{\sigma^{DC}}
#'
#'@description `calc_sigma_dc` will calculate one value for probability of
#'infection using
#' defaults if no arguments are provided. Defaults are described in the
#' sir_model_description document and are sourced from the literature or expert
#' elicitation.
#'
#' @param V_DC Transferred volume (ml). Default is 0.1
#' @param C_nu viral load in sputum; RNA copies/ml. Default samples from expert
#'   elicited distribution of parameter 'Viral Load'. Concentration of
#'   infectious particles in sputum (gc per ml)
#' @param pfu_conv conversion value (PFU per GC). Set to 1/10^5.2
#' @param k set to 410 as the default
#' @param nsamples number of samples wanted. Default is set to 1
#' @param seed sets a seed for random draws of parameter from default
#'   distributions.
#'
#' @details Mathematical background to calculate this infection probability
#' given physical contact. We use a Wells-Riley dose response model that
#' estimates infection probability as a log-logistic function of dose
#' (\eqn{d^{dc}}; plaque-forming units, PFU) and the dose-response function
#' (\eqn{k}; Watanabe et al. 2010). The dose received is the product of a
#' typical volume of sputum transferred given contact (\eqn{V^{DC}}) and the
#' concentration of SARS-CoV-2 in sputum (\eqn{C_{\nu}}). We initially assume
#' that at each contact \eqn{100\mu l} of sputum is transferred between
#' individuals making contact.
#' \deqn{\sigma^{DC} = 1 - e^{-(d^{DC}/k)}} where
#' \deqn{d^{DC} = V^{DC} \cdot C_{\nu} \cdot pfuConv}
#'
#'
#' @return returns a number or vector of probability values for probability of
#' infection via direct contact.
#' @export
#'
#' @importFrom stats rlnorm
#'
#' @examples
#' \dontrun{
#' calc_sigma_dc() # will run the defaults
#' calc_sigma_dc(nsamples = 10)}
#'
calc_sigma_dc <- function(V_DC = NULL,
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
   sigma_DC <- 1 - exp(-d_DC/k)

   return(sigma_DC)
}
