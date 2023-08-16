#' @title Contact rate calculation for wild deer
#'
#' @description `calc_contact_rate()` uses proximity rate model developed by
#' Habib et al. (2011) to estimate proximity rates for deer in wild settings,
#' conditional on density and habitat availability. This function can also be
#' used for captive settings with conditions that result in identical deer-deer
#' proximity rates. The user can alter proximity rates with the inclusion of
#' attractants on a landscape.
#'
#' @param kappa value for contact scaling constant (influences slope of density-
#' contact relationship; reported in Habib et al. 2011)
#' @param N_w total population size of deer. Defaults is a random draw from a
#' Poisson distribution with mean = 1000.
#' @param q value for concavity scaling constant (0-1, with 0 equating to
#' density dependence and 1 equating to frequency dependence; reported in Habib
#' et al. 2011)
#' @param A_w area inhabited by N, in square kilometers. Default set to 100
#' square kilometers to match Habib et al.'s (2011) approach.
#' @param rho_attractant proportional change for attractants present in
#' simulated conditions (optional). Default set to 1
#' @param nsamples number of values to be returned. Default is 1, but will
#' return a vector if > 2
#' @param seed if specified, sets a seed for the function
#' @param type_contact option between "low", "medium", and "high" based on
#' parameters from Habib et al. 2011 for particular values of wooded habitat
#' available to the deer population in question. "low" contact fixes kappa to
#' 16.37 and q to 0.53, mimicking proximity rates in areas with 53% wooded
#' habitat. "med" contact fixes kappa to 11.35 and q to 0.34, mimicking
#' proximity rates in areas with 26% wooded habitat. "high" contact fixes kappa
#' to 15.58 and q to 0.32, mimicking proximity rates in areas with 12% wooded
#' habitat. If set to "manual", manual input of kappa and q are needed. All
#' values from Habib et al. (2011) are available in the `contact_rate_params`
#' dataset.
#'
#' @return returns a number or numeric vector if nsamples > 1
#' @export
#'
#' @importFrom stats rpois
#'
#' @examples
#'  \dontrun{
#' calc_contact_rate(type_contact = "low")
#'
#' calc_contact_rate(rho_attractant = 1,
#' kappa = 16.37,
#' N_w = 1000,
#' q = 0.53,
#' A_w = 100,
#' type_contact = "manual")
#'
#' calc_contact_rate(rho_attractant = c(1,1),
#' kappa = c(16.37, 16.37),
#' N_w = rpois(2, 1000),
#' q = c(0.53, 0.53),
#' A_w = c(100, 100),
#' type_contact = "manual")}
#'
calc_contact_rate <- function(kappa = NULL,
                              N_w = NULL,
                              q = NULL,
                              A_w = NULL,
                              rho_attractant = NULL,
                              nsamples = NULL,
                              seed = NULL,
                              type_contact = NULL){


   if(is.null(nsamples)) nsamples = 1
   if(!is.null(seed)) set.seed(seed)
   if(is.null(type_contact)) return(print("Type of contact must be specified as 'low', 'med', 'high'. If set to 'manual', user must provide kappa and q values "))

   if (type_contact == "low") {
      kappa = rep(16.37, nsamples)
      q = rep(0.53, nsamples)
   }

   if (type_contact == "med") {
      kappa = rep(11.35, nsamples)
      q = rep(0.34, nsamples)
   }

   if (type_contact == "high") {
      kappa = rep(15.58, nsamples)
      q = rep(0.32, nsamples)
   }

   if (type_contact == "manual") {
      if(is.null(kappa) | is.null(q)) return(print("kappa and q must be specified for type_contact = 'manual' ")) else{
         kappa = kappa
         q = q
      }
   }

   if (is.null(rho_attractant)) rho_attractant = rep(1, nsamples)
   if (is.null(N_w)) N_w = rpois(nsamples,1000)
   if (is.null(A_w)) A_w = rep(100, nsamples)

   contact_val <- kappa * (N_w^(1-q)/A_w) * rho_attractant
   return(contact_val)
}
