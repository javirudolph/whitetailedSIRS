#' @title Contact rate calculation for wild deer
#'
#' @description `calc_contact_rate()` uses proximity rate model developed by Habib et al. (2011) to estimate proximity rates for deer in wild settings, conditional on density and habitat availability. This function can also be used for captive settings with conditions that result in identical deer-deer proximity rates.
#'
#' @param sigma_season value for season scaling (optional). Default set to 1
#' @param scaling_c value for contact scaling constant (influences slope of density-contact relationship; reported in Habib et al. 2011)
#' @param N_w total population size of deer. Defaults is a random draw from a Poisson distribution with mean = 1000.
#' @param q value for concavity scaling constant (0-1, with 0 equating to density dependence and 1 equating to frequency dependence; reported in Habib et al. 2011)
#' @param A_w area inhabited by N, in square kilometers. Default set to 100 square kilometers to match Habib et al.'s (2011) approach.
#' @param nsamples number of values to be returned. Default is 1, but will return a vector if > 2
#' @param seed if specified, sets a seed for the function
#' @param type_contact option between "low", "medium", and "high" based on parameters from Habib et al. 2011 for particular values of wooded habitat available to the deer population in question. "low" contact fixes scaling_c to 16.37 and q to 0.53, mimicking proximity rates in areas with 53% wooded habitat. "med" contact fixes scaling_c to 11.35 and q to 0.34, mimicking proximity rates in areas with 26% wooded habitat. "high" contact fixes scaling_c to 15.58 and q to 0.32, mimicking proximity rates in areas with 12% wooded habitat. If set to "manual", manual input of scaling_c and q are needed. All values from Habib et al. (2011) are available in the `contact_rate_params` dataset.
#'
#' @return returns a number or numeric vector if nsamples > 1
#' @export
#'
#' @examples
#' calc_contact_rate(type_contact = "low")
#' calc_contact_rate(sigma_season = 1, scaling_c = 16.37, N_w = 1000, q = 0.53, A_w = 100, type_contact = "manual")
#' calc_contact_rate(c(1,1), c(16.37, 16.37), rpois(2, 1000), c(0.53, 0.53), c(100, 100), type_contact = "manual")
calc_contact_rate <- function(sigma_season = NULL,
                              scaling_c = NULL,
                              N_w = NULL,
                              q = NULL,
                              A_w = NULL,
                              nsamples = NULL,
                              seed = NULL,
                              type_contact = NULL){


   if(is.null(nsamples)) nsamples = 1
   if(!is.null(seed)) set.seed(seed)
   if(is.null(type_contact)) return(print("Type of contact must be specified as 'low', 'med', 'high'. If set to 'manual', user must provide scaling_c and q values "))

   if (type_contact == "low") {
      scaling_c = rep(16.37, nsamples)
      q = rep(0.53, nsamples)
   }

   if (type_contact == "med") {
      scaling_c = rep(11.35, nsamples)
      q = rep(0.34, nsamples)
   }

   if (type_contact == "high") {
      scaling_c = rep(15.58, nsamples)
      q = rep(0.32, nsamples)
   }

   if (type_contact == "manual") {
      if(is.null(scaling_c) | is.null(q)) return(print("scaling_c and q must be specified for type_contact = 'manual' ")) else{
         scaling_c = scaling_c
         q = q
      }
   }

   if (is.null(sigma_season)) sigma_season = rep(1, nsamples)
   if (is.null(N_w)) N_w = rpois(nsamples,1000)
   if (is.null(A_w)) A_w = rep(100, nsamples)

   contact_val <- sigma_season * scaling_c * (N_w^(1-q)/A_w)
   return(contact_val)
}
