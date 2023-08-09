#' Define starting compartment sizes for SIRS ODE Solver
#'
#' @param S_wild_prop Size of susceptible compartment in wild population
#' (proportion)
#' @param I_wild_prop Size of infectious compartment in wild population
#' (proportion)
#' @param R_wild_prop Size of recovered compartment in wild population
#' (proportion)
#' @param S_captive_prop Size of susceptible compartment in captive population
#' (proportion)
#' @param I_captive_prop Size of infectious compartment in captive population
#' (proportion)
#' @param R_captive_prop Size of recovered compartment in captive population
#' (proportion)
#' @param draws Number of iterations of simulation
#' @param steady Logical argument to state whether these initial compartments
#' will be used to calculate compartment sizes as steady state equilibrium
#' (steady = TRUE), or for a projection through a specified time period
#' (steady = FALSE; default).
#'
#' @details Provides a vector of compartment sizes for wild and captive
#' populations. This function pairs wild and captive deer populations so that
#' transmission between populations can occur. Values (except for draws
#' parameter) must be proportions, and must sum to one for each population.
#'
#' @return List of six vectors (steady == TRUE) or eight vectors (steady ==
#' FALSE), repeating starting conditions specified by user. Length of vectors
#' are determined by draws argument. When steady == FALSE, output will contain
#' starting values for tracking cumulative infections (I_wild_cumulative and
#' I_captive_cumulative), both set to zero.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' initial_compartments(S_wild_prop = 0, draws = 5) #Starting conditions for a
#' #simulation focused only on captive deer
#' initial_compartments(draws = 5) #Starting conditions for a simulation of
#' #introduction and spread in both wild and captive deer.}
#'
initial_compartments <- function(S_wild_prop = 1, I_wild_prop = 0, R_wild_prop = 0, S_captive_prop = 1, I_captive_prop = 0, R_captive_prop = 0, draws = NULL, steady = FALSE)
{
   if(is.null(draws)) {stop("Specify number of iterations (draws)")}
   if(steady == FALSE) {list(
   S_wild = rep(S_wild_prop, draws),
   I_wild = rep(I_wild_prop, draws),
   R_wild = rep(R_wild_prop, draws),
   I_wild_cumulative = rep(0, draws),
   S_captive = rep(S_captive_prop, draws),
   I_captive = rep(I_captive_prop, draws),
   R_captive = rep(R_captive_prop, draws),
   I_captive_cumulative = rep(0, draws))}

   else {list(
   S_wild = rep(S_wild_prop, draws),
   I_wild = rep(I_wild_prop, draws),
   R_wild = rep(R_wild_prop, draws),
   S_captive = rep(S_captive_prop, draws),
   I_captive = rep(I_captive_prop, draws),
   R_captive = rep(R_captive_prop, draws))}
}

