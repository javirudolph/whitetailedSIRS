#' Define starting compartment sizes for SIRS ODE Solver
#'
#' @param S_wild_prop Size of susceptible compartment in wild population (proportion)
#' @param I_wild_prop Size of infectious compartment in wild population (proportion)
#' @param R_wild_prop Size of recovered compartment in wild population (proportion)
#' @param I_wild_cumulative Cumulative infected in wild segment, starting always at 0
#' @param S_captive_prop Size of susceptible compartment in captive population (proportion)
#' @param I_captive_prop Size of infectious compartment in captive population (proportion)
#' @param R_captive_prop Size of recovered compartment in captive population (proportion)
#' @param I_captive_cumulative Cumulative infected in captive segment, starting always at 0
#' @param draws $Number of iterations of simulation
#'
#' @details Provides a vector of compartment sizes for wild and captive populations. This function pairs wild and captive deer populations so that transmission between populations can occur. Values (except for draws parameter) must be proportions, and must sum to one for each population.
#'
#' @return List of six vectors, repeating starting conditions specified by user. Length of vectors are determined by draws argument.
#' @export
#'
#' @examples
#' initial_compartments(S_wild_prop = 0, draws = 5) #Starting conditions for a simulation focused only on captive deer
#' initial_compartments(draws = 5) #Starting conditions for a simulation of introduction and spread in both wild and captive deer.
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

