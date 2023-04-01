#' Random draw of parameters from expert elicitation
#'
#' @param elicitation_data defaults to the package data on expert elicitation.
#'   It requires a dataframe with columns for parameter name, mu, sd, and
#'   distribution family. This function is written around the `elicitation_data`
#'   dataset provided in this package.
#' @param nsamples a number of samples to draw. If none set, it defaults to 1.
#' @param seed if specified, a seed is set. If left NULL no seed is set.
#' @param return_df if TRUE the object returned is a data frame. If FALSE it
#'   will return a list column
#'
#' @return returns a tibble with list column for the samples drawn unless
#'   otherwise specified in the `return_df` argument.
#' @export
#'
#' @examples
#' draw_elicitation_samples() # this will return the default elicitation_data with an additional column for a random sample of 1 for each parameter.
#' draw_elicitation_samples(return_df = TRUE) # will return the same as above but expanded as a dataframe instead of a tibble with a list column
draw_elicitation_samples <- function(elicitation_data = NULL, nsamples = NULL, seed = NULL, return_df = FALSE){

   if(!is.null(seed)) set.seed(seed)
   if(is.null(nsamples)) nsamples = 1

   # we use the default elicitation data unless otherwise specified
   if(is.null(elicitation_data)) elicitation_data <- whitetailedSIRS::elicitation_data
   elicitation_data |>
      # create a random sample using the parameters depending on the specified distribution
      dplyr::mutate(my_sample = ifelse(family == "log-normal",
                                       purrr::pmap(list(mu, sd), function(mu, sd) rlnorm(nsamples, mu, sd)),
                                       purrr::pmap(list(mu, sd),
                                                   function(mu, sd) greybox::rlogitnorm(nsamples, mu, sd)))) -> elicitation_data_with_samples

   if(return_df == TRUE){
      elicitation_data_with_samples |>
         tidyr::unnest(cols = my_sample) -> my_df
      return(my_df)
   } else (
      return(elicitation_data_with_samples)
   )

}



