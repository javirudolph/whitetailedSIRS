#' Random draw of parameters from expert elicitation
#'
#' @param elicitation_data defaults to the package data on expert elicitation
#' @param nsamples a number of samples to draw
#' @param seed if specified, a seed is set
#' @param return_df is TRUE the object returned is a data frame
#'
#' @return returns a tibble with list column for the samples drawn unless
#'   otherwise specified in the `return_df` argument.
#' @export
#'
#' @examples
draw_elicitation_samples <- function(elicitation_data = NULL, nsamples = NULL, seed = NULL, return_df = FALSE){

   if(!is.null(seed)) set.seed(seed)

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



