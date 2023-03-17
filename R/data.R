#' elicitation_data
#'
#' Group averaged (using quantile averaging) parameter estimates for
#' parameters derived using expert elicitation. Parameters include 1) Days until a recovered individual can be
#' reinfected with SARS-CoV-2 (Parameter = Temporary Immunity; units = days), 2)
#' Number of individuals infected via wastewater for every 100 individuals
#' infected via direct transmission (Parameter = Wastewater Infections; units =
#' individuals), 3) Number of individuals infected via formites for every 100
#' individuals infected via direct transmission (Parameter = Fomite Infections;
#' units = individuals), 4) Ratio of deer sputum viral load : humansputum viral
#' load (Parameter = Viral Load; units = multiplicative ratio), 5) Dose-response
#' value for quantum dosage using the Wells-Riley dose response model (Parameter
#' = Dose-Response; units = r value). 6) deer-deer
#' contact duration (minutes), 7) deer-deer direct contact probability, 8)
#' deer-deer proximity rate with baiting (per day; 17 events per day without
#' baiting), 9) deer-human proximity rate, rural (per 120 days), 10) deer-human
#' proximity duration, rural (minutes), 11) deer-human proximity rate, suburban
#' (per 120 days), 12) deer-human proximity duration, suburban (minutes), 13)
#' deer-human proximity rate, captive (per 120 days), 14) deer-human proximity
#' duration, captive (minutes), 15) deer-deer proximity rate in captivity (per
#' day).
#'
#' @format ## `elicitation_data`
#' A data frame with 4 columns and 15 rows, each row corresponding to an elicited parameter:
#' \describe{
#'    \item{parameter}{Parameter elicited}
#'    \item{mu}{mean value}
#'    \item{sd}{standard deviation}
#'    \item{family}{distribution family for the parameters given}
#' }

#' @source Contact: Elias Rosenblatt erosenbl@uvm.edu
#'
"elicitation_data"


#' Contact rate parameters
#'
#' Modeling the relationship between density and contact rates in wild deer
#' populations. Habib et al. (2011) used radio-collared deer in eastern Alberta
#' to, in part, estimate contact rates between deer in the same group and deer
#' in different groups. These results are unique as they calculate the per
#' capita contact rate in terms of deer contacted/individual/time. This differs
#' from many other approaches, which just look at the raw probability of two
#' collared individuals being in the same place at the same time. Habib et al.
#' (2011) estimated home ranges, contact rates, and resource selection functions
#' across variable densities and habitat conditions (areas with 12%, 26%, 53%,
#' and 67% wooded habitat). They then simulated individual white-tailed deer and
#' their movements based on these observed data to estimate the total number of
#' individuals in contact, which was the sum of contact rates within the same
#' group and with different groups. They estimated the most likely
#' parameters for the per-capita contact rate.
#'
#' @format `contact_rate_params`
#'
#'
#' @source https://doi.org/10.1016/j.ecolmodel.2011.05.007
#'
"contact_rate_params"
