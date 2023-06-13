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

#' scenario_results
#'
#' A dataframe containing results used to compare degrees of introduction, spread, prevalence, persistence and cumulative infections of SARS-CoV-2 in simulated white-tailed deer populations. 200 iterations (run_id) were run for each scenario (Context). Scenarios included captive deer in outdoor ranch facilities, captive deer in intensive facilities, wild deer in rural areas, and wild deer in suburban areas. Setting indicates whether deer were captive or wild. r0, or basic reproductive number, indicates the number of secondary infections caused by a single infectious deer over the course of it's infection. FOI, or Force-Of-Infection, is a hazard rate of a deer becoming infected from infectious humans, per day. Prevalence is the percent of a population infected, averaged over a simulated 120-day projection. Persistence is a logical condition indicating if equilibrium determined by SIRS ODE equations and `run_steady()` from the rootSolve package predicts at least 1 in 1,000 deer infected at equilibrium. Cumulative infections reports the total proportion of the population infected over the course of the 120-fall projection, and can exceed 1, indicating that all individuals were infected at least once during the fall season.
#'
#' @format `scenario_results`
#'
#'
#' @source Derived using `SIRS_analysis_by_contexts.Rmd`
#'
'scenario_results'

#' alternatives_across_systems
#'
#' A dataframe containing results used to compare influences of potential management alternatives on the prevalence, persistence and cumulative infections of SARS-CoV-2 in simulated white-tailed deer populations. 200 iterations (run_id) were run for captive and wild populations of white-tailed deer, seperated by a single fenceline that allows proximity and direct contact between captive and wild deer (Complex), under each alternative implemented (Alternative). Complexes include captive deer in outdoor ranch facilities and wild deer in rural areas (Outdoor ranch and rural deer), captive deer in outdoor ranch facilities and wild deer in suburban areas (Outdoor ranch and suburban deer), captive deer in intensive facilities and wild deer in rural areas (Intensive facility and rural deer), and captive deer in intensive facilities and wild deer in suburban areas (Intensive facility and suburban deer). Management alternatives (Alternative) include 1) no action (baseline), 2) improve separation between captive and wild deer by improving fencing (double-fence captive facilities), 3) reduce and eliminate baiting practices through legislation, enforcement, and education (Eliminate baiting), 4) increase air flow in indoor facilities to match air exchange rate in outdoor settings (Improve indoor facility air quality; only applies for complexes with intensive facilities), 5) Cease research activities that would bring research and management staff in close proximity with wild deer (Pause Research), 6) Require personal protective equipment for humans during interactions with deer in both captive and wild settings (PPE, Both), 7) Require PPE in captive settings only (PPE, Captive), 8) Require PPE in wild settings only (PPE, Wild), 9) Reduce wild deer densities by 10% to reduce deer-deer contact rate and spread of SARS-CoV-2 (Reduce wild density by 10%), 10) Reduce wild deer densities by 25% to reduce deer-deer contact rate and spread of SARS-CoV-2 (Reduce wild density by 25%), 11) restrict human activities in areas posing elevated probability of human-deer interactions, such as suburban neighborhoods, parks, trail heads, etc. (Restrict human interactions), 12) Vaccinate and repeatedly boost captive deer (Vaccinate and boost captive deer), and 13) provide initial vaccination to captive deer with no secondary boosting (Vaccinate captive deer). Wild and captive prevalence (meanWild and meanCaptive, respectively) are reported as the percent of a population infected, averaged over a simulated 120-day projection (run_id), for a particular alternative applied to a particular context. Cumulative infections reports the total proportion of the wild and captive populations infected over the course of the 120-fall projection (cumulativeWild and cumulativeCaptive, respectively), and can exceed 1, indicating that all individuals were infected at least once during the fall season, for a particular alternative applied to a particular context. Persistence is a logical condition indicating if equilibrium determined by SIRS ODE equations and `run_steady()` from the rootSolve package predicts at least 1 in 1,000 deer infected at equilibrium for wild and captive deer (persistWild and persistCaptive, respectively), for a particular alternative applied to a particular context.
#'
#' @format `alternatives_across_systems`
#'
#'
#' @source Derived from `Management_Alternatives_Systems.Rmd`
#'
"alternatives_across_systems"

#' cross_sector_results
#'
#' A dataframe containing results used to compare cumulative influences of the top performing alternative action for each OneHealth sectors (public health, agriculture, and wildlife sectors) on the prevalence, persistence and cumulative infections of SARS-CoV-2 in simulated white-tailed deer populations. 200 iterations (run_id) were run for captive and wild populations of white-tailed deer, separated by a single fenceline that allows proximity and direct contact between captive and wild deer (Complex), under the alternative identified for public health, agriculture, and wildlife (Alternative) that have the greatest reduction in prevalence, persistence, and cumulative infections. Complexes include captive deer in outdoor ranch facilities and wild deer in rural areas (Outdoor ranch and rural deer), captive deer in outdoor ranch facilities and wild deer in suburban areas (Outdoor ranch and suburban deer), captive deer in intensive facilities and wild deer in rural areas (Intensive facility and rural deer), and captive deer in intensive facilities and wild deer in suburban areas (Intensive facility and suburban deer). Cross-sector collaboration would vaccinate and boost captive deer (agriculture sector), require personal protective equipment for humans interacting with deer in wild and captive settings (public health sector), and eliminate baiting practices through legislation, enforcement, and education (wildlife sector).
#' Wild and captive prevalence (meanWild and meanCaptive, respectively) are reported as the percent of a population infected, averaged over a simulated 120-day projection (run_id), for a particular alternative applied to a particular context. Cumulative infections reports the total proportion of the wild and captive populations infected over the course of the 120-fall projection (cumulativeWild and cumulativeCaptive, respectively), and can exceed 1, indicating that all individuals were infected at least once during the fall season, for a particular alternative applied to a particular context. Persistence is a logical condition indicating if equilibrium determined by SIRS ODE equations and `run_steady()` from the rootSolve package predicts at least 1 in 1,000 deer infected at equilibrium for wild and captive deer (persistWild and persistCaptive, respectively), for a particular alternative applied to a particular context.
#'
#' @format `cross_sector_results`
#'
#'
#' @source Derived from `Management_Alternatives_Systems.Rmd`, and compared to single alternative impacts stored in `alternatives_across_systems.rda`
#'
"cross_sector_results"

#' initial_infection_results_1_in_1000
#'
#' A dataframe containing results used to compare degrees of introduction, spread, prevalence, persistence and cumulative infections of SARS-CoV-2 in simulated white-tailed deer populations with an initial infection of 1 in 1000 deer on day 0. These results do not include continuous human introduction during the 120-day fall simulation, and thus represent the risk of single introductions to spawn persisting infections in white-tailed deer. 200 iterations (run_id) were run for each scenario (Context). Scenarios included captive deer in outdoor ranch facilities, captive deer in intensive facilities, wild deer in rural areas, and wild deer in suburban areas. Setting indicates whether deer were captive or wild. r0, or basic reproductive number, indicates the number of secondary infections caused by a single infectious deer over the course of it's infection. Prevalence is the percent of a population infected, averaged over a simulated 120-day projection. Persistence is a logical condition indicating if equilibrium determined by SIRS ODE equations and `run_steady()` from the rootSolve package predicts at least 1 in 1,000 deer infected at equilibrium. Cumulative infections reports the total proportion of the population infected over the course of the 120-fall projection, and can exceed 1, indicating that all individuals were infected at least once during the fall season.
#'
#' @format `initial_infection_results_1_in_1000`
#'
#'
#' @source Derived from `SIRS_analysis_by_contexts_initialspill.Rmd`, to be compared with continual introduction results from `SIRS_analysis_by_contexts.Rmd`
#'
"initial_infection_results_1_in_1000"

#' initial_infection_results_1_in_1mil
#'
#' A dataframe containing results used to compare degrees of introduction, spread, prevalence, persistence and cumulative infections of SARS-CoV-2 in simulated white-tailed deer populations with an initial infection of 1 in 1 million deer on day 0. These results do not include continuous human introduction during the 120-day fall simulation, and thus represent the risk of single introductions to spawn persisting infections in white-tailed deer. 200 iterations (run_id) were run for each scenario (Context). Scenarios included captive deer in outdoor ranch facilities, captive deer in intensive facilities, wild deer in rural areas, and wild deer in suburban areas. Setting indicates whether deer were captive or wild. r0, or basic reproductive number, indicates the number of secondary infections caused by a single infectious deer over the course of it's infection. Prevalence is the percent of a population infected, averaged over a simulated 120-day projection. Persistence is a logical condition indicating if equilibrium determined by SIRS ODE equations and `run_steady()` from the rootSolve package predicts at least 1 in 1,000 deer infected at equilibrium. Cumulative infections reports the total proportion of the population infected over the course of the 120-fall projection, and can exceed 1, indicating that all individuals were infected at least once during the fall season.
#'
#' @format `initial_infection_results_1_in_1mil`
#'
#'
#' @source Derived from `SIRS_analysis_by_contexts_initialspill.Rmd`, to be compared with continual introduction results from `SIRS_analysis_by_contexts.Rmd`
#'
"initial_infection_results_1_in_1mil"

#' initial_infection_results_1_in_1bil
#'
#' A dataframe containing results used to compare degrees of introduction, spread, prevalence, persistence and cumulative infections of SARS-CoV-2 in simulated white-tailed deer populations with an initial infection of 1 in 1 billion deer on day 0. These results do not include continuous human introduction during the 120-day fall simulation, and thus represent the risk of single introductions to spawn persisting infections in white-tailed deer. 200 iterations (run_id) were run for each scenario (Context). Scenarios included captive deer in outdoor ranch facilities, captive deer in intensive facilities, wild deer in rural areas, and wild deer in suburban areas. Setting indicates whether deer were captive or wild. r0, or basic reproductive number, indicates the number of secondary infections caused by a single infectious deer over the course of it's infection. Prevalence is the percent of a population infected, averaged over a simulated 120-day projection. Persistence is a logical condition indicating if equilibrium determined by SIRS ODE equations and `run_steady()` from the rootSolve package predicts at least 1 in 1,000 deer infected at equilibrium. Cumulative infections reports the total proportion of the population infected over the course of the 120-fall projection, and can exceed 1, indicating that all individuals were infected at least once during the fall season.
#'
#' @format `initial_infection_results_1_in_1bil`
#'
#'
#' @source Derived from `SIRS_analysis_by_contexts_initialspill.Rmd`, to be compared with continual introduction results from `SIRS_analysis_by_contexts.Rmd`
#'
"initial_infection_results_1_in_1bil"
