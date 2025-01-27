
<!-- README.md is generated from README.Rmd. Please edit that file -->

# whitetailedSIRS: A package to project SARS-CoV-2 outbreak dynamics in white-tailed deer

## Authors

Elias Rosenblatt (<erosenbl@uvm.edu>), Rubenstein School of Environment
and Natural Resources, 81 Carrigan Drive, Burlington, VT, USA

F. Javiera Rudolph, Department of Ecosystem Sciences and Management,
Pennsylvania State University, University Park, PA, USA; U.S. Geological
Survey, Eastern Ecological Science Center, Laurel, MD, USA

Fernando Arce, Department of Environmental Conservation, University of
Massachusetts, Amherst, MA, USA

Jonathan D. Cook, U.S. Geological Survey, Eastern Ecological Science
Center, Laurel, MD, USA

Graziella V. DiRenzo, U. S. Geological Survey, Massachusetts Cooperative
Fish and Wildlife Research Unit, University of Massachusetts, Amherst,
MA, USA; Department of Environmental Conservation, University of
Massachusetts, Amherst, MA, USA

Evan H.C. Grant, U.S. Geological Survey, Eastern Ecological Science
Center, Turner’s Falls, Massachusetts, USA

Michael C. Runge, U.S. Geological Survey, Eastern Ecological Science
Center, Laurel, MD, USA

Brittany A. Mosher, Rubenstein School of Environment and Natural
Resources, 81 Carrigan Drive, Burlington, VT, USA

<!-- badges: start -->
<!-- badges: end -->

## Information

Repository Type: Program R scripts

Year of Origin: 2023

Year of Version: 2023

Version: 1.0.0

Digital Object Identifier (DOI): <https://doi.org/10.5066/P9TZK938>

USGS Information Product Data System (IPDS) no.: IP-155118

## Suggested Citation for Software

Rosenblatt, E, Rudolph, J.F., and Arce, F., Cook, J. D., DiRenzo, G.V.,
Grant, E.H.C., Runge, M.C., and Mosher, B.A.. whitetailedSIRS: A package
to project SARS-CoV-2 outbreak dynamics in white-tailed deer. Version
1.0.0: U.S. Geological Survey software release,
<https://doi.org/10.5066/P9TZK938>

## Download package

You can download the `whitetailedSIRS` package directly by using the
following code:

``` r
# Install githubinstall and devtools packages if not installed already
install.packages("githubinstall")
install.packages("devtools")

# Load the packages
library(githubinstall)
library(devtools)

# Function to download the package
devtools::install_github("disease-decision-analysis-and-research/whitetailedSIRS")
```

## Abstract

This software release contains several R scripts that generate epidemic
projections of SARS-CoV-2 in white tailed deer populations using a
**S**usceptible-**I**nfected-**R**ecovered-**S**usceptible (SIRS)
modeling framework. We provide a workflow of vignettes used in
Rosenblatt et al. In Prep and Cook et al. In Prep. Users are able to
specify transmission parameters for human-deer and deer-deer
transmission to quantify changes in outbreak dynamics. The outputs of
this package are ggplot friendly, and for the most part, we use a
list-column workflow when working with multiple simulations. Details on
the model can be found in the vignettes.

## Acknowledgements

We thank Daniel Walsh, Susan Shriner, and Kim Pepin for their continued
feedback as part of a broader decision analysis detailed in Cook et
al. In Prep and Rosenblatt et al. In Prep. This work was supported by
the Coronavirus Aid, Relief, and Economic Security Act (P.L. 116-136).

## Vignettes

### Vignette 1: Introduction.Rmd

Data inputs: N/A

Details: A introductory vignette detailing the SIRS ODE equations and
corresponding functions, used in Rosenblatt et al. In prep and Cook et
al. In Prep.

Outputs: N/A

### Vignette 2: sir_model_description.Rmd

Data inputs: N/A

Details: A vignette introducing and detailing the SIRS ODE equations and
corresponding functions, used in Rosenblatt et al. In prep and Cook et
al. In Prep.

Outputs: N/A

### Vignette 3: example_values.Rmd

Data inputs: N/A

Details: A vignette stepping through the suggested work-flow when using
the `whitetailedSIRS` package. Other vignettes use the same work-flow,
so this vignette is useful for new users to understand how to replicate
or modify the analysis presented.

Outputs: N/A

### Vignette 4: SIRS_analysis_by_context.Rmd

Data inputs: N/A

Details: This vignette details the analysis used to study differences in
outbreak dynamics in four scenarios of SARS-CoV-2 transmission to and
among white-tailed deer populations. The output of this vignette is used
to visualize differences in the vignette
`whitetailedSIRS::Visualize_by_context`. This analysis corresponds with
Objectives 1-3 of *Rosenblatt et al. In Prep*.

Outputs: N/A

### Vignette 5: SIRS_analysis_by_context_initialspill.Rmd

Data inputs: N/A

Details: This vignette follows the same blueprint as
`whitetailedSIRS::SIRS_analysis_by_context`. The primary difference here
is that initial infected compartment sizes are set non-zero values and
human prevalence is set to zero. This mimics an initial spillover event
of a given magnitude, to test how outbreak dynamics differ from
continuous spillover detailed in
`whitetailedSIRS::SIRS_analysis_by_context`. These differences are
visualized in the vignette `whitetailedSIRS::Visualize_by_context`. Most
of the simulation code is suppresed in the rendered vignette, but can be
viewed in the .Rmd file. This analysis corresponds with Objective 4 of
*Rosenblatt et al. In Prep*.

Outputs: N/A

### Vignette 6: Visualize_by_context.Rmd

Data inputs: data/scenario_results.rda; data/scenario_projections.rda

Details: This vignette demonstrates how to visualize the differences in
various characteristics of outbreaks between scenarios. These figures
are used in the results published in *Rosenblatt et al. In Prep.*.

Outputs: N/A

### Vignette 7: Connected_Systems.Rmd

Data inputs: data/scenario_results.rda

Details: This vignette tests how connected systems (wild and captive
separated by a fence line) differ in outbreak characteristics, compared
to isolated scenario. When these scenarios are connected, there may be
differences in prevalence, cumulative cases, and persistence due to
fence line interactions. This analysis corresponds with Objective 5 of
*Rosenblatt et al. In Prep*.

Outputs: N/A

### Vignette 8: Management_Alternatives_Systems.Rmd

Data inputs: N/A

Details: This vignette runs outbreak simulations to estimate the effects
of various management alternatives on the dynamics of a SARS-CoV-2
outbreak in wild and captive white-tailed deer. We considered
captive:wild systems that are separated by a fence. These management
alternatives may be implemented in both wild and captive populations, or
in one scenario. We focus on 11 alternatives, ranging across
agriculture, public health, and wildlife sectors. These alternatives are
detailed in Cook et al. In Prep..

Outputs: N/A

## Functions

### Function 1: draw_elicitation_samples.R

Data inputs: elicitation_data.rda

Details: R function that samples a user-specified number of values from
expert-elicited parameter estimates used in Rosenblatt et al. In Prep
and Cook et al. In Prep.

Outputs: A data frame listing expert-elicited parameters, error
distribution characteristics (mean, standard deviation, and family) and
user-specified number of random samples from the error distribution.

### Function 2: get_EE_param_vals.R

Data inputs: Output object from elicitation_data.rda

Details: R function that prepare random draws from elicitation_data.rda
for inputs in the SIRS ODE equations.

Outputs: A vector object containing drawn values for an expert-elicited
parameter, with a length specified by the user.

### Function 3: calc_contact_rate.R

Data inputs: N/A

Details: R function that uses proximity rate model developed by Habib et
al. (2011) to estimate proximity rates for deer in wild or wild-like
captive settings, conditional on density and habitat availability. This
function can also be used for captive settings with conditions that
result in identical deer-deer proximity rates.

Outputs: A vector object containing drawn deer-deer proximity rate
values (proximity events per day), with a length specified by the user.

### Function 4: calc_sigma_aero.R

Data inputs: N/A

Details: R function that calculates probability of infection from
aerosol transmission of SARS-CoV-2. User can specify number of
probabilities generated for this derived parameter, an specific
parameters influencing these probabilities (e.g. duration of proximity,
dose-response, etc.).

Outputs: A vector object containing drawn infection probabilities for
aerosol transmission, with a length specified by the user.

### Function 5: calc_sigma_dc.R

Data inputs: N/A

Details: R function that calculates probability of infection from fluid
transmission of SARS-CoV-2. User can specify number of probabilities
generated for this derived parameter, an specific parameters influencing
these probabilities (e.g. duration of proximity, dose-response, etc.).

Outputs: A vector object containing drawn infection probabilities for
fluid transmission, with a length specified by the user.

### Function 6: initial_compartments.R

Data inputs: N/A

Details: R function that populates a list of initial Susceptible (S) -
Infected (I) - Recovered (R) compartment sizes. The function has an
argument to format the output to allow the calculation of the size of
each compartment at equilibrium (steady = TRUE), or to format the output
to allow the calculation of cumulative infections over the course of a
projected outbreak (steady = FALSE). These two process cannot be done in
the same execution of the function, but rather be run separately. The
length of each vector object in the list is determined by the user. The
formatted output is ready to be fed into the run.R function, which uses
Epi_sirs.R and Epi_sirs_with_cumulative.R functions to project how these
compartment sizes change through a projection.

Outputs: A list object containing vector objects containing the starting
size of each SIR compartment. If steady = FALSE, the list includes 2
additional vectors “I_wild_cumulative” and “I_captive_cumulative”. This
list object is prepared to be fed into the run.R function.

### Function 7: alternative.R

Data inputs: N/A

Details: R function that helps to prepare parameters used to solve SIRS
ODE equations for both wild and captive deer, to be fed into the params
argument of the run.R function. 15 parameters must be defined with
lengths equal to the number of simulations run. This function takes
these parameter inputs and calculates derived transmission rate
parameters for use with the ODE equations.

Outputs: A list of vector objects containing estimates for derived
parameters, for use with the run.R function.

### Function 8: run.R

Data inputs: N/A

Details: R function that Run ODE Solver for SIRS model, using parameter
estimates (output from alternatives.R), initial compartment sizes for
projection (output from initial_compartments.R with argument steady =
FALSE) and initial compartment sizes formatted for steady state
calculation (output from initial_compartments.R with argument steady =
TRUE).

Outputs: A list containing a run identifier (run_id), initial
compartment sizes for projection (inits.fall), initial compartment sizes
for steady state calculation (inits.steady), parameter values used for
each run (params), compartment sizes for each time step in reach run
(ode_proj), compartment sizes at steady-state equilibrium
(steady_state), and the scenario label for each run_id (Context). This
output is used for all visualization and table summaries. For results
from Rosenblatt et al. In Prep, this output is stored in the package in
scenario_results.rda.

### Function 9: simple_sirs.R

Data inputs: Outputs from alternatives.R and initial_compartments.R.

Details: R function that defines the ODE SIR equations to work alongside
the `deSolve::ode()` function, and it will return an object with the
proportion of individuals found in each of the SIR compartments at each
of the specified time points. This function is used in this package to
calculate persistence, or the steady state equilibrium (via
`rootSolve::run_steady()`), along with how compartment sizes change
through a projection.

Outputs: N/A

### Function 10: simple_sirs_with_cumulative.R

Data inputs: Outputs from alternatives.R and initial_compartments.R.

Details: Similar to `whitetailedSIRS::simple_sirs`, this R function
defines the ODE SIR equations to work alongside the `deSolve::ode()`
function, and it will return an object with the proportion of
individuals found in each of the SIR compartments at each of the
specified time points. An added feature is that this function has ODE
equations to calculate the cumulative infections from day 0 to day t.

Outputs: N/A

## References

Cook, J.D., E. Rosenblatt, G.V. Direnzo, E.H.C. Grant, B.A. Mosher, F.
Arce, S. Christensen, R. Ghai, M.C. Runge. In Prep. Using decision
science to evaluate the risk and management of SARS-CoV-2 zoonotic
transmission between humans and white-tailed deer.

Habib, T.J., Merrill, E.H., Pybus, M.J. and Coltman, D.W., 2011.
Modelling landscape effects on density–contact rate relationships of
deer in eastern Alberta: implications for chronic wasting disease.
Ecological Modelling, 222(15), pp.2722-2732.

Rosenblatt, E.G., Cook, J.D., DiRenzo, G.V., Grant, E.H.C., Arce, F.,
Pepin K.M., Rudolph, F.J., Runge, M.C., Shriner, S., Walsh, D.P., and
Mosher B.A. (2023). Epidemiological modeling of SARS-CoV-2 in
white-tailed deer (Odocoileus virginianus) reveals conditions for
introduction and widespread transmission. bioRxiv 2023.08.30.555493;
doi: <https://doi.org/10.1101/2023.08.30.555493>.
