# I am writing this script to test and make sure that my sir function gives the same results as those from Elias and Fer


# below, I copied all functions from the previous version of the package
# after the functions I have sections of code to bring in expert elicitation
# I use these sets of parameters to test if both give the same:

# sars2deer -------------------

contactRate <- function(seasonal.adj = 1, c, N, A = 100, q){
   prox <-  seasonal.adj*c*((N^(1-q))/A)
   return(as.numeric(prox))
}

pinfectAerosol <- function(exhalationRate, inhalationRate,sputumConc, quantaConvo, volDroplet, airRefresh, settleRate, decayRate, airVol, contactDuration, r)
{
   dose <- inhalationRate*(sputumConc*quantaConvo*exhalationRate*volDroplet)/((airRefresh+settleRate+decayRate)*airVol)*contactDuration
   v.aero <- 1-exp(-r*dose)
   return(as.numeric(v.aero))
}

pinfectDC <- function(volTrans, sputumConc, pfuConvo){
   dose <- volTrans*sputumConc*pfuConvo
   v.dc <- 1-exp(-(dose/410))
   return(as.numeric(v.dc))
}


eliPars <- function(viroEli,deerEli, seed = 123){
   set.seed(seed)
   #Random draws of elicited virus parameters
   alphaDeer <- rlnorm(runs, as.numeric(viroEli[viroEli$Parameter == 'Temporary Immunity', 'meanlog']), as.numeric(viroEli[viroEli$Parameter == 'Temporary Immunity', 'sdlog'])) #Temporary immunity (days)
   viralLoad <- rlnorm(runs,as.numeric(viroEli[viroEli$Parameter == 'Viral Load', 'meanlog']), as.numeric(viroEli[viroEli$Parameter == 'Viral Load', 'sdlog'])) #ratio deer:human sputum viral load
   doseResponse <- rlnorm(runs,as.numeric(viroEli[viroEli$Parameter == 'Dose-Response', 'meanlog']), as.numeric(viroEli[viroEli$Parameter == 'Dose-Response', 'sdlog'])) #wells-riley dose response value in deer
   #Random draws of elicited deer contact parameters
   ## deer-deer interactions
   deerContDur <- rlnorm(runs,as.numeric(deerEli[deerEli$Parameter == 'Deer Proximity Duration (minutes)', 'mean']), as.numeric(deerEli[deerEli$Parameter == 'Deer Proximity Duration (minutes)', 'sd'])) #Deer-deer proximity duration (minutes)
   deerProbCont <- greybox::rlogitnorm(runs,as.numeric(deerEli[deerEli$Parameter == 'Direct Contact Probability', 'mean']), as.numeric(deerEli[deerEli$Parameter == 'Direct Contact Probability', 'sd'])) #Deer-deer direct contact probability
   deerProximityBait <- rlnorm(runs,as.numeric(deerEli[deerEli$Parameter == 'Proximity rate with baiting (17 events without baiting)', 'mean']), as.numeric(deerEli[deerEli$Parameter == 'Proximity rate with baiting (17 events without baiting)', 'sd'])) #Proximity events per day with baiting (reference level is 17 events without baiting)
   ## deer-human interactions in rural setting
   deerHumanProxRateRural <- rlnorm(runs,as.numeric(deerEli[deerEli$Parameter == 'Deer-Human Proximity Rate, Rural (per 120 days)', 'mean']), as.numeric(deerEli[deerEli$Parameter == 'Deer-Human Proximity Rate, Rural (per 120 days)', 'sd'])) #Human-deer proximity rate (per 120 days) in rural conditions
   deerHumanDurRural <- rlnorm(runs,as.numeric(deerEli[deerEli$Parameter == 'Deer-Human Proximity Duration, Rural (minutes)', 'mean']), as.numeric(deerEli[deerEli$Parameter == 'Deer-Human Proximity Duration, Rural (minutes)', 'sd'])) #Duration of human-deer proximity event in rural conditions (minutes)
   ## deer-human suburban
   deerHumanProxRateSub <- rlnorm(runs,as.numeric(deerEli[deerEli$Parameter == 'Deer-Human Proximity Rate, Suburban (per 120 days)', 'mean']), as.numeric(deerEli[deerEli$Parameter == 'Deer-Human Proximity Rate, Suburban (per 120 days)', 'sd']))#Human-deer proximity rate (per 120 days) in suburban conditions
   deerHumanDurSub <- rlnorm(runs,as.numeric(deerEli[7,2]), as.numeric(deerEli[7,3])) #Duration of human-deer proximity event in suburban conditions (minutes)
   ## deer-human Captive
   deerHumanProxRateCapt <- rlnorm(runs,as.numeric(deerEli[deerEli$Parameter == 'Deer-Human Proximity Rate, Captive (per 120 days)', 'mean']), as.numeric(deerEli[deerEli$Parameter == 'Deer-Human Proximity Rate, Captive (per 120 days)', 'sd'])) #Human-deer proximity rate (per 120 days) in captive conditions
   deerHumanDurCapt <- rlnorm(runs,as.numeric(deerEli[deerEli$Parameter == 'Deer-Human Proximity Duration, Captive (minutes)', 'mean']), as.numeric(deerEli[deerEli$Parameter == 'Deer-Human Proximity Duration, Captive (minutes)', 'sd'])) #Duration of human-deer proximity event in captive conditions (minutes)
   deerdeerProxRateCapt <- rlnorm(runs,as.numeric(deerEli[deerEli$Parameter == 'Deer-Deer Proximity Rate, Captive (per day)', 'mean']), as.numeric(deerEli[deerEli$Parameter == 'Deer-Deer Proximity Rate, Captive (per day)', 'sd'])) # Deer-deer proximity rate in captivity (per day)

   return(data.frame(alphaDeer = alphaDeer,viralLoad = viralLoad,doseResponse = doseResponse,
                     deerContDur = deerContDur,deerProbCont = deerProbCont,
                     deerProximityBait = deerProximityBait,
                     deerHumanProxRateRural = deerHumanProxRateRural, deerHumanDurRural = deerHumanDurRural,
                     deerHumanProxRateSub = deerHumanProxRateSub, deerHumanDurSub = deerHumanDurSub,
                     deerHumanProxRateCapt = deerHumanProxRateCapt,deerHumanDurCapt = deerHumanDurCapt,
                     deerdeerProxRateCapt = deerdeerProxRateCapt))
}


parmsWeigth <- function(runs,
                        c = rep(16.37,runs),
                        q = rep(0.53,runs),
                        area = rep(100,runs),
                        N = rpois(runs,1000),
                        IR = rep(0.846,runs),
                        IR.human = rep(0.53,runs),
                        cv = 10^5.6,
                        ci = 0.0014,
                        Vd = rep(0.009,runs),
                        AER = rep(4,runs),
                        k = rep(0.24,runs),
                        λ = rep(0.63,runs),
                        V = rep(7.07,runs),
                        tcontact,
                        tcontact.wild.human,
                        tcontact.captive.human,
                        r.deer,
                        Vdc = rep(0.1, runs),
                        Vdc.human = rep(0.1,runs),
                        Cv = 10^5.6,
                        pfu.trans = rep(1/10^5.2, runs),
                        alphaD,
                        sigma.dc.wild,
                        sigma.dc.captive,
                        sigma.dc.wild.human  = rep(0,runs),
                        sigma.dc.captive.human = rep(0,runs),
                        I.human.prop = rep(.05,runs),
                        Phi.cw = rep(0, runs),
                        Phi.wc = rep(0, runs),
                        gamma= rep(1/6,runs),
                        c.fence,
                        c.captive,
                        c.wild.human,
                        c.captive.human,
                        S.wild = rep(1,runs),
                        I.wild = rep(0,runs),
                        R.wild = rep(0,runs),
                        S.captive = rep(1,runs),
                        I.captive = rep(0,runs),
                        R.captive = rep(0,runs)){

   parms <- data.frame(c, q, area, N, IR, IR.human, cv, ci, Vd, AER, k, λ, V, tcontact, tcontact.wild.human, tcontact.captive.human,r.deer,
                       Vdc, Vdc.human, Cv, pfu.trans, alphaD, sigma.dc.wild, sigma.dc.captive, sigma.dc.wild.human, sigma.dc.captive.human, I.human.prop, Phi.cw, Phi.wc, gamma, c.fence, c.captive, c.wild.human, c.captive.human, S.wild, I.wild, R.wild, S.captive, I.captive, R.captive)
   names(parms) <- c('c', 'q', 'area', 'N', 'IR', 'IR.human', 'cv', 'ci', 'Vd', 'AER', 'k', 'λ', 'V', 'tcontact', 'tcontact.wild.human','tcontact.captive.human', "r.deer",
                     'Vdc', 'Vdc.human', 'Cv', 'pfu.trans', 'alphaD', 'sigma.dc.wild', 'sigma.dc.captive', 'sigma.dc.wild.human', 'sigma.dc.captive.human',
                     'I.human.prop', 'Phi.cw', 'Phi.wc', 'gamma', 'c.fence', 'c.captive', 'c.wild.human', 'c.captive.human', 'S.wild', 'I.wild', 'R.wild', 'S.captive', 'I.captive', 'R.captive')
   return(parms)
}

parmsSIR <- function(parms, i = 1){
   ####Step 1: Group parameters needed for derived parameters
   contact_parameters <- c(
      c = parms[i, 1], #contact rate
      q = parms[i, 2],  #intermediate rate between density- and frequency dependence
      Area = parms[i, 3], #Area where N individuals exist
      N = parms[i,4] #N size for contact rates
   )
   aerosol_parameters <- c(
      IR = parms[i,5], #Inhalation/Exhalation Rate for deer (m3/hr)
      IR.human = parms[i, 6],#Inhalation/Exhalation Rate for a human(m3/hr)
      cv = parms[i, 7], #SARS-CoV-2 concentration in sputum
      ci = parms[i, 8], # Quanta conversion from RNA copies, assume same for humans and deer (quanta/RNA)
      Vd = parms[i, 9], # Droplet volume, assume same for humans and deer (ml/m3)
      AER = parms[i, 10], # Air refresh rate (hr-1)
      k = parms[i, 11], # Aerosol settling rate (hr-1)
      λ = parms[i, 12], # Aerosol decay rate (hr-1)
      V = parms[i, 13], # Shared airspace volume (m3)
      tcontact = parms[i, 14]/60, #Time airspace is shared (hr; expressed as x minutes/60 minutes)
      tcontact.wild.human = parms[i, 15]/60, #Time airspace is shared with a human (hr; expressed as x minutes/60 minutes)
      tcontact.captive.human = parms[i, 16]/60, #Time airspace is shared with a human (hr; expressed as x minutes/60 minutes)
      r.deer = parms[i, 17] #Aerosolized dose response for deer
   )

   dc_parameters <- c(
      Vdc = parms[i, 18], # volume of fluid transferred at contact, deer-deer
      Vdc.human = parms[i, 19], # volume of fluid transferred at contact, human-deer
      Cv = parms[i, 20], # Concentration of SARS-CoV-2 in sputum (genomic copies per ml)
      pfu.trans = parms[i, 21] # Conversion from genomic copies (gc) to plaque-forming units (PFU)
   )

   transmission_parameters <- c(
      alphaD = 1/parms[i,22], # loss of immunity (1/days of temporary immunity)
      sigma.dc.wild = parms[i,23], # proportion proximity events that result in physical contacts
      sigma.dc.captive = parms[i,24], # proportion proximity events that result in physical contacts
      sigma.dc.wild.human = parms[i,25], # proportion proximity events that result in physical contacts
      sigma.dc.captive.human = parms[i,26], #proportion proximity events that result in physical contacts
      I.human.prop = parms[i,27], #human prevalence
      Phi.cw = parms[i, 28] ,  # escape parameter between captive and wild
      Phi.wc = parms[i, 29] ,  # escape parameter between wild and captive
      gamma=parms[i, 30], #recovery rate (1/days infectious)
      c.fence = parms[i, 31], #deer-deer fenceline proximity rate
      c.captive = parms[i, 32], #deer-deer proximity rate in captivity
      c.wild.human = parms[i, 33]/120, #Human-deer proximity rate in wild
      c.captive.human = parms[i, 34]/120 #Human-deer proximity rate in captivity
   )


   ####Step 2: Calculate derived parameters
   ##Deer-deer wild proximity Rate calculation
   c.wild <-  contactRate(c = contact_parameters['c'], N = contact_parameters['N'],
                          A = contact_parameters['Area'], q = contact_parameters['q'])
   ##Aerosol infection probabilities
   #Deer-deer aerosol transmission probability
   v.aero <- pinfectAerosol(exhalationRate = aerosol_parameters['IR'],inhalationRate = aerosol_parameters['IR'], sputumConc = aerosol_parameters['cv'], quantaConvo = aerosol_parameters['ci'], volDroplet = aerosol_parameters['Vd'], airRefresh = aerosol_parameters['AER'], settleRate = aerosol_parameters['k'], decayRate = aerosol_parameters['λ'], airVol = aerosol_parameters['V'], contactDuration = aerosol_parameters['tcontact'], r = aerosol_parameters['r.deer'])

   #Human - wild deer aerosol transmission probability
   v.aero.wild.human <- pinfectAerosol(exhalationRate = aerosol_parameters['IR.human'],inhalationRate = aerosol_parameters['IR'], sputumConc = aerosol_parameters['cv'], quantaConvo = aerosol_parameters['ci'], volDroplet = aerosol_parameters['Vd'], airRefresh = aerosol_parameters['AER'], settleRate = aerosol_parameters['k'], decayRate = aerosol_parameters['λ'], airVol = aerosol_parameters['V'], contactDuration = aerosol_parameters['tcontact.wild.human'], r = aerosol_parameters['r.deer'])

   #Human - captive deer aerosol transmission probability
   v.aero.captive.human <- pinfectAerosol(exhalationRate = aerosol_parameters['IR.human'],inhalationRate = aerosol_parameters['IR'], sputumConc = aerosol_parameters['cv'], quantaConvo = aerosol_parameters['ci'], volDroplet = aerosol_parameters['Vd'], airRefresh = aerosol_parameters['AER'], settleRate = aerosol_parameters['k'], decayRate = aerosol_parameters['λ'], airVol = aerosol_parameters['V'], contactDuration = aerosol_parameters['tcontact.captive.human'], r = aerosol_parameters['r.deer'])

   ##Direct contact infection probabilities
   #Deer - deer direct contact infection probability
   v.dc <- pinfectDC(volTrans = dc_parameters['Vdc'], sputumConc = dc_parameters['Cv'], pfuConvo = dc_parameters['pfu.trans'])
   #Human - deer direct contact infection probability
   v.dc.human <- pinfectDC(volTrans = dc_parameters['Vdc.human'], sputumConc = dc_parameters['Cv'], pfuConvo = dc_parameters['pfu.trans'])

   ####Step 3: Compile derived parameters with transmission parameters
   transmission_parameters <- c(transmission_parameters,
                                c.wild = c.wild, #Contact rate with humans in the wild, calculated above
                                v.aero=v.aero, #aerosolized dose, calculated above
                                v.aero.wild.human = v.aero.wild.human, #Aerosol infection probability with humans in wild, calculated above
                                v.aero.captive.human = v.aero.captive.human, #Aerosol infection probability with humans in captive facilities, calculated above
                                v.dc=v.dc, #direct contact infection probability with deer, calculated above
                                v.dc.human = v.dc.human) #direct contact infection probability with human, calculated above

   return(transmission_parameters)
}


derivedParms <- function(parms){
   params <- split(parms, seq(nrow(parms)))
   transPar <- vector('list', length(params))
   for (j in 1:length(params)){
      transPar[[j]] <- as.data.frame(t(parmsSIR(parms = params[[j]])))
   }
   derivedParms <- as.data.frame(do.call(rbind, transPar))
   transPar <- split(derivedParms, seq(nrow(derivedParms)))
   return(transPar)
}

SIRS <- function(time, state, parameters) {
   with(as.list(c(state, parameters)), {
      dS.wild <- alphaD*R.wild -
         (S.wild*((c.wild*v.aero*I.wild)+(c.wild*sigma.dc.wild*v.dc*I.wild)+(c.fence*v.aero*I.captive)+(c.fence*sigma.dc.wild*v.dc*I.captive)+(c.wild.human*v.aero.wild.human*I.human.prop))) +
         (Phi.cw*S.captive) -
         (Phi.wc*S.wild)

      dI.wild <- (S.wild*((c.wild*v.aero*I.wild)+(c.wild*sigma.dc.wild*v.dc*I.wild)+(c.fence*v.aero*I.captive)+(c.fence*sigma.dc.wild*v.dc*I.captive)+(c.wild.human*v.aero.wild.human*I.human.prop))) +
         (Phi.cw*I.captive) -
         (Phi.wc*I.wild) -
         (gamma*I.wild)

      dR.wild <- (Phi.cw*R.captive) -
         (Phi.wc*R.wild) +
         (gamma*I.wild) -
         (alphaD*R.wild)

      dS.captive <- alphaD*R.captive -
         (S.captive*((c.captive*v.aero*I.captive)+(c.captive*sigma.dc.captive*v.dc*I.captive)+(c.fence*v.aero*I.wild)+(c.fence*sigma.dc.captive*v.dc*I.wild)+(c.captive.human*v.aero.captive.human*I.human.prop))) -
         (Phi.cw*S.captive) +
         (Phi.wc*S.wild)

      dI.captive <- (S.captive*((c.captive*v.aero*I.captive)+(c.captive*sigma.dc.captive*v.dc*I.captive)+(c.fence*v.aero*I.wild)+(c.fence*sigma.dc.captive*v.dc*I.wild)+(c.captive.human*v.aero.captive.human*I.human.prop))) -
         (Phi.cw*I.captive) +
         (Phi.wc*I.wild) -
         (gamma*I.captive)

      dR.captive <- (gamma*I.captive) -
         (alphaD*R.captive) -
         (Phi.cw*R.captive) +
         (Phi.wc*R.wild)

      return(list(c(dS.wild, dI.wild, dR.wild, dS.captive, dI.captive, dR.captive)))
   })
}


# Load Params -------------------------
runs <- 50
viroEli <- readRDS('./data/virologyEEDistParameters.RDS') #Virology panel
deerEli <- readRDS('./data/DeerEEDistParameters.RDS') #Deer ecology panel
## function eliPars execute the random draw and keep it on appropiate
## shape for further procedures.
ePars <- eliPars(viroEli = viroEli,deerEli = deerEli)


parms <- parmsWeigth(runs,
                     area = rep(100,runs),
                     N = rpois(runs,1000),
                     cv = 10^5.6 * ePars$viralLoad,
                     tcontact = ePars$deerContDur,
                     tcontact.wild.human = ePars$deerHumanDurRural,
                     tcontact.captive.human = ePars$deerHumanDurCapt,
                     r.deer = ePars$doseResponse,
                     Cv = 10^5.6 * ePars$viralLoad,
                     alphaD = ePars$alphaDeer,
                     sigma.dc.wild = ePars$deerProbCont,
                     sigma.dc.captive = ePars$deerProbCont,
                     I.human.prop = rep(.05,runs),
                     c.fence = 0.00072/ePars$deerProbCont,
                     c.captive = ePars$deerdeerProxRateCapt,
                     c.wild.human = ePars$deerHumanProxRateRural,
                     c.captive.human = ePars$deerHumanProxRateCapt,
                     S.wild = rep(1,runs),
                     I.wild = rep(0,runs),
                     R.wild = rep(0,runs),
                     S.captive = rep(1,runs),
                     I.captive = rep(0,runs),
                     R.captive = rep(0,runs))



## We then split them in a list for further procedures and function calls
params <- split(parms, seq(nrow(parms)))

transPar <- derivedParms(parms = parms)

# Run 1 to compare ------------------

library(deSolve)
library(tidyverse)
library(cowplot)

j<-1
init <- c(S.wild = parms[j,'S.wild'], I.wild = parms[j,'I.wild'], R.wild = parms[j,'R.wild'],
          S.captive = parms[j,'S.captive'], I.captive = parms[j,'I.captive'], R.captive = parms[j,'R.captive'])
times = 0:120

fers_ode <- ode(y = init, time = times, parms = transPar[[1]], func = SIRS)

fers_ode %>%
   as_tibble() %>%
   pivot_longer(-time, names_to = "compartment", values_to = "proportion") %>%
   separate_wider_delim(compartment, delim = ".", names = c("sir_type", "pop_type")) %>%
   mutate(sir_type = factor(sir_type, levels = c("S", "I", "R")),
          pop_type = factor(pop_type, levels = c("wild", "captive"))) %>%
   ggplot(aes(x = time, y = proportion, color = sir_type, linetype = pop_type)) +
   geom_line() +
   labs(title = "SIR fer", y = "Proportion of population", x = "Time in days",
        color = "SIR", linetype = "Population type") +
   theme_bw() -> sir_fer
sir_fer

# test my version

library(whitetailedSIRS)

j<-1
inits <- c(
   S_wild = parms[j,'S.wild'],
   I_wild = parms[j,'I.wild'],
   R_wild = parms[j,'R.wild'],
   S_captive = parms[j,'S.captive'],
   I_captive = parms[j,'I.captive'],
   R_captive = parms[j,'R.captive'])

my_params <- c(alpha_immunity = transPar[[1]]$alphaD,
               beta_aero_ww = transPar[[1]]$c.wild * transPar[[1]]$v.aero,
               beta_aero_cw = transPar[[1]]$c.fence * transPar[[1]]$sigma.dc.wild * transPar[[1]]$v.dc,
               beta_aero_cc = transPar[[1]]$c.captive * transPar[[1]]$v.aero,
               beta_aero_hw = transPar[[1]]$c.wild.human * transPar[[1]]$v.aero.wild.human,
               beta_aero_hc = transPar[[1]]$c.captive.human * transPar[[1]]$v.aero.captive.human,
               beta_dc_ww = transPar[[1]]$c.wild * transPar[[1]]$sigma.dc.wild * transPar[[1]]$v.dc,
               beta_dc_cw = transPar[[1]]$c.fence * transPar[[1]]$sigma.dc.captive * transPar[[1]]$v.dc,
               beta_dc_cc = transPar[[1]]$c.captive * transPar[[1]]$sigma.dc.captive * transPar[[1]]$v.dc,
               phi_cw = transPar[[1]]$Phi.cw,
               phi_wc = transPar[[1]]$Phi.wc,
               gamma_recov = transPar[[1]]$gamma,
               I_human = transPar[[1]]$I.human.prop)

my_ode <- ode(y = inits, time = times, parms = my_params, func = simple_sirs)

my_ode %>%
   as_tibble() %>%
   pivot_longer(-time, names_to = "compartment", values_to = "proportion") %>%
   separate(compartment, sep = "_", c("sir_type", "pop_type")) %>%
   mutate(sir_type = factor(sir_type, levels = c("S", "I", "R")),
          pop_type = factor(pop_type, levels = c("wild", "captive"))) %>%
   ggplot(aes(x = time, y = proportion, color = sir_type, linetype = pop_type)) +
   geom_line() +
   labs(title = "SIR dynamics", y = "Proportion of population", x = "Time in days",
        color = "SIR", linetype = "Population type") +
   theme_bw() -> javi_sir
javi_sir

# see side by side:

plot_grid(sir_fer + theme(legend.position = "none"), javi_sir + theme(legend.position = "none"))

# great! they both do the same thing!!!
# So, this tells me the issue is not my SIR function, it is the way in which the parameters are being modified before getting into the function

# what if I get them out of the function?
alpha_immunity = transPar[[1]]$alphaD
beta_aero_ww = transPar[[1]]$c.wild * transPar[[1]]$v.aero
beta_aero_cw = transPar[[1]]$c.fence * transPar[[1]]$sigma.dc.wild * transPar[[1]]$v.dc
beta_aero_cc = transPar[[1]]$c.captive * transPar[[1]]$v.aero
beta_aero_hw = transPar[[1]]$c.wild.human * transPar[[1]]$v.aero.wild.human
beta_aero_hc = transPar[[1]]$c.captive.human * transPar[[1]]$v.aero.captive.human
beta_dc_ww = transPar[[1]]$c.wild * transPar[[1]]$sigma.dc.wild * transPar[[1]]$v.dc
beta_dc_cw = transPar[[1]]$c.fence * transPar[[1]]$sigma.dc.captive * transPar[[1]]$v.dc
beta_dc_cc = transPar[[1]]$c.captive * transPar[[1]]$sigma.dc.captive * transPar[[1]]$v.dc
phi_cw = transPar[[1]]$Phi.cw
phi_wc = transPar[[1]]$Phi.wc
gamma_recov = transPar[[1]]$gamma
I_human = transPar[[1]]$I.human.prop

my_params <- c(alpha_immunity,
               beta_aero_ww,
               beta_aero_cw,
               beta_aero_cc,
               beta_aero_hw,
               beta_aero_hc,
               beta_dc_ww,
               beta_dc_cw,
               beta_dc_cc,
               phi_cw,
               phi_wc,
               gamma_recov,
               I_human)

my_ode <- ode(y = inits, time = times, parms = my_params, func = simple_sirs)

my_ode %>%
   as_tibble() %>%
   pivot_longer(-time, names_to = "compartment", values_to = "proportion") %>%
   separate(compartment, sep = "_", c("sir_type", "pop_type")) %>%
   mutate(sir_type = factor(sir_type, levels = c("S", "I", "R")),
          pop_type = factor(pop_type, levels = c("wild", "captive"))) %>%
   ggplot(aes(x = time, y = proportion, color = sir_type, linetype = pop_type)) +
   geom_line() +
   labs(title = "SIR dynamics", y = "Proportion of population", x = "Time in days",
        color = "SIR", linetype = "Population type") +
   theme_bw() -> javi_sir
javi_sir

# same result! ok... functions work as they should.
# fix issue in how they are getting into the function then
# this is a matter of inputing the right parameters
