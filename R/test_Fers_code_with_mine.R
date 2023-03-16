library(whitetailedSIRS)
library(tidyverse)

set.seed(123)

j<-1
S_wild = parms[j,'S.wild']
I_wild = parms[j,'I.wild']
R_wild = parms[j,'R.wild']
S_captive = parms[j,'S.captive']
I_captive = parms[j,'I.captive']
R_captive = parms[j,'R.captive']

# parms comes from parmsweigh fx
parms[1,] %>% str()


c.wild <- contactRate(seasonal.adj = 1, c = parms$c[1], N = parms$N[1], A = 100, q = parms$q[1])
v.aero <- pinfectAerosol(exhalationRate = parms$IR[1],
                                    inhalationRate = parms$IR[1],
                                    sputumConc = parms$cv[1],
                                    quantaConvo = parms$ci[1],
                                    volDroplet = parms$Vd[1],
                                    airRefresh = parms$AER[1],
                                    settleRate = parms$k[1],
                                    decayRate = parms$λ[1],
                                    airVol = parms$V[1],
                                    contactDuration = parms$tcontact[1],
                                    r = parms$r.deer[1])

calc_nu_aero(C_nu = parms$cv[1], C_i = parms$ci[1], V_d = parms$Vd[1],
             t_contact = parms$tcontact[1], r = parms$r.deer[1])

v.aero.wild.human <- calc_nu_aero(ER = parms$IR.human[1], IR = parms$IR[1],
                                  C_nu = parms$cv[1], C_i = parms$ci[1], V_d = parms$Vd[1],
                                  t_contact = parms$tcontact.wild.human[1], r = parms$r.deer[1])


# pinfectAerosol(exhalationRate = parms[1,'IR.human'],inhalationRate = parms[1,'IR'], sputumConc = parms[1,'cv'], quantaConvo = parms[1,'ci'], volDroplet = parms[1,'Vd'], airRefresh = parms[1,'AER'], settleRate = parms[1,'k'], decayRate = parms[1,'λ'], airVol = parms[1,'V'], contactDuration = parms[1,'tcontact.wild.human'], r = parms[1,'r.deer'])

v.aero.captive.human <- pinfectAerosol(exhalationRate = parms[1,'IR.human'],inhalationRate = parms[1,'IR'], sputumConc = parms[1,'cv'], quantaConvo = parms[1,'ci'], volDroplet = parms[1,'Vd'], airRefresh = parms[1,'AER'], settleRate = parms[1,'k'], decayRate = parms[1,'λ'], airVol = parms[1,'V'], contactDuration = parms[1,'tcontact.captive.human'], r = parms[1,'r.deer'])


v.dc <- pinfectDC(volTrans = parms[1,'Vdc'], sputumConc = parms[1,'Cv'], pfuConvo = parms[1,'pfu.trans'])
calc_nu_dc(V_DC = volTrans, C_nu = sputumConc, pfu_conv = pfuConvo, k = 410)
#Human - deer direct contact infection probability
v.dc.human <- pinfectDC(volTrans = parms[1,'Vdc.human'], sputumConc = parms[1,'Cv'], pfuConvo = parms[1,'pfu.trans'])

sigma.dc.wild <- parms$sigma.dc.wild[1]
c.fence <- parms$c.fence[1]
c.wild.human <- parms$c.wild.human[1]
c.captive <- parms$c.captive[1]
sigma.dc.captive <- parms$sigma.dc.captive[1]
c.captive.human <- parms$c.captive.human[1]

beta_aero_ww = c.wild*v.aero
beta_dc_ww = c.wild*sigma.dc.wild*v.dc
beta_aero_cw = c.fence*v.aero
beta_dc_cw = c.fence*sigma.dc.wild*v.dc
beta_aero_hw = c.wild.human*v.aero.wild.human
beta_aero_cc = c.captive*v.aero
beta_dc_cc = c.captive*sigma.dc.captive*v.dc
beta_aero_hc = c.captive.human*v.aero.captive.human


# beta_aero_ww <- c_ww * nu_aero_deer_deer
# beta_aero_cw <- c_cw * nu_aero_deer_deer
# beta_aero_cc <- c_cc * nu_aero_deer_deer
# beta_aero_hw <- c_hw_rural * nu_aero_deer_human_rural
# beta_aero_hc <- c_hc * nu_aero_deer_human_capt
#
#
# beta_dc_ww <- c_ww * sigma_dc * nu_dc_deer_deer
# beta_dc_cw <- c_cw * sigma_dc * nu_dc_deer_deer
# beta_dc_cc <- c_cc * sigma_dc * nu_dc_deer_deer

library(deSolve)
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


# MY ODE
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
