library(whitetailedSIRS)
library(tidyverse)



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
