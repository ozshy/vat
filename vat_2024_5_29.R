# vat_2024_x_y.R 

# packages used
library(ggplot2); theme_set(theme_bw())
library(latex2exp)# LaTeX in ggplot
library(xtable)# export data frames to LaTeX tables

mu = 2 # market power (trans cost)
(va=2)
(vb=1)
(deltav = va-vb)
(deltav.vec = seq(0,2,0.01))
length(deltav.vec)
mu > deltav# verify Assumption 2
#
ca=0.4
cb=0.2
(deltac = ca-cb)
deltav > 2*deltac# verify Assumption 1c
#
n=120 #number of consumers
(tau.vec = seq(0.0, 0.25, 0.001))# horizontal axis
length(tau.vec)

## sales tax is embedded into the price: fast-computing (section 2)
(qaI.vec = mu + (deltav +(2*ca+cb)*(1+tau.vec))/3)# eq (5)
(qbI.vec = mu + (-deltav +(ca+2*cb)*(1+tau.vec))/3)
(paI.vec = qaI.vec/(1+tau.vec))
(pbI.vec = qbI.vec/(1+tau.vec))
# eq (6)
(xhatI.vec = 1/2 + (deltav-deltac*(1+tau.vec))/(6*mu))
(profitaI.vec = n*(3*mu+deltav -deltac*(1+tau.vec))^2/(18*mu*(1+tau.vec)))
(profitaI.vec = n*xhatI.vec*(paI.vec-ca))# verify
(profitbI.vec = n*(3*mu-deltav+deltac*(1+tau.vec))^2/(18*mu*(1+tau.vec)))
(profitbI.vec = n*(1-xhatI.vec)*(pbI.vec-cb))# verify
#
#
## sales tax is separated from price: slow-computing (section 3.2)
(paII.vec = mu +(deltav+ca*(2+tau.vec) +cb*(1+tau.vec))/(2*tau.vec+3))
(qaII.vec = paII.vec*(1+tau.vec))
(pbII.vec = mu +(-deltav +ca*(1+tau.vec) +cb*(2+tau.vec))/(2*tau.vec+3))
(qbII.vec = pbII.vec*(1+tau.vec))
(xhatII.vec = 1/2 + (deltav -deltac*(1+tau.vec))/(2*mu*(2*tau.vec+3)))
(profitaII.vec = n*(mu*(2*tau.vec+3) +deltav -deltac*(1+tau.vec))^2/(2*mu*(2*tau.vec+3)^2))
(profitaII.vec = n*xhatII.vec*(paII.vec-ca))# verify
(profitbII.vec = n*(mu*(2*tau.vec+3) -deltav +deltac*(1+tau.vec))^2/(2*mu*(2*tau.vec+3)^2))
(profitbII.vec = n*(1-xhatII.vec)*(pbII.vec-cb))# verify
# 
## making it a data frame
p.df = data.frame(tau.vec, paI.vec, pbI.vec, qaI.vec, qbI.vec, xhatI.vec, profitaI.vec, profitbI.vec,  paII.vec, pbII.vec, qbII.vec, xhatII.vec, profitaII.vec, profitbII.vec)
dim(p.df)


ggplot(p.df, aes(x=tau.vec)) +geom_line(aes(y=paI.vec), linetype="solid", size=1.2, color="black") +geom_line(aes(y=pbI.vec), linetype="solid", size=1.2, color="black") +geom_line(aes(y=paII.vec), linetype="longdash", size=1.2, color="red") +geom_line(aes(y=pbII.vec), linetype="longdash", size=1.2, color="red") +geom_line(aes(y=qaI.vec), linetype="solid", size=1.2, color="blue") +geom_line(aes(y=qbI.vec), linetype="solid", size=1.2, color="blue") +geom_line(aes(y=qaII.vec), linetype="longdash", size=1.2, color="magenta") +geom_line(aes(y=qbII.vec), linetype="longdash", size=1.2, color="magenta") + scale_x_continuous(breaks = seq(0,0.25,0.05)) + scale_y_continuous(breaks = seq(1.6,3.3,0.1)) +labs(x=TeX("Sales tax (VAT) rate: $\\tau$"), y=TeX("Equilibrium producer and consumer prices:  $p_B$, $q_B$, $p_A$,$q_A$"))  +theme(axis.text.x = element_text(size = 14, color = "black"),  axis.text.y = element_text(size = 16, color = "black"), text = element_text(size = 20)) +annotate("text", x = 0.225, y = 1.70, label =TeX("$p_B^I$"), size = 8, color="black") +annotate("text", x = 0.24, y = 2.08, label =TeX("$q_B^I$"), size = 8, color="blue") +annotate("text", x = 0.225, y = 1.90, label =TeX("$p_B^{II}$"), size = 8, color="red")  +annotate("text", x = 0.24, y = 2.39, label =TeX("$q_B^{II}$"), size = 8, color="magenta") +annotate("text", x = 0.20, y = 2.20, label =TeX("$p_A^I$"), size = 8, color="black") +annotate("text", x = 0.20, y = 2.54, label =TeX("$p_A^{II}$"), size = 8, color="red") +annotate("text", x = 0.15, y = 2.80, label =TeX("$q_A^I$"), size = 8, color="blue") +annotate("text", x = 0.15, y = 3.13, label =TeX("$q_A^{II}$"), size = 8, color="magenta") +annotate("text", x = -0.003, y = 1.94, label =TeX("B"), size = 8, color="black") +annotate("text", x = -0.003, y = 2.67, label =TeX("A"), size = 8, color="black")  

#Figure 3 (prices vs. tau) ends####

#Figure 4=5 (NOT USED in the paper) (Regret: prices vs. tau) begins####
## Regret: sales tax is separated from price: Section 5

(paIII.vec = mu +(deltav+2*ca +cb)/3)
(qaIII.vec = paIII.vec*(1+tau.vec))
(pbIII.vec = mu +(-deltav +ca +2*cb)/3)
(qbIII.vec = pbIII.vec*(1+tau.vec))
(xhatIII.vec = 1/2 + (deltav +pbIII.vec -paIII.vec)/(2*mu))
# 
## making it a data frame
pIII.df = data.frame(tau.vec, paI.vec, pbI.vec, qaI.vec, qbI.vec, paIII.vec, pbIII.vec, qbIII.vec)
dim(pIII.df)

ggplot(pIII.df, aes(x=tau.vec)) +geom_line(aes(y=paI.vec), linetype="solid", size=1.2, color="black") +geom_line(aes(y=pbI.vec), linetype="solid", size=1.2, color="black") +geom_line(aes(y=paIII.vec), linetype="longdash", size=1.2, color="red") +geom_line(aes(y=pbIII.vec), linetype="longdash", size=1.2, color="red") +geom_line(aes(y=qaI.vec), linetype="solid", size=1.2, color="blue") +geom_line(aes(y=qbI.vec), linetype="solid", size=1.2, color="blue") +geom_line(aes(y=qaIII.vec), linetype="longdash", size=1.2, color="magenta") +geom_line(aes(y=qbIII.vec), linetype="longdash", size=1.2, color="magenta") + scale_x_continuous(breaks = seq(0,0.25,0.05)) + scale_y_continuous(breaks = seq(1.6,3.3,0.1)) +labs(x=TeX("Sales tax (VAT) rate: $\\tau$"), y=TeX("Equilibrium producer and consumer prices:  $p_B$, $q_B$, $p_A$,$q_A$"))  +theme(axis.text.x = element_text(size = 14, color = "black"),  axis.text.y = element_text(size = 16, color = "black"), text = element_text(size = 20)) +annotate("text", x = 0.225, y = 1.70, label =TeX("$p_B^I$"), size = 8, color="black") +annotate("text", x = 0.24, y = 2.08, label =TeX("$q_B^I$"), size = 8, color="blue") +annotate("text", x = 0.225, y = 1.90, label =TeX("$p_B^{III}$"), size = 8, color="red")  +annotate("text", x = 0.24, y = 2.39, label =TeX("$q_B^{III}$"), size = 8, color="magenta") +annotate("text", x = 0.20, y = 2.20, label =TeX("$p_A^I$"), size = 8, color="black") +annotate("text", x = 0.20, y = 2.54, label =TeX("$p_A^{III}$"), size = 8, color="red") +annotate("text", x = 0.15, y = 2.80, label =TeX("$q_A^I$"), size = 8, color="blue") +annotate("text", x = 0.15, y = 3.13, label =TeX("$q_A^{III}$"), size = 8, color="magenta") +annotate("text", x = -0.003, y = 1.94, label =TeX("B"), size = 8, color="black") +annotate("text", x = -0.003, y = 2.67, label =TeX("A"), size = 8, color="black")  

#Figure 5 (NOT USED in the paper) (Regret: prices vs. tau) ends####



# Unused coding begins####
################
#Figure (profits as function of Delta V, not used!)
## Construct data frame for plotting prices
#(vat_profit.df = data.frame(deltav.vec, profita1.vec, profitb1.vec, profitaII1.vec, profitbII1.vec, profita2.vec, profitb2.vec, profitaII2.vec, profitbII2.vec, g1.vec, gII1.vec, g2.vec, gII2.vec))
#
# Plot Figure (profits as function of Delta V) NOT in the paper
#ggplot(vat_price.df, aes(x=deltav.vec)) +geom_line(aes(y=profita1.vec), linetype="solid", size=1.2, color="black") +geom_line(aes(y=profitb1.vec), linetype="solid", size=1.2, color="black") +geom_line(aes(y=profitaII1.vec), linetype="longdash", size=1.2, color="red") +geom_line(aes(y=profitbII1.vec), linetype="longdash", size=1.2, color="red") +geom_line(aes(y=g1.vec), linetype="longdash", size=1.2, color="blue") +geom_line(aes(y=gII1.vec), linetype="longdash", size=1.2, color="green")    
################
# #Figure 4 (welfare w.r.t tau) begins####
# (va=2)
# (vb=1)
# (deltav = va-vb)
# (mu)
# (n)
# (tau1)# to be redefined as a vector (horizontal axix)
# (tau2)
# (tau.vec = seq(0.0, 0.3, 0.001))
# length(tau.vec)
# 
# # Welfare under tax-inclusive pricing
# (wI = n*(va*(3*mu+deltav)/(6*mu) +vb*(3*mu-deltav)/(6*mu) - (deltav^2 +9*mu^2)/(36*mu) ))
# # Welfare when sales tax is separated from prices
# (wII = n*(va*(mu*(2*tau1+3)+deltav)/(2*mu*(2*tau1+3)) + vb*(mu*(2*tau1+3)-deltav)/(2*mu*(2*tau1+3)) -((deltav^2 +mu^2*(2*tau1+3)^2) /(4*mu*(2*tau1+3)^2)) ))
# # Welfare socially optimal
# (wstar = n*((va*(mu+deltav))/(2*mu) +(vb*(mu-deltav))/(2*mu) -(deltav^2 +mu^2)/(4*mu)))
# 
# # verifying accuracy by comparing to the differences computed in Appendix F
# (wstarMinuswI_app = (n*deltav*(3*deltav-2*deltav))/(9*mu))
# (wstar-wI)
# #
# (wIMinuswII_app = (n*deltav^2*tau1*(5*tau1+6)) /(9*mu*(2*tau1+3)^2))
# (wI-wII)
# 
# # replacing tau1 with tau.vec (vector)
# (wI.vec = rep(wI, length(tau.vec)) )
# # Welfare when sales tax is separated from prices
# (wII.vec = n*(va*(mu*(2*tau.vec+3)+deltav)/(2*mu*(2*tau.vec+3)) + vb*(mu*(2*tau.vec+3)-deltav)/(2*mu*(2*tau.vec+3)) -((deltav^2 +mu^2*(2*tau.vec+3)^2) /(4*mu*(2*tau.vec+3)^2)) ))
# # Welfare socially optimal
# (wstar.vec = rep(wstar, length(tau.vec)))
# 
# (w.df = data.frame(tau.vec, wI.vec, wII.vec, wstar.vec))
# 
# # Plot Figure 4 in the paper
# ggplot(w.df, aes(x=tau.vec)) +geom_line(aes(y=wI.vec), linetype="longdash", size=1.2, color="black") +geom_line(aes(y=wII.vec), linetype="solid", size=1.2, color="red") +geom_line(aes(y=wstar.vec), linetype="dotted", size=1.5, color="blue") + scale_x_continuous(breaks = seq(0,0.3,0.05)) + scale_y_continuous(breaks = seq(127,136,1)) +labs(x=TeX("Sales tax (VAT) rate: $\\tau$"), y=TeX("Three welfare levels:  $W^{II}$, $W^{I}$, $W^{*}$$"))  +theme(axis.text.x = element_text(size = 14, color = "black"),  axis.text.y = element_text(size = 16, color = "black"), text = element_text(size = 20)) +annotate("text", x = 0.15, y = 134.7, label =TeX("$W^{*}$ (welfare under social optimum)"), size = 8, color="blue")  +annotate("text", x = 0.15, y = 128.7, label =TeX("$W^{I}$ (welfare with tax-inclusive pricing)"), size = 8, color="black")  +annotate("text", x = 0.10, y = 127.5, label =TeX("$W^{II}$ (pricing without tax)"), size = 8, color="red")  


# Figure 4 (welfare w.r.t tau) ends####

################
################