# vat_2024_x_y.R 

# packages used
library(ggplot2); theme_set(theme_bw())
library(latex2exp)# LaTeX in ggplot
library(xtable)# export data frames to LaTeX tables

#Figure 3 (deleted, prices vs. Delta V) begins####
mu = 2 # market power (trans cost)
deltav = 1 #va-vb, later will be a vector on the horizontal axis
(deltav.vec = seq(0,2,0.01))
length(deltav.vec)
n=120 #number of consumers
tau = 0.1 #sales tax rate
tau1 = 0.1# show shifts in the charts when sales tax increases
tau2 = 0.2

## sales tax is embedded into the price (section 2)
(pa = (3*mu + deltav)/(3*(1+tau)))
(pb = (3*mu - deltav)/(3*(1+tau)))
(xhat = 1/2 + deltav/(6*mu))
(profita = n*(3*mu+deltav)^2/(18*mu*(1+tau)))
(profita = n*xhat*pa)# verify
(profitb = n*(3*mu-deltav)^2/(18*mu*(1+tau)))
(profitb = n*(1-xhat)*pb)# verify
(g = n*tau*(deltav^2+9*mu^2)/(9*mu*(1+tau)))
(g = n*xhat*tau*pa + n*(1-xhat)*tau*pb)# verify
#
# prepare vectors as function of delta v (tau1)
(pa1.vec = (3*mu + deltav.vec)/(3*(1+tau1)))
(pb1.vec = (3*mu - deltav.vec)/(3*(1+tau1)))
(xhat1.vec = 1/2 + deltav.vec/(6*mu))
(profita1.vec = n*xhat1.vec*pa1.vec)
(profitb1.vec = n*(1-xhat1.vec)*pb1.vec)
(g1.vec = n*xhat1.vec*tau1*pa1.vec + n*(1-xhat1.vec)*tau1*pb1.vec)
#
# prepare vectors as function of delta v (tau2)
(pa2.vec = (3*mu + deltav.vec)/(3*(1+tau2)))
(pb2.vec = (3*mu - deltav.vec)/(3*(1+tau2)))
(xhat2.vec = 1/2 + deltav.vec/(6*mu))
(profita2.vec = n*xhat2.vec*pa2.vec)
(profitb2.vec = n*(1-xhat2.vec)*pb2.vec)
(g2.vec = n*xhat2.vec*tau2*pa2.vec + n*(1-xhat2.vec)*tau2*pb2.vec)

## sales tax is separated from price: two-stage (section 3.2)
(paII = (mu*(2*tau+3) +deltav)/(2*tau+3))
(pbII = (mu*(2*tau+3) -deltav)/(2*tau+3))
(xhatII = 1/2 + deltav/(2*mu*(2*tau+3)))
(profitaII = n*(mu*(2*tau+3) +deltav)^2/(2*mu*(2*tau+3)^2))
(profitaII = n*xhatII*paII)# verify
(profitbII = n*(mu*(2*tau+3) -deltav)^2/(2*mu*(2*tau+3)^2))
(profitbII = n*(1-xhatII)*pbII)# verify
(gII = n*tau*((deltav^2) +mu^2*(2*tau+3)^2)/(mu*(2*tau+3)^2))
(gII = n*xhatII*tau*paII + n*(1-xhatII)*tau*pbII)# verify
# 
# Prepare vectors as function of delta v (tau1)
(paII1.vec = (mu*(2*tau1+3) +deltav.vec)/(2*tau1+3))
(pbII1.vec = (mu*(2*tau1+3) -deltav.vec)/(2*tau1+3))
(xhatII1.vec = 1/2 + deltav.vec/(2*mu*(2*tau1+3)))
(profitaII1.vec = n*xhatII1.vec*paII1.vec)
(profitbII1.vec = n*(1-xhatII1.vec)*pbII1.vec)
(gII1.vec = n*xhatII1.vec*tau1*paII1.vec + n*(1-xhatII1.vec)*tau1*pbII1.vec)
# 
# Prepare vectors as function of delta v (tau2)
(paII2.vec = (mu*(2*tau2+3) +deltav.vec)/(2*tau2+3))
(pbII2.vec = (mu*(2*tau2+3) -deltav.vec)/(2*tau2+3))
(xhatII2.vec = 1/2 + deltav.vec/(2*mu*(2*tau2+3)))
(profitaII2.vec = n*xhatII2.vec*paII2.vec)
(profitbII2.vec = n*(1-xhatII2.vec)*pbII2.vec)
(gII2.vec = n*xhatII2.vec*tau2*paII2.vec + n*(1-xhatII2.vec)*tau2*pbII2.vec)

## Construct data frame for plotting prices
(vat_price.df = data.frame(deltav.vec, pa1.vec, pb1.vec, paII1.vec, pbII1.vec, pa2.vec, pb2.vec, paII2.vec, pbII2.vec))
#
# Plot Figure 3 in the paper
ggplot(vat_price.df, aes(x=deltav.vec)) +geom_line(aes(y=pa1.vec), linetype="solid", size=1.2, color="black") +geom_line(aes(y=pb1.vec), linetype="solid", size=1.2, color="black") +geom_line(aes(y=paII1.vec), linetype="longdash", size=1.2, color="red") +geom_line(aes(y=pbII1.vec), linetype="longdash", size=1.2, color="red") + scale_x_continuous(breaks = seq(0,2,0.2)) + scale_y_continuous(breaks = seq(1.2,2.6,0.1)) +labs(x=TeX("Quality difference: $\\Delta V = V_A-V_B$"), y=TeX("Producer prices: $p_A^{I}$, $p_B^{I}$, $p_A^{II}$, $p_B^{II}$"))  +theme(axis.text.x = element_text(size = 14, color = "black"),  axis.text.y = element_text(size = 16, color = "black"), text = element_text(size = 20)) +annotate("text", x = 1.2, y = 1.39, label =TeX("$p_B^{I}$"), size = 8, color="black")  +annotate("text", x = 1.2, y = 1.7, label =TeX("$p_B^{II}$"), size = 8, color="red") +annotate("text", x = 1.2, y = 2.12, label =TeX("$p_A^{I}$"), size = 8, color="black")  +annotate("text", x = 1.2, y = 2.45, label =TeX("$p_A^{II}$"), size = 8, color="red")

#Figure 3 (deleted: prices vs. Delta V) ends####

#Figure 3 (prices vs. tau) begins####
(va=2)
(vb=1)
(deltav = va-vb)
(mu)
(n)
(tau1)# to be redefined as a vector (horizontal axix)
(tau2)
(tau.vec = seq(0.0, 0.25, 0.001))
length(tau.vec)

## sales tax is embedded into the price (section 2)
(paI.vec = (3*mu + deltav)/(3*(1+tau.vec)))
(qaI.vec = paI.vec*(1+tau.vec))
(pbI.vec = (3*mu - deltav)/(3*(1+tau.vec)))
(qbI.vec = pbI.vec*(1+tau.vec))
(xhatI.vec = rep(1/2 + deltav/(6*mu), length(tau.vec)))
(profitaI.vec = n*(3*mu+deltav)^2/(18*mu*(1+tau.vec)))
(profitaI.vec = n*xhatI.vec*paI.vec)# verify
(profitbI.vec = n*(3*mu-deltav)^2/(18*mu*(1+tau.vec)))
(profitbI.vec = n*(1-xhatI.vec)*pbI.vec)# verify
(gI.vec = n*tau.vec*(deltav^2+9*mu^2)/(9*mu*(1+tau.vec)))
(gI.vec = n*xhatI.vec*tau.vec*paI.vec + n*(1-xhatI.vec)*tau.vec*pbI.vec)# verify
#
## sales tax is separated from price: two-stage (section 3.2)
(paII.vec = (mu*(2*tau.vec+3) +deltav)/(2*tau.vec+3))
(qaII.vec = paII.vec*(1+tau.vec))
(pbII.vec = (mu*(2*tau.vec+3) -deltav)/(2*tau.vec+3))
(qbII.vec = pbII.vec*(1+tau.vec))
(xhatII.vec = 1/2 + deltav/(2*mu*(2*tau.vec+3)))
(profitaII.vec = n*(mu*(2*tau.vec+3) +deltav)^2/(2*mu*(2*tau.vec+3)^2))
(profitaII.vec = n*xhatII.vec*paII.vec)# verify
(profitbII.vec = n*(mu*(2*tau.vec+3) -deltav)^2/(2*mu*(2*tau.vec+3)^2))
(profitbII.vec = n*(1-xhatII.vec)*pbII.vec)# verify
(gII.vec = n*tau.vec*((deltav^2) +mu^2*(2*tau.vec+3)^2)/(mu*(2*tau.vec+3)^2))
(gII.vec = n*xhatII.vec*tau.vec*paII.vec + n*(1-xhatII.vec)*tau.vec*pbII.vec)# verify
# 
## making it a data frame
p.df = data.frame(tau.vec, paI.vec, pbI.vec, qaI.vec, qbI.vec, xhatI.vec, profitaI.vec, profitbI.vec, gI.vec, paII.vec, pbII.vec, qaII.vec, qbII.vec, xhatII.vec, profitaII.vec, profitbII.vec, gII.vec)
dim(p.df)

# Plot Figure 3 in the paper
ggplot(p.df, aes(x=tau.vec)) +geom_line(aes(y=paI.vec), linetype="solid", size=1.2, color="black") +geom_line(aes(y=pbI.vec), linetype="solid", size=1.2, color="black") +geom_line(aes(y=paII.vec), linetype="longdash", size=1.2, color="red") +geom_line(aes(y=pbII.vec), linetype="longdash", size=1.2, color="red") +geom_line(aes(y=qaI.vec), linetype="solid", size=1.2, color="blue") +geom_line(aes(y=qbI.vec), linetype="solid", size=1.2, color="blue") +geom_line(aes(y=qaII.vec), linetype="longdash", size=1.2, color="magenta") +geom_line(aes(y=qbII.vec), linetype="longdash", size=1.2, color="magenta") + scale_x_continuous(breaks = seq(0,0.25,0.05)) + scale_y_continuous(breaks = seq(1.2,2.9,0.1)) +labs(x=TeX("Sales tax (VAT) rate: $\\tau$"), y=TeX("Equilibrium producer and consumer prices:  $p_B$, $q_B$, $p_A$,$q_A$"))  +theme(axis.text.x = element_text(size = 14, color = "black"),  axis.text.y = element_text(size = 16, color = "black"), text = element_text(size = 20)) +annotate("text", x = 0.24, y = 1.28, label =TeX("$p_B^I$"), size = 8, color="black") +annotate("text", x = 0.24, y = 1.6, label =TeX("$q_B^I$"), size = 8, color="blue") +annotate("text", x = 0.24, y = 1.78, label =TeX("$p_B^{II}$"), size = 8, color="red")  +annotate("text", x = 0.24, y = 2.06, label =TeX("$q_B^{II}$"), size = 8, color="magenta") +annotate("text", x = 0.15, y = 2.1, label =TeX("$p_A^I$"), size = 8, color="black") +annotate("text", x = 0.15, y = 2.24, label =TeX("$p_A^{II}$"), size = 8, color="red") +annotate("text", x = 0.15, y = 2.41, label =TeX("$q_A^I$"), size = 8, color="blue") +annotate("text", x = 0.15, y = 2.72, label =TeX("$q_A^{II}$"), size = 8, color="magenta") +annotate("text", x = -0.003, y = 1.67, label =TeX("B"), size = 8, color="black") +annotate("text", x = -0.003, y = 2.34, label =TeX("A"), size = 8, color="black")  

#Figure 3 (prices vs. tau) ends####

#Figure 4 (welfare w.r.t tau) begins####
(va=2)
(vb=1)
(deltav = va-vb)
(mu)
(n)
(tau1)# to be redefined as a vector (horizontal axix)
(tau2)
(tau.vec = seq(0.0, 0.3, 0.001))
length(tau.vec)

# Welfare under tax-inclusive pricing
(wI = n*(va*(3*mu+deltav)/(6*mu) +vb*(3*mu-deltav)/(6*mu) - (deltav^2 +9*mu^2)/(36*mu) ))
# Welfare when sales tax is separated from prices
(wII = n*(va*(mu*(2*tau1+3)+deltav)/(2*mu*(2*tau1+3)) + vb*(mu*(2*tau1+3)-deltav)/(2*mu*(2*tau1+3)) -((deltav^2 +mu^2*(2*tau1+3)^2) /(4*mu*(2*tau1+3)^2)) ))
# Welfare socially optimal
(wstar = n*((va*(mu+deltav))/(2*mu) +(vb*(mu-deltav))/(2*mu) -(deltav^2 +mu^2)/(4*mu)))

# verifying accuracy by comparing to the differences computed in Appendix F
(wstarMinuswI_app = (n*deltav*(3*deltav-2*deltav))/(9*mu))
(wstar-wI)
#
(wIMinuswII_app = (n*deltav^2*tau1*(5*tau1+6)) /(9*mu*(2*tau1+3)^2))
(wI-wII)

# replacing tau1 with tau.vec (vector)
(wI.vec = rep(wI, length(tau.vec)) )
# Welfare when sales tax is separated from prices
(wII.vec = n*(va*(mu*(2*tau.vec+3)+deltav)/(2*mu*(2*tau.vec+3)) + vb*(mu*(2*tau.vec+3)-deltav)/(2*mu*(2*tau.vec+3)) -((deltav^2 +mu^2*(2*tau.vec+3)^2) /(4*mu*(2*tau.vec+3)^2)) ))
# Welfare socially optimal
(wstar.vec = rep(wstar, length(tau.vec)))

(w.df = data.frame(tau.vec, wI.vec, wII.vec, wstar.vec))

# Plot Figure 4 in the paper
ggplot(w.df, aes(x=tau.vec)) +geom_line(aes(y=wI.vec), linetype="longdash", size=1.2, color="black") +geom_line(aes(y=wII.vec), linetype="solid", size=1.2, color="red") +geom_line(aes(y=wstar.vec), linetype="dotted", size=1.5, color="blue") + scale_x_continuous(breaks = seq(0,0.3,0.05)) + scale_y_continuous(breaks = seq(127,136,1)) +labs(x=TeX("Sales tax (VAT) rate: $\\tau$"), y=TeX("Three welfare levels:  $W^{II}$, $W^{I}$, $W^{*}$$"))  +theme(axis.text.x = element_text(size = 14, color = "black"),  axis.text.y = element_text(size = 16, color = "black"), text = element_text(size = 20)) +annotate("text", x = 0.15, y = 134.7, label =TeX("$W^{*}$ (welfare under social optimum)"), size = 8, color="blue")  +annotate("text", x = 0.15, y = 128.7, label =TeX("$W^{I}$ (welfare with tax-inclusive pricing)"), size = 8, color="black")  +annotate("text", x = 0.10, y = 127.5, label =TeX("$W^{II}$ (pricing without tax)"), size = 8, color="red")  


# Figure 4 (welfare w.r.t tau) ends####

# Unused coding begins####
################
#Figure (profits as function of Delta V, not used!)
## Construct data frame for plotting prices
#(vat_profit.df = data.frame(deltav.vec, profita1.vec, profitb1.vec, profitaII1.vec, profitbII1.vec, profita2.vec, profitb2.vec, profitaII2.vec, profitbII2.vec, g1.vec, gII1.vec, g2.vec, gII2.vec))
#
# Plot Figure (profits as function of Delta V) NOT in the paper
#ggplot(vat_price.df, aes(x=deltav.vec)) +geom_line(aes(y=profita1.vec), linetype="solid", size=1.2, color="black") +geom_line(aes(y=profitb1.vec), linetype="solid", size=1.2, color="black") +geom_line(aes(y=profitaII1.vec), linetype="longdash", size=1.2, color="red") +geom_line(aes(y=profitbII1.vec), linetype="longdash", size=1.2, color="red") +geom_line(aes(y=g1.vec), linetype="longdash", size=1.2, color="blue") +geom_line(aes(y=gII1.vec), linetype="longdash", size=1.2, color="green")    
################

################
################