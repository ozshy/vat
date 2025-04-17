# vat_2025_x_y.R 

# packages used
library(ggplot2); theme_set(theme_bw())
library(latex2exp)# LaTeX in ggplot
library(xtable)# export data frames to LaTeX tables

lambda = 2 # market power (trans cost)
(va=2)
(vb=1)
(deltav = va-vb)
(deltav.vec = seq(0,2,0.01))
length(deltav.vec)
lambda > deltav# verify Assumption 2
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
(qaI.vec = lambda + (deltav +(2*ca+cb)*(1+tau.vec))/3)# eq (5)
(qbI.vec = lambda + (-deltav +(ca+2*cb)*(1+tau.vec))/3)
(paI.vec = qaI.vec/(1+tau.vec))
(pbI.vec = qbI.vec/(1+tau.vec))
# eq (6)
(xhatI.vec = 1/2 + (deltav-deltac*(1+tau.vec))/(6*lambda))
(profitaI.vec = n*(3*lambda+deltav -deltac*(1+tau.vec))^2/(18*lambda*(1+tau.vec)))
(profitaI.vec = n*xhatI.vec*(paI.vec-ca))# verify
(profitbI.vec = n*(3*lambda-deltav+deltac*(1+tau.vec))^2/(18*lambda*(1+tau.vec)))
(profitbI.vec = n*(1-xhatI.vec)*(pbI.vec-cb))# verify
#
#

## making it a data frame
p.df = data.frame(tau.vec, paI.vec, pbI.vec, qaI.vec, qbI.vec, xhatI.vec, profitaI.vec, profitbI.vec)
dim(p.df)

ggplot(p.df, aes(x=tau.vec)) +geom_line(aes(y=paI.vec), linetype="solid", size=1.2, color="blue") +geom_line(aes(y=pbI.vec), linetype="solid", size=1.2, color="blue")  +geom_line(aes(y=qaI.vec), linetype="longdash", size=1.2, color="black") +geom_line(aes(y=qbI.vec), linetype="longdash", size=1.2, color="black")  + scale_x_continuous(breaks = seq(0,0.25,0.05)) + scale_y_continuous(breaks = seq(1.6,3.3,0.1)) +labs(x=TeX("Sales tax (VAT) rate: $\\tau$"), y=TeX("Equilibrium seller and consumer prices:  $p_B$, $q_B$, $p_A$,$q_A$"))  +theme(axis.text.x = element_text(size = 14, color = "black"),  axis.text.y = element_text(size = 16, color = "black"), text = element_text(size = 20)) +annotate("text", x = 0.225, y = 1.70, label =TeX("$p_B^I$"), size = 8, color="blue") +annotate("text", x = 0.24, y = 2.07, label =TeX("$q_B^I$"), size = 8, color="black") +annotate("text", x = 0.20, y = 2.35, label =TeX("$p_A^I$"), size = 8, color="blue") +annotate("text", x = 0.15, y = 2.79, label =TeX("$q_A^I$"), size = 8, color="black") +annotate("text", x = -0.003, y = 1.94, label =TeX("B"), size = 8, color="black") +annotate("text", x = -0.003, y = 2.67, label =TeX("A"), size = 8, color="black")  

#Figure 2 (prices vs. tau) ends####

#Figure 3 (prices vs. sigma) begins####
# new parameters
(sigma.vec = seq(0,1, 0.005))
length(sigma.vec)
# Recall old parameters
ca
cb
deltac
lambda

# low tau (tau1)
(tau1 = 0.1)
#prices as functions of sigma, eq (10) in the paper
(pae1.vec = (3*lambda + deltav +(2*ca+cb)*(1+tau1-sigma.vec*tau1))/(3*(1+tau1-sigma.vec*tau1)))
#
(pbe1.vec = (3*lambda - deltav +(ca+2*cb)*(1+tau1-sigma.vec*tau1))/(3*(1+tau1-sigma.vec*tau1)))

#market shares as functions of sigma, eq (8) in the paper
#
(xhatf1.vec = 1/2 + (deltav+(pbe1.vec-pae1.vec)*(1+tau1))/(2*lambda))
#
(xhats1.vec = 1/2 + (deltav+(pbe1.vec-pae1.vec))/(2*lambda))

#profits as functions of sigma, eq (11) in the paper
(profitae1.vec = n*(3*lambda +deltav -deltac*(1+tau1-sigma.vec*tau1))^2/(18*lambda*(1+tau1-sigma.vec*tau1)))
#
(profitbe1.vec = n*(3*lambda -deltav +deltac*(1+tau1-sigma.vec*tau1))^2/(18*lambda*(1+tau1-sigma.vec*tau1)))
#
# cross verify profits using equation (9) in the paper [yes!]
#(profitae1.vec = (pae1.vec -ca)*n*((1-sigma.vec)*xhatf1.vec +sigma.vec*xhats1.vec))
#
#(profitbe1.vec = (pbe1.vec -cb)*n*((1-sigma.vec)*(1-xhatf1.vec) +sigma.vec*(1-xhats1.vec)))

# high tau (tau2)
(tau2 = 0.2)
#prices as functions of sigma, eq (10) in the paper
(pae2.vec = (3*lambda + deltav +(2*ca+cb)*(1+tau2-sigma.vec*tau2))/(3*(1+tau2-sigma.vec*tau2)))
#
(pbe2.vec = (3*lambda - deltav +(ca+2*cb)*(1+tau2-sigma.vec*tau2))/(3*(1+tau2-sigma.vec*tau2)))

#market shares as functions of sigma, eq (8) in the paper
#
(xhatf2.vec = 1/2 + (deltav+(pbe2.vec-pae2.vec)*(1+tau2))/(2*lambda))
#
(xhats2.vec = 1/2 + (deltav+(pbe2.vec-pae2.vec))/(2*lambda))

#profits as functions of sigma, eq (11) in the paper
(profitae2.vec = n*(3*lambda +deltav -deltac*(1+tau2-sigma.vec*tau2))^2/(18*lambda*(1+tau2-sigma.vec*tau2)))
#
(profitbe2.vec = n*(3*lambda -deltav +deltac*(1+tau2-sigma.vec*tau2))^2/(18*lambda*(1+tau2-sigma.vec*tau2)))
#
# cross verify profits using equation (9) in the paper [yes!]
#(profitae2.vec = (pae2.vec -ca)*n*((1-sigma.vec)*xhatf2.vec +sigma.vec*xhats2.vec))
#
#(profitbe1.vec = (pbe2.vec -cb)*n*((1-sigma.vec)*(1-xhatf2.vec) +sigma.vec*(1-xhats2.vec)))

# making a data frame for Figure 3
(sigma.df = data.frame(sigma.vec, pae1.vec, pbe1.vec, profitae1.vec, profitbe1.vec, pae2.vec, pbe2.vec, profitae2.vec, profitbe2.vec))

# plotting Figure 3
ggplot(sigma.df, aes(x=sigma.vec)) +geom_line(aes(y=pae1.vec), linetype="solid", size=1.2, color="blue") +geom_line(aes(y=pbe1.vec), linetype="solid", size=1.2, color="blue")  +geom_line(aes(y=pae2.vec), linetype="longdash", size=1.2, color="black") +geom_line(aes(y=pbe2.vec), linetype="longdash", size=1.2, color="black") + scale_x_continuous(breaks = seq(0,1,0.1)) + scale_y_continuous(breaks = seq(1.6,2.7,0.1)) +labs(x=TeX("Fraction of slow-computing consumers: $\\sigma$"), y=TeX("Equilibrium seller prices:  $p_A^E$, and $p_B^E$"))  +theme(axis.text.x = element_text(size = 14, color = "black"),  axis.text.y = element_text(size = 16, color = "black"), text = element_text(size = 20)) +annotate("text", x = 0.2, y = 2.55, label =TeX("$p_A^E (\\tau = 0.1$)"), size = 8, color="blue") +annotate("text", x = 0.2, y = 2.26, label =TeX("$p_A^E (\\tau = 0.2$)"), size = 8, color="black") +annotate("text", x = 0.2, y = 1.85, label =TeX("$p_B^E (\\tau = 0.1$)"), size = 8, color="blue") +annotate("text", x = 0.2, y = 1.64, label =TeX("$p_B^E (\\tau = 0.2$)"), size = 8, color="black") +annotate("text", x = 1.015, y = 1.94, label =TeX("B"), size = 8, color="black") +annotate("text", x = 1.015, y = 2.67, label =TeX("A"), size = 8, color="black")  +annotate("text", x = -0.02, y = 2.45, label =TeX("$p_A^I"), size = 8, color="red") +annotate("text", x = -0.02, y = 2.27, label =TeX("$p_A^I"), size = 8, color="red")+annotate("text", x = -0.02, y = 1.78, label =TeX("$p_B^I"), size = 8, color="red") +annotate("text", x = -0.02, y = 1.66, label =TeX("$p_B^I"), size = 8, color="red") +geom_vline(xintercept = 0.0, linetype ="dotted", size=1.2, color="red")


 #Figure 3 (prices vs. lambda) ends####


