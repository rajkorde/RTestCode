library(pwr)

#T test sample size
#pwr.t.test(n=11165, sig.level = 0.95, power = 0.8)

#prop test sample size
pwr.2p.test(h=ES.h(0.65,0.85), sig.level = 0.05, power = 0.8)

power.prop.test(p1=0.65, p2=0.85, sig.level=0.05, power = 0.8, alternative="two.sided")

#running prop test
prop.test(20, 50, 0.3)
prop.test(200, 500, 0.3)

smokers  <- c( 83, 90, 129, 70 )
patients <- c( 86, 93, 136, 82 )
prop.test(smokers, patients)


pwr.2p.test(h = ES.h(0.04,0.05), n = NULL, sig.level = 0.05, power = 0.8)

pwr.2p2n.test(h = ES.h(0.04,0.05), n1 = 300000, sig.level = 0.05, power = 0.8)

zstar = qnorm(.975) 
p = 0.25 
E = 0.05 
zstar^2 ∗ p ∗ (1−p) / E^2 
