#Collin Kennedy
#UC Davis

#P(Y 18)
1 - pbinom(25,36,.5)

#same probability, using normal approximation with continuity correction
pnorm(.166667) - pnorm(-.166667)


pchisq(5.9915,2,lower.tail = FALSE)


pchisq(11.267,2,lower.tail = FALSE)

pnorm(2.5) - pnorm(2.833)

pchisq(8.048,1, lower.tail = FALSE)


#H0: p = .3
#H1: p > .3 
#phat = .4

#using normal approximation
1 - pnorm(2.5)

#exact binomial:
#P(Y>= 8) = 1 - P(Y <= 7) 
1 - pbinom(7,20,.3)
