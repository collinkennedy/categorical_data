#calculate probabilities from binomial distribution


#P(at least 5 students get As)
1 - pbinom(4,8,.8) #number of successes (As), n, success probability

#write a function to calculate a sum of binomial probabilities over a 
#specified interval(within a certain range.. not CDF)
sumOfProb = function(n, successProbability, startX, endX, inclusive = TRUE){
  returnProb = 0
  if(inclusive == TRUE){
  while(startX <= endX){
    returnProb = returnProb + dbinom(startX, n, successProbability)
    startX = startX + 1
  }
  }
  else{
    while(startX < endX){
      returnProb = returnProb + dbinom(startX, n, successProbability)
      startX = startX + 1
    }
  }
  return(returnProb)
}

sumOfProb(8,.8,4,7)

#adding probabilities manually -> P(Y = 4) + P(Y = 5) + P(Y = 6) + P(Y = 7):
dbinom(4,8,.8) + dbinom(5,8,.8) + dbinom(6,8,.8) + dbinom(7,8,.8)

pbinom(7,8,.8) - pbinom(3,8,.8) #including 4...



#test and verify sumOfProb results in correct output...
stopifnot(round(sumOfProb(8,.8,4,7),7) == 0.8218214) #good


dbinom(12,18,.6) #12 successes, 18 trials, .6 success probability


#P(Y >= 12)
1 - pbinom(11,18,.6)

1 - sumOfProb(18,.6,0,11,inclusive = TRUE)
  
#P(12 < Y < 16):
sumOfProb(18,.6, 13,16, inclusive = FALSE)

pnorm(.58,mean = 0,sd=1) #area/ probability to the left of .58 under the standard normal curve.


dnorm(.8095)

1 - pnorm(.336788)

pnorm(2.26) - pnorm(.817913)


#P(At least 1 stock has a positive return)...
1 - pbinom(0,8,.4)
sumOfProb(8,.4,1,8, inclusive = TRUE)


#P(Y = 5):

dbinom(5,10,.2)


#P(Y >= 2):

1 - sumOfProb(10,.2,0,1)

pnorm(-.5613,mean=0,sd=1)


#P
pbinom(25,30,.8)


1-pnorm(.39)



dbinom(12,18,.6)
pnorm(.8179) - pnorm(.33678)

1 - sumOfProb(18,.6,0,11)

1-pnorm(.33678)




#Multinomial distribution:

dmultinom()

factorial(10)/(factorial(4)*factorial(3)*factorial(2))



1 - sumOfProb(6,.25,0,1,inclusive = TRUE)

1 - pbinom(1,6,.4)

(factorial(6)/(factorial(2)*factorial(2)*factorial(2)))*(.3)^2*(.3)^2*(.4)^2

1 - sumOfProb(6,.4,0,1)

1 - pnorm(1.71)

pnorm(.243902) - pnorm(-.243902)

#1
a).297
b).466


1-pnorm(1.625)
