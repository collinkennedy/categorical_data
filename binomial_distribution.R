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

#test and verify sumOfProb results in correct output...
stopifnot(round(sumOfProb(8,.8,4,7),7) == 0.8218214)
  


