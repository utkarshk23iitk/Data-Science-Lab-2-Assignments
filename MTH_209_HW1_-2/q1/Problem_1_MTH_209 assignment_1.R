
###### Problem 1: Estimation of e by using results of Probability theory ######

#### Method:1 ####
### We will take 1e6 samples from uniform(0,1) and will take their geometric mean yn.And then inverse the yn.
### we will repeat this experiment 1e3 times and take the mean of all yn's


n= 1e6       ## No.of observation from U(0,1) by each individual.

k= 1e2      ## No. of observer..

vec<- numeric(length = k)      ## Vector that will store the estimated value of e from each observer. 

for( i in 1:k){
  uni<- runif(n,0,1)        ##generation of 1e6 samples from U(0,1)
  
  yn<- prod(uni^(1/n))      ## Taking geometric mean of all samples
  
  vec[i]<- 1/yn
}

e <- mean(vec)  ## mean of the values estimated by every individual.

cat("The value of e :",e)     ## Value of estimated e..

###########################################################################################################

### Method: 2 ###
### In this method we will estimate the value of e by deck of cards, if we have n deck of cards and their derangement is d and total permutation is n then value of e would be d/n
### So we perform this experiment by taking m-set of cards and shuffle these cards k times. if we get some of these arrangement as deranged outcome then count that number 'p' and calculate k/p to get value of e..
### This experiment would lead to give expansion of e^(-1) in some sense.. if we solve mathematically



is_derangement <- function(permutation) {
  all(permutation != seq_along(permutation))  
  ## Check if the permutation is deranged
}

estimate_e <- function(deck_size, trials) {
  derangements <- 0
  cumulative_e <- numeric(trials)  
  ## Store e estimates at each trial
  
  for (i in 1:trials) {
    deck <- sample(1:deck_size)  
    ## Shuffle the cards
    
    if (is_derangement(deck)) {
      derangements <- derangements + 1
    }
    
    cumulative_e[i] <- ifelse(derangements > 0, i / derangements, Inf) 
    #inverse of probability after each trial
  }
  
  return(cumulative_e) #e estimates after each trial
}

cards <- 52  ## Standard 52 deck of cards (numbered 1 to 52)
trials <- 2*1e4  ## Number of trials
repeats <-100;   ## Repetitions for accuracy

vec_e<-numeric(length = repeats); #vector that stores e estimates

for(i in 1:repeats){
  cumulative_e <- estimate_e(cards,trials)
  vec_e[i] <- cumulative_e[length(cumulative_e)]
}

final_estimate <- mean(vec_e)

cat("The value of e:", final_estimate)

## Plot how estimate gets closer to real value
plot(1:trials, cumulative_e, type = "l", col = "blue",
     , ylim = c(2, 3), xlab = "Number of Trials", ylab = "Estimated Value of e")
abline(h = exp(1), col = "red")




