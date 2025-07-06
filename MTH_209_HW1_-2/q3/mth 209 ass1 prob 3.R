# Install and load the necessary package
install.packages("tseries")
library(tseries)

# Define a function to perform a recursive runs test
# Base case: Stop recursion if the length of the data is less than or equal to the minimum size
recursive_runs_test<-function(data,min_size=2,results=data.frame()){
  if(length(data)<=min_size){
    return(results)
  }
  # Perform a runs test on the current dataset
  # The test checks if the sequence of data points above and below the median is random
  runs_test_result<-runs.test(as.factor(data>median(data)))
  
  p_value<-runs_test_result$p.value
  # Determine the result based on the p-value
  # If p-value > 0.05, we fail to reject the null hypothesis, indicating randomness
  result<-ifelse(p_value>0.05,"Random","Not random")
  
  results<-rbind(results,data.frame(subset_size = length(data),p_value=p_value,result=result))
  
  # Divide the data into two halves and recursively apply the runs test to each half
  mid<- floor(length(data)/2)
  results<-recursive_runs_test(data[1:mid],min_size,results)
  results<-recursive_runs_test(data[(mid+1):length(data)],min_size,results)
  return(results)
  
}

data<- rnorm(100,mean=0,sd=1)
results<-recursive_runs_test(data,min_size = 2) # Call the recursive runs test function on the generated data
print(results)
# Calculate the average p-value from the results
mean(results$p_value)

# Determine overall randomness based on the mean p-value
# If the mean p-value > 0.05, the data appears random
ifelse(mean(results$p_value)>0.05,"The data appears to be random","The data is not random")