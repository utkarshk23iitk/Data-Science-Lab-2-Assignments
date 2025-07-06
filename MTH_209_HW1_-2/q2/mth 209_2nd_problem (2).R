## Problem 2....
## We are doing this question by taking dealing n-dimensional sphere and cube..
## our reference is the origin in n-dimension...and fixed radius 1;
## value of pi=3.141592
is_point_inside<-function(n,point){
  r_centre<-numeric(length = length(point))
  for(i in 1:length(point)){
    r_centre[i]=0
  }
  dist=norm(point-r_centre,"2")
  if(dist<=1){
    return(1)
  }
  else{
    return(0)
  }
  
  
}

generate_point_ref<-function(n){
  points<-numeric(n)
  for(i in 1:n){
    points[i]<-runif(1,min=-1,max = 1)
  }
  return(points)
}





value_pi_individual <- function(n){
  obs<-numeric(10000)
  for(i in 1:10000){
    point<-generate_point_ref(n)
    obs[i]<-is_point_inside(n,point)
    
  }
  sum=sum(obs)
  ratio=sum/10000
  value_pi<- ((ratio*((2)^n)*gamma((n/2)+1)))^(2/n)
  return(value_pi)
  
  
  
  
}
r_centre<-c(0,0,0,0,0)
value_pi_individual(5)
dimen<-5


samples_by_diff<-numeric(100);
for(i in 1:100){
  samples_by_diff[i]<-value_pi_individual(dimen)
}
most_apprx_pi<-mean(samples_by_diff)

most_apprx_pi
error<-(abs(most_apprx_pi-pi)/pi)*100
error
