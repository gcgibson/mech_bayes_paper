library(ggplot2)

pop <- 1000



a <- matrix(1,nrow=pop,ncol=pop)
diag(a) <- 0

## random graph
for (i in 1:pop){
  for (j in 1:pop){
    delete <- rbinom(1,1,0 )
    if (delete == 1 & i !=j){
      a[i,j] <- 0
      a[j,i] <- 0 
    }
  }
}


#a <- matrix(0,nrow=pop,ncol=pop)

#for (j in 110:210){
 # a[j,] <- 1
#  a[,j] <-1
#}
diag(a) <- 0
### SIR MODEL


sir <- function(time, pop, I_init, beta,gamma,delta_t,on_graph) {
  infected_ids <-c(1:I_init)
  recovered_ids <- c()
  susceptible_ids <- c((I_init+1):pop)
 sir_matrix <- matrix(NA,nrow=time,ncol=4)
 sir_matrix[1,] <- c(pop-I_init,I_init,0,I_init)
 for (t in 2:time){
   if ( !on_graph){
    new_infections <- mean(rpois(n=10000, sir_matrix[t-1,1]*beta*sir_matrix[t-1,2]/pop))
    new_recoveries <- mean(rpois(n=10000,sir_matrix[t-1,2]*gamma))
   } else{
     new_infections <- 0
     infected_ids_frozen <- infected_ids
     for (i in infected_ids_frozen){
      # print (infected_ids)
       connected <- a[,i]
       connected_ids <- which(a[,i]==1)
       connected_sus_ids <- susceptible_ids[susceptible_ids %in% connected_ids]
       
       if (length(connected_sus_ids) >0 ){
         succesful_infections <- rbinom(1,size=length(connected_sus_ids),prob=delta_t*beta/pop)
         if (length(connected_sus_ids) > succesful_infections ){
          sus_to_be_infected <- sample(connected_sus_ids,succesful_infections)
         } else{
           sus_to_be_infected <- connected_sus_ids
         }
       } else{
         sus_to_be_infected <- c()
       }
       
       if (length(sus_to_be_infected) > 0){
         infected_ids <- c(infected_ids,sus_to_be_infected)
         susceptible_ids <- setdiff(susceptible_ids,sus_to_be_infected)
         new_infections <- new_infections + succesful_infections
       }
       
       new_recoveries <- rbinom(n=1,size=1,prob=gamma*delta_t)
       
       if (new_recoveries == 1) {
        # print (new_recoveries)
         infected_ids <- setdiff(infected_ids,i)
         recovered_ids <- c(recovered_ids,i)
       }
     
     }
     
     
   }
   if (sir_matrix[t-1,2] == 0){
     break
   }
   
   sir_matrix[t,1] <- sir_matrix[t-1,1] - new_infections
   sir_matrix[t,2] <- sir_matrix[t-1,2] + new_infections - new_recoveries
   sir_matrix[t,3] <- sir_matrix[t-1,3] + new_recoveries
   sir_matrix[t,4] <- new_infections
   
 }
 print ("Stop")
 return (sir_matrix)
}


#plot(sim[,4])
cori_estimator <- function(incidence,gamma,delta_t){
  rt <-c()
  for (t in 2:length(incidence)){
    num <- incidence[t]
    denom <-0
    
    for (k in 1:(t-1)){
      #mult_ <- (pexp(k,rate = gamma)-pexp(k-1,rate = gamma))
      mult_ <- dgeom(k-1,gamma)
      denom <- denom + incidence[t-k]*mult_
    }
   
  
    rt <- c(rt , num/denom)
  }
  rt
}

rt_mat <- matrix(NA,nrow=100,ncol=99)
for (sim_idx in 1:100){

  beta <- .8
  gamma <-.6
  sim <- sir(time = 100,pop=pop,I_init = 100,beta=beta,gamma=gamma,delta_t = 1,on_graph = T)
  rt <-cori_estimator(sim[,4],gamma,delta_t)
  rt_mat[sim_idx,] <- rt
}
library(ggplot2)

plot_df <- data.frame(x=rep(1:99,100),rt=c(t(rt_mat)))
ggplot(d)
#plot(rt,type='l',ylim=c(0,2))
#abline(h=1,col='blue')
#lines((beta/gamma)*sim[,1]/pop,col='red')
print (sum(sim[,4])/pop)
print(rt)

