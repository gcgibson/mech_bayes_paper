library(ggplot2)

pop <- 1000



a <- matrix(1,nrow=pop,ncol=pop)
diag(a) <- 0

## random graph
for (i in 1:pop){
  for (j in 1:pop){
    delete <- rbinom(1,1,0)
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

get_observed_rt <- function(sim,pop){
  sus_to_be_infected <- sim[[7]]
  secondary_infections <- sim[[3]]
  rt_mat <- c()
  for (t in 1:length(sus_to_be_infected)){
    tmp <- secondary_infections[,sus_to_be_infected[[t]]]
    if (length(tmp) > 0){
      if (length(dim(tmp)) == 1){
        tmp2 <- rowSums(tmp)
        rt_mat <- c(rt_mat,mean(rowSums(tmp2[tmp2>0])))
      }else{
        rt_mat <- c(rt_mat,mean(tmp[tmp>0]))
      }
    } else{
      rt_mat <- c(rt_mat,0)
    }
  }
  return (rt_mat)
}


sir <- function(time, pop, I_init, beta,gamma,delta_t,on_graph) {
  infected_ids <-c(1:I_init)
  recovered_ids <- c()
  susceptible_ids <- c((I_init+1):pop)
 
  
  sus_ids_mat <- list()
  infected_ids_mat <- list()
  newly_infected_ids_mat <- list()
  
  recovered_ids_mat <-list()
  
  sus_ids_mat[[1]] <- susceptible_ids
  infected_ids_mat[[1]] <- infected_ids
  newly_infected_ids_mat[[1]] <- infected_ids
  recovered_ids_mat[[1]] <- recovered_ids
  
  
  
 sir_matrix <- matrix(NA,nrow=time,ncol=4)
 sir_matrix[1,] <- c(pop-I_init,I_init,0,I_init)
 sir_matrix_avg <- matrix(NA,nrow=time,ncol=4)
 sir_matrix_avg[1,] <- c(pop-I_init,I_init,0,I_init)
 secondary_infections_at_time_t <- matrix(0,nrow=time,ncol=pop)
 average_secondary_infections <- c()
 for (t in 2:time){
    new_infections_avg <- mean(rpois(n=10000, sir_matrix_avg[t-1,1]*beta*sir_matrix_avg[t-1,2]/pop))
    new_recoveries_avg <- mean(rpois(n=10000,sir_matrix_avg[t-1,2]*gamma))
    sir_matrix_avg[t,1] <- sir_matrix_avg[t-1,1] - new_infections_avg
    sir_matrix_avg[t,2] <- sir_matrix_avg[t-1,2] + new_infections_avg - new_recoveries_avg
    sir_matrix_avg[t,3] <- sir_matrix_avg[t-1,3] + new_recoveries_avg
    sir_matrix_avg[t,4] <- new_infections_avg
    
    
    
     new_infections <- 0
     infected_ids_frozen <- infected_ids
     new_infection_idx <- 1
     
     tot_sus_to_be_infected <-c()
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
       
       secondary_infections_at_time_t[t,i ] <- secondary_infections_at_time_t[time,i ] + succesful_infections 
       new_infection_idx <- new_infection_idx + succesful_infections
       
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
     
       tot_sus_to_be_infected <- c(tot_sus_to_be_infected,sus_to_be_infected)
     }
     
     
     
   
   if (sir_matrix[t-1,2] == 0){
     break
   }
   
     sus_ids_mat[[t]] <- susceptible_ids
     infected_ids_mat[[t]] <- infected_ids
     recovered_ids_mat[[t]] <- recovered_ids
     newly_infected_ids_mat[[t]] <- tot_sus_to_be_infected
     
   average_secondary_infections <- c(average_secondary_infections,new_infections_avg/(sir_matrix[t-1,2]-new_recoveries_avg))
   sir_matrix[t,1] <- sir_matrix[t-1,1] - new_infections
   sir_matrix[t,2] <- sir_matrix[t-1,2] + new_infections - new_recoveries
   sir_matrix[t,3] <- sir_matrix[t-1,3] + new_recoveries
   sir_matrix[t,4] <- new_infections
  # print (tot_sus_to_be_infected)
  # print (new_infections)
  # print ("------")
   
 }
 print ("Stop")
 ret_list <- list()
 ret_list[[1]] <- sir_matrix
 ret_list[[2]] <- sir_matrix_avg
 ret_list[[3]] <- secondary_infections_at_time_t
 ret_list[[4]] <- sus_ids_mat
 ret_list[[5]] <- infected_ids_mat
 ret_list[[6]] <- recovered_ids_mat
 ret_list[[7]] <- newly_infected_ids_mat
 
 return (ret_list)
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


time_sim <- 20
rt_mat <- matrix(NA,nrow=100,ncol=time_sim -1)
observed_rt_mat <- matrix(NA,nrow=100,ncol=time_sim -1)

final_size <- c()
for (sim_idx in 1:100){

  beta <- 1.2
  gamma <-.6
  sim <- sir(time = time_sim,pop=pop,I_init = 100,beta=beta,gamma=gamma,delta_t = 1,on_graph = T)
  #plot(sim[[2]][,4])
  rt <-cori_estimator(sim[[1]][,4],gamma,1)
  rt_mat[sim_idx,] <- rt
  final_size <- c(final_size,sum(sim[[2]][,4])/pop)
  observed_rt_mat[sim_idx,] <- get_observed_rt(sim = sim,pop=pop)[2:time_sim]
}
library(ggplot2)

plot_df <- data.frame(x=rep(1:(time_sim-1),100),rt=colMeans((rt_mat)),observed_rt = c(t(observed_rt_mat)))
ggplot(plot_df,aes(x=x,y=rt)) + geom_line(alpha=1)+ylim(c(0,5))+
  geom_line(data=data.frame(x=1:(time_sim-1),y=colMeans(observed_rt_mat)),aes(x=x,y=y),col='blue')  +
  geom_line(data=data.frame(x=1:time_sim,y=beta/gamma*sim[[2]][,1]/pop),aes(x=x,y=y),col='green')
  

