model_string_pois<-
  "model
{
  # Likelihood
  for( t in 1:n.dates ){
     
    n.train[t] ~dpois(baseline[t])
    
  for(d in 1:(D+1)){
  
  n[t,d] ~ dpois(lambda[t,d])
  
  log(lambda[t,d]) <- (int +     
            beta.logged2[d]*step(D-d) +
            sum.beta.logged2[t]*(1-step(D-d)) +
                    sin26[t]*delta1[1] +
                    cos26[t]*delta1[2] +
                    sin52[t]*delta1[3] +
                    cos52[t]*delta1[4] +
                    epsilon1[epiyr.index[t]] +
                    phi[t] 
                    ) 
  }
    log(expected[t])<- 
        (int +      sin26[t]*delta1[1] +
                    cos26[t]*delta1[2] +
                    sin52[t]*delta1[3] +
                    cos52[t]*delta1[4] +
                    epsilon1[epiyr.index[t]] +
                    phi[t] 
           )
           
    #baseline is just standard harmonic reg, fit to train period     
    log(baseline[t])<- 
        (int +      sin26[t]*delta2[1] +
                    cos26[t]*delta2[2] +
                    sin52[t]*delta2[3] +
                    cos52[t]*delta2[4] +
                    epsilon2[epiyr.index[t]] )
    
  sum.n[t] <- sum(n[t,])  
  sum.lambda[t] <- expected[t]

  baseline.n[t] <-n.train[t]

  excessN[t] <- sum.n[t] - baseline.n[t]
  }

    #Time Series Random Effect
  phi[1] ~ dnorm(0, sigma2_phi_inv)
  for(t in 2:n.dates){
     phi[t] ~ dnorm((alpha*phi[t-1]), sigma2_phi_inv)
     }

  #Hyper-Prior Distributions
  sigma2_phi_inv <- 1/(sigma_phi*sigma_phi)
  sigma_phi ~ dunif(0, 1000)

  alpha ~ dunif(0,1)
     
     
   ##  
  int~dnorm(0, 1e-4)

  ## Prior for beta
  beta.logged <- log(beta)
  beta.logged2 <- c(beta.logged,0)
  beta ~ ddirch(beta.priors)
  
  tau.disp <- 1/sd.disp^2
  sd.disp~dunif(0,100)
  
  for( t in 1:n.dates ){
    sum.beta[t] <- sum(beta[1:N.first.obs[t]])
    sum.beta.logged2[t] <- log(sum.beta[t])
  }

  # Prior for variance
  tau2.alpha ~ dgamma(alphat.shape.prior,alphat.rate.prior)
  
  for(i in 1:4){
    delta1[i] ~ dnorm(0,1e-4)
    delta2[i] ~ dnorm(0,1e-4)

  }

  epsilon1[1]~dnorm(0,1e-4)
  epsilon2[1]~dnorm(0,1e-4)  
  for(k in 2:n.epiyr){
   epsilon1[k]~dnorm(epsilon1[k-1],prec.epsilon1)
   epsilon2[k]~dnorm(epsilon2[k-1],prec.epsilon2)
  }
  
  prec.epsilon1 <- 1/sd.epsilon1^2
  prec.epsilon2 <- 1/sd.epsilon2^2
  sd.epsilon1~dunif(0,100)
  sd.epsilon2~dunif(0,100)
  
  
}
"
