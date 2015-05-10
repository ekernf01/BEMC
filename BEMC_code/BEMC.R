#Basis Expansion Monte Carlo
#Eric Kernfeld, University of Washington
#eric.kern13@gmail.com
#
#
#This code implements the parallel MCMC technique described in ...
#The current implementation works only for samplers on R^n.
#
#Inputs: 
# omega_dim --    dimension of the space where the sampler runs.  
# sampler --      a Metropolis-Hastings sampler with target \pi. 
#                 Should be an R function that accepts a vector for 
#                 input and produces a vector for output, both length omega_dim.
# num_samples --  How many samples to draw for each
# basis_len --    How many basis functions to use


#this lets you evaluate and sample MVNs by passing the Cholesky factor of the covariance
require("LaplacesDemon") 


BEMC <- function(sampler, omega_dim, num_samples, basis_len){
  #To do: sanitize input
  #To do: optimize and parallelize the shit out of this
  #To do: derive and implement gradient scheme for h params
  locations = array(rnorm(basis_len*omega_dim), c(basis_len, nrow=omega_dim))
  cov_chol_factors = array(0, dim=3, c(basis_len, omega_dim, omega_dim))
  for(i in 1:basis_len){
    temp_mat = matrix(rnorm(omega_dim*omega_dim), ncol=omega_dim, nrow=omega_dim)
    cov_chol_factors[i,,] = chol(t(temp_mat)%*%temp_mat)
  }
  for(i in 1:basis_len){
    for(j in 1:basis_len){
      #Draw N std normal samples of size omega_dim
      Z <- rmvnc(num_samples, mu=locations[i,], U=cov_chol_factors[k,,])
      W <- apply(FUN=sampler, X=Z, margin=1)
      g[i,j] = dmvnc(x=????, mu=locations[i,], U=cov_chol_factors[k,,])
    }
  }
}