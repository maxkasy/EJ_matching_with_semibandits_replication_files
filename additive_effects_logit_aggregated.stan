data {
  int<lower=1> dim_U; //number of variables in U
  int<lower=1> dim_V; //number of variables in V
  int<lower=0> n[dim_U*dim_V]; //number of observations for each combination of U and V
  int<lower=0> s[dim_U*dim_V]; //number of successes for each combination of U and V
}
parameters {
  real<lower=0> betabar;
  real<lower=0> tau[3]; //variance components for beta
  real eta_U[dim_U]; //unscaled  effects
  real eta_V[dim_V]; //unscaled  effects
  real eta_UV[dim_U * dim_V]; //remainder effect 
}
transformed parameters {
  vector[dim_U*dim_V] beta;
  for (i in 1:dim_U){
      for (j in 1:dim_V){    
        beta[i + dim_U*(j-1)] = betabar +
          tau[1] * eta_U[i] + tau[2] * eta_V[j] + tau[3] * eta_UV[i + dim_U*(j-1)];
      }
  } 
}
model {
  target += std_normal_lpdf(betabar); // prior log-density for variance parameters
  target += std_normal_lpdf(tau); // prior log-density for variance parameters
  
  target += std_normal_lpdf(eta_U);       // prior log-density
  target += std_normal_lpdf(eta_V);       // prior log-density
  target += std_normal_lpdf(eta_UV);       // prior log-density
  
  target += binomial_logit_lpmf(s | n, beta); // log-likelihood  
}

