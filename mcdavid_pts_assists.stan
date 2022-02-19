// This Stan program defines a simple model, with a
// vector of values 'y' modeled as normally distributed
// with mean 'mu' and standard deviation 'sigma'.
//
// Learn more about model development with Stan at:
//
//    http://mc-stan.org/users/interfaces/rstan.html
//    https://github.com/stan-dev/rstan/wiki/RStan-Getting-Started
//

// The input data is a vector 'y' of length 'N'.

functions{
  real normal_lub_rng(real mu, real sigma, real lb, real ub) {
  real p_lb = normal_cdf(lb| mu, sigma);
  real p_ub = normal_cdf(ub| mu, sigma);
  real u = uniform_rng(p_lb, p_ub);
  real y = mu + sigma * Phi(u);
  return y;
}
}
data {
  int<lower=0> N;
  int Assists[N];
  int Goals[N];
  int opponent[N];
  int season[N];
  vector[N] home;

  
  
  int ns;
  int nt;
  
  int N2;
  
  int opponent_remain[N2];
  int home_remain[N2];
  int season_remain[N2];
  
  real cur_goals;
  real cur_assists;
  
}


parameters {

  matrix[nt,ns] mu_assists_team_0;
  matrix[nt,ns] mu_goals_team_0;
  vector[ns] mu_assists_t_0;
  vector[ns] mu_goals_t_0;
  
  real<lower=0> sigma_assists_t;

  real<lower=0> sigma_goals_t;
  real<lower=0> sigma_assists_team;
  real<lower=0> sigma_goals_team;

  vector[3] b_home;
  
  real beta_goal;
  
  real<lower = 0, upper =1> rho_a;
  real<lower=0, upper =1> rho_g;
}

transformed parameters{
  
  
   matrix[nt,ns] mu_assists_team;
  matrix[nt,ns] mu_goals_team;
  vector[ns] mu_assists_t;
  vector[ns] mu_goals_t;
  

  
  mu_assists_t[1] = mu_assists_t_0[1];
  mu_goals_t[1] = mu_goals_t_0[1];
  
   for(i in 2:ns){
   mu_assists_t[i] = mu_assists_t[i-1] + mu_assists_t_0[i]*sigma_assists_t;
   mu_goals_t[i] = mu_goals_t[i-1] + mu_goals_t_0[i]*sigma_goals_t;
 
 }
 
 
 mu_assists_team[:,1] = mu_assists_team_0[:,1];
 mu_goals_team[:,1] = mu_goals_team_0[:,1];

  
 for(i in 2:ns){
  for(j in 1:nt){
    mu_assists_team[j,i] = rho_a*mu_assists_team[j,(i-1)] + mu_assists_team_0[j,i]*sigma_assists_team;
    mu_goals_team[j,i] = rho_g*mu_goals_team[j,(i-1)] + mu_goals_team_0[j,i]*sigma_goals_team;
  }
}
  
  
}


model {
  
  


vector[N] mn_a;
vector[N] mn_g;

for(i in 1:N){

  mn_a[i] = mu_assists_t[season[i]]+ mu_assists_team[opponent[i],season[i]]+ home[i]*b_home[2] + Goals[i]*beta_goal;
  mn_g[i] = mu_goals_t[season[i]] + mu_goals_team[opponent[i],season[i]]+ home[i]*b_home[3];
  
}
  
  Assists ~ poisson_log(mn_a);
  Goals ~ poisson_log(mn_g);
  
  
  mu_assists_team_0[:,1] ~ normal(0,0.1);
  mu_goals_team_0[:,1] ~ normal(0,0.1);
  
  to_vector(mu_assists_team_0[:,2:ns]) ~ normal(0,1);
  to_vector(mu_goals_team_0[:,2:ns]) ~ normal(0,1);
  
  
  mu_assists_t_0[1] ~ normal(-0.3,.5);
  mu_goals_t_0[1] ~ normal(-1.05,.5);
  
  for(i in 2:ns){
  mu_assists_t_0[i] ~ normal(0,1);
  mu_goals_t_0[i] ~ normal(0,1);
  }
  
  
  
  
  sigma_assists_t ~ normal(0,0.25)T[0,];
  sigma_goals_t ~ normal(0,0.25)T[0,];
  sigma_assists_team ~ normal(0,0.08)T[0,];
  sigma_goals_team ~ normal(0,0.08)T[0,];


  b_home ~ normal(0,0.5);
  beta_goal ~ normal(0,0.5);


rho_a ~ normal(0,0.3)T[0,1];
rho_g ~ normal(0,0.3)T[0,1];


}generated quantities{
  
  vector[N2] pred_assists;
  vector[N2] pred_goals;
  
  real pred_total_assists;
  real pred_total_goals;
  real pred_total_points;
  
  real over_100;
  real over_200;
  real lb;
  lb = 0;
  
  for(i in 1:N2){
    
    
    pred_assists[i] = poisson_log_rng(mu_assists_t[season_remain[i]] + mu_assists_team[opponent_remain[i],season_remain[i]]+ home_remain[i]*b_home[2]);
    pred_goals[i] = poisson_log_rng(mu_goals_t[season_remain[i]] + mu_goals_team[opponent_remain[i],season_remain[i]]+ home_remain[i]*b_home[3]);

    
  }
  
  pred_total_assists = cur_assists + sum(pred_assists);
  pred_total_goals = cur_goals + sum(pred_goals);
  
  pred_total_points = pred_total_assists + pred_total_goals;
  
  if(pred_total_points >= 100){
    over_100 = 1.0;
  }else{
    over_100 = 0.0;
  }
  
  if(pred_total_points>= 200){
    over_200 = 1.0;
  }else{
    over_200 = 0.0;
  }
  
  
}

