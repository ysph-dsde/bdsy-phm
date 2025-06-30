
jcode <- "
model {
  for (i in 1:N) {
    cases[i] ~ dpois(lambda[i])
    log(lambda[i]) <- mu[sero_id[i]] + beta[sero_id[i]] * (year_id[i] - mean_year)
  }

  for (s in 1:n_sero) {
    mu[s] ~ dnorm(0, 0.001)
    beta[s] ~ dnorm(0, 0.001)
  }

  mean_year <- (n_year + 1) / 2
}
"