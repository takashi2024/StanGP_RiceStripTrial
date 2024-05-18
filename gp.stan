data {
  int<lower=1> N;
  matrix[N, N] distances;
  int K; // number of columns in the model matrix
  matrix[N,K] X; // the model matrix
  vector[N] y;
}
parameters {
  // For main model
  real<lower=0> rho;
  real<lower=0> alpha;
  real<lower=0> sigma;
  vector[K] beta;
}
model {
  matrix[N, N] COV;
  COV = square(alpha) * exp(-0.5 * (square(distances / rho))) + diag_matrix(rep_vector(sigma, N));
  matrix[N, N] L_K = cholesky_decompose(COV);

  alpha ~ std_normal();
  sigma ~ std_normal();
  rho ~ normal(2, 2.5);
  beta ~ std_normal();
  y ~ multi_normal_cholesky(X * beta, L_K);
}
