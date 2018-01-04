cat t

ll_pen_nb <- function(beta, X, y, offset, theta, lambda, S, ll_factor, lambda_factor, n) {

  # theta must be a matrix to use lgamma on

#
#  arma::mat theta_mat(1,1);
#  theta_mat.fill(theta);

  # mu = exp(eta) = exp(offset + X * beta)
  eta <- offset + X * beta
  mu <- exp(eta)

  # some other parts
  aux1 <- theta + y
  aux2 <- theta + mu
  lga <- lgamma(aux1)
  lgt <- lgamma(theta)
  lf <- lgamma(y + 1)

  # first part of equation with the gamma and factorial fucntion
  ls <- sum(lga - (lf + lgts))

  # second part of the equation
  #arma::vec rs = (trans(y) * eta) + (n * theta * log(theta)) - (trans(aux1) * log(aux2));
  rs <- (t(y) * eta) + (n * theta * log(theta)) - (trans(aux1) * log(aux2))

  # penalization term
  pen <- (t(beta) * S) * beta;

  # all together normalized by a factor
  res = (-1/ll_factor) * (ls + rs) + ((1/lambda_factor) * (lambda * pen))
}


gr_ll_pen_nb <- function(beta, X, XT, y, offset, theta, lambda, S) {
  # mu = exp(eta) = exp(offset + X * beta)
  mu <- exp(offset + X * beta)
  z <- (y - mu)/(1 + (mu/theta))
  pen <- S * beta
  gr <- XT *z
  ((-1) * gr) + (2 * lambda * pen)
}

compute_pen_hessian <- function(beta, X, XT, offset, y, S, lambda, theta) {
  mu <- exp(offset + X * beta)

  ls <- mu %% (1 + y/theta)
  rs <- (1 + mu/theta)^2
  dd <- ls/r;

  stop("TODO")

  #D = diagmat(sp_mat(dd));

  #arma::sp_mat H = XT * D * X;
  #arma::sp_mat pen = 2 * lambda * S;
  #arma::sp_mat res = H + pen;
  #return res;
}

compute_stdError <- function(X, H) {
  V = (X * H) %% X
  Diag = sum(V, 1)
  sqrt(Diag)
}

