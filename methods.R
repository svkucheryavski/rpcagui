getModel = function(project, input, P) {
  T = project$data %*% P
  expvar = rep(0, ncol(P))
  totvar = sum(project$data^2)
  for (i in 1:ncol(P)) {
    xx = T[, i, drop = F] %*% t(P[, i, drop = F])
    expvar[i] = sum(xx^2) / totvar
  }
  model = list(P = P, T = T, expvar = expvar)
  model
}

getP = function(project, input) {
  if (input$method == 'svd'){
    if (input$algorithm == 'rand')
      P = nipalspca(getB(project$data, k = input$ncomp, q = input$q, p = input$p), input$ncomp)
    else
      P = svdpca(project$data, input$ncomp)    
  } else if (input$method == 'nipals') {
    if (input$algorithm == 'rand')
      P = nipalspca(getB(project$data, k = input$ncomp, q = input$q, p = input$p), input$ncomp)
    else
      P = nipalspca(project$data, input$ncomp)    
  } else if (input$method == 'eigcov') {
    if (input$algorithm == 'rand')
      P = nipalspca(getB(project$data, k = input$ncomp, q = input$q, p = input$p), input$ncomp)
    else
      P = svdpca(project$data, input$ncomp)    
  }
  
  P
}

getB = function(X, k = NULL, q = 0, p = 0, dist = 'unif') {
  nrows = nrow(X)
  ncols = ncol(X)
  
  if (is.null(k))
    k = ncols
  
  l = k + p
  if (dist == 'unif')
    Y = X %*% matrix(runif(ncols * l, -1, 1), ncols, l)  
  else
    Y = X %*% matrix(rnorm(ncols * l), ncols, l)  
  
  Q = qr.Q(qr(Y))
  if (q > 0) {  
    for (i in 1:niter) {
      Y = crossprod(X, Q)
      Q = qr.Q(qr(Y))
      Y = X %*% Q
      Q = qr.Q(qr(Y))
    }
  }
  
  B = crossprod(Q, X)
  B
}

svdpca = function(X, ncomp = 4) {
  P = svd(X, nu = ncomp, nv = ncomp)$v
  P = P[, 1:ncomp]
  P
}

nipalspca = function(X, ncomp = 4) {
  nobj = nrow(X)
  nvar = ncol(X)   
  
  P = matrix(0, nrow = nvar, ncol = ncomp)
  
  E = X
  for (i in 1:ncomp) {      
    ind = which.max(apply(E, 2, sd))
    t = E[, ind, drop = F]
    tau = 9999999999999999
    th = 99999999999999
    t0 = t - t
    while (sqrt(as.vector(crossprod(t - t0))) > 0.00001) {      
      p = crossprod(E, t) / as.vector(crossprod(t))
      p = p / as.vector(crossprod(p)) ^ 0.5
      t0 = t
      t = (E %*% p)/as.vector(crossprod(p))
    }
    
    E = E - tcrossprod(t, p)
    P[, i] = p
  }
  P
}

eigcovpca = function(X, ncomp = 4) {
  covX = cov(X) / (nrow(X) - 1)
  e = eigen(covX)
  P = e$vectors[, 1:ncomp]
  P
}

