hlmlogitsimev <- function (x, b, model, ci = 0.95, constant = 1) 
{
  mydata <- x$x
  if (any(class(x) == "counterfactual") && !is.null(x$model)) {
    
    x <- model.matrix(x$model, x$x)
    
  }
  else {
    if (any(class(x) == "list")) 
      x <- x$x
    if (is.data.frame(x)) 
      x <- as.matrix(x)
    if (!is.matrix(x)) {
      if (is.matrix(b)) {
        x <- t(x)
        if (!is.na(constant)) {
          x <- append(x, 1, constant - 1)
        }
      }
      else {
        x <- as.matrix(x)
        if (!is.na(constant)) {
          x <- appendmatrix(x, rep(1, nrow(x)), constant)
        }
      }
    }
    else {
      if (!is.na(constant)) {
        x <- appendmatrix(x, rep(1, nrow(x)), constant)
      }
    }
  }
  esims <- nrow(as.matrix(b))
  nscen <- nrow(x)
  nci <- length(ci)
  res <- list(pe = rep(NA, nscen), lower = matrix(NA, nrow = nscen, 
                                                  ncol = nci), upper = matrix(NA, nrow = nscen, ncol = nci))
  ranef_adjust_1 <- ranef(model)$VANID
  ranef_adjust_1$VANID <- row.names(ranef_adjust_1)
  ranef_adjust_2 <- ranef(model)$NYCCD 
  ranef_adjust_2$NYCCD <- row.names(ranef_adjust_2)
  
  for (i in 1:nscen) {
    simmu <- b %*% x[i, ]
    ran1 <- ranef_adjust_1$`(Intercept)`[ranef_adjust_1$VANID == mydatadata$VANID[i]]
    ran2 <- ranef_adjust_2$`(Intercept)`[ranef_adjust_1$NYCCD == data$NYCCD[i]]
    print(ran1)
    print(ran2)
    simmu <- simmu + ran1 + ran2
    simy <- 1/(1 + exp(-simmu))
    res$pe[i] <- mean(simy)
    for (k in 1:nci) {
      cint <- quantile(simy, probs = c((1 - ci[k])/2, 
                                       (1 - (1 - ci[k])/2)))
      res$lower[i, k] <- cint[1]
      res$upper[i, k] <- cint[2]
    }
  }
  res$lower <- drop(res$lower)
  res$upper <- drop(res$upper)
  res
}
