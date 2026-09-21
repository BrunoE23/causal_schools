# All fitted transformations learn exclusively from the passed training rows.
sl_preprocess <- function(raw) {
  raw <- as.matrix(raw); storage.mode(raw) <- 'double'
  raw[!is.finite(raw)] <- NA_real_
  med <- apply(raw,2,function(v) if(all(is.na(v))) 0 else median(v,na.rm=TRUE))
  missing <- is.na(raw)
  for (j in seq_len(ncol(raw))) raw[missing[,j],j] <- med[j]
  z <- cbind(raw,1.0*missing)
  colnames(z) <- c(colnames(raw),paste0(colnames(raw),'__missing'))
  center <- colMeans(z)
  scale <- sqrt(colMeans(sweep(z,2,center,'-')^2))
  keep <- is.finite(scale) & scale>1e-12
  list(medians=med,center=center,scale=scale,keep=keep,
       x=sweep(sweep(z[,keep,drop=FALSE],2,center[keep],'-'),2,scale[keep],'/'))
}

sl_transform <- function(raw, prep) {
  raw <- as.matrix(raw); storage.mode(raw) <- 'double'
  stopifnot(identical(colnames(raw),names(prep$medians)))
  raw[!is.finite(raw)] <- NA_real_
  missing <- is.na(raw)
  for (j in seq_len(ncol(raw))) raw[missing[,j],j] <- prep$medians[j]
  z <- cbind(raw,1.0*missing)
  colnames(z) <- c(colnames(raw),paste0(colnames(raw),'__missing'))
  sweep(sweep(z[,prep$keep,drop=FALSE],2,prep$center[prep$keep],'-'),2,prep$scale[prep$keep],'/')
}

sl_path <- function(raw,y,lambda) {
  stopifnot(all(is.finite(y)),length(y)==nrow(raw))
  pp <- sl_preprocess(raw)
  ym <- mean(y); ys <- sqrt(mean((y-ym)^2))
  if (!is.finite(ys) || ys<1e-12) stop('Constant training outcome.')
  if(ncol(pp$x)<2) stop('Too few nonconstant candidate features.')
  fit <- glmnet::glmnet(pp$x,(y-ym)/ys,alpha=1,family='gaussian',lambda=lambda,
    standardize=FALSE,intercept=TRUE,control=list(thresh=1e-10,maxit=1000000))
  stopifnot(fit$jerr==0L,length(fit$lambda)==length(lambda),max(abs(fit$lambda-lambda))<1e-14)
  list(prep=pp,fit=fit,ym=ym,ys=ys)
}

sl_predict <- function(model,raw,lambda) {
  model$ym+model$ys*as.matrix(predict(model$fit,newx=sl_transform(raw,model$prep),s=lambda))
}

sl_tune <- function(raw,y,fold,lambda) {
  losses <- matrix(NA_real_,5,length(lambda))
  stopifnot(setequal(unique(fold),1:5))
  for (k in 1:5) {
    tr <- which(fold!=k); te <- which(fold==k)
    model <- sl_path(raw[tr,,drop=FALSE],y[tr],lambda)
    pred <- sl_predict(model,raw[te,,drop=FALSE],lambda)
    losses[k,] <- colMeans((pred-y[te])^2)
  }
  cvm <- colMeans(losses); cvse <- apply(losses,2,sd)/sqrt(5)
  imin <- which.min(cvm)
  i1se <- which(cvm<=cvm[imin]+cvse[imin])[1]
  list(index=c(one_se=i1se,minimum=imin),
       curve=data.table(INDEX=seq_along(lambda),LAMBDA=lambda,CV_MSE=cvm,CV_SE=cvse),losses=losses)
}

sl_coefficients <- function(model,lambda) {
  b <- as.matrix(coef(model$fit,s=lambda))
  all_features <- names(model$prep$center)
  ans <- data.table(FEATURE=all_features,BETA_SD=0.0,TRAIN_CONSTANT=!model$prep$keep)
  ans[match(rownames(b)[-1],FEATURE),BETA_SD:=as.numeric(b[-1,1])]
  ans[,SELECTED:=BETA_SD!=0]
  ans
}

sl_kkt <- function(model,y,lambda) {
  b <- as.matrix(coef(model$fit,s=lambda))
  beta <- as.numeric(b[-1,1]); intercept <- b[1,1]
  yy <- (y-model$ym)/model$ys
  resid <- yy-intercept-as.numeric(model$prep$x %*% beta)
  grad <- as.numeric(crossprod(model$prep$x,resid))/length(y)
  active <- beta!=0
  violation <- ifelse(active,abs(grad-lambda*sign(beta)),pmax(abs(grad)-lambda,0))
  max(c(abs(mean(resid)),violation))
}
