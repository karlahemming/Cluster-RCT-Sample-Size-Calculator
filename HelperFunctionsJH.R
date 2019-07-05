###############################################
# Functions to accompany K Hemming's shiny app
#
# J Kasza, 2018-02-08
#
# Modifications:
#
###############################################


#A function to output the design matrix
#To allow more compact representation, 
#this supposes one cluster for each treatment sequence.
DesignMatrix <- function(DesType, periods=1){
  #1="Parallel" 
  #2="Before and After", 
  #3="Cross-over", 
  #4="Stepped-wedge" ,
  #5="Longitudinal parallel", 
  #6="Multi cross-over"

  #KH modified 
 if(DesType == "Parallel" && periods==1){
    return(matrix(data=c(0,1), nrow=2, ncol=1))
  }
 else if(DesType == "Before and After"){ 
    return(matrix(data=c(0,0,0,1), nrow=2, ncol=2, byrow=TRUE))
 }
 else if(DesType == "Cross-over"){ 
    return(matrix(data=c(0,1,1,0), nrow=2, ncol=2, byrow=TRUE))
 } 
  else if(DesType == "Stepped-wedge"){ 
    desmat <- matrix(data=0, nrow=periods-1, ncol=periods)
    for(i in 1:nrow(desmat)) {
      desmat[i,(i+1):periods] <- 1
    }
    return(desmat)
  } 
  #KH modified 
  else if(DesType == "Parallel" && periods!=1){ 
    desmat <- matrix(data=c(rep(0,periods), rep(1, periods)), nrow=2, ncol=periods, byrow=TRUE)
    return(desmat)
  } 
  else if(DesType == "Multi cross-over"){ 
    if(periods%%2 == 1) return(matrix(data=c(rep(c(0,1),periods)), nrow=2, ncol=periods, byrow=TRUE))
    else if(periods%%2 == 0) return(matrix(data=c(rep(c(0,1),periods/2), rep(c(1,0), periods/2)), nrow=2, ncol=periods, byrow=TRUE))
  } 
  
}

#Variance of treatment effect estimator:
#Two functions are available to calculate this:
#   vartheta_m takes the number of subjects in each cluster-period as the first argument
#   vartheta_Krep takes the number of clusters per treatment sequence as first argument
#The desmat variants allow for design matrices with missing cluster-periods
# and for models with and without decays in between-period correlations. 

vartheta_m <- function(m, DesType, periods, Krep, icc, cac, iac, sd){
 #Assume Krep clusters per sequence
 #icc is the within-period ICC
 #cac is the cluster autocorrelation, 
 # what I have previously called r: decay between two periods
 #m is the number of clusters per period
 #iac is the individual-level autocorrelation
  
  totalvar <- sd^2
  sig2CP <- icc*totalvar
  r <- cac 
  sig2E <- (1-iac)*(totalvar - sig2CP)
  sig2 <- sig2E/m
  sigindiv <- sig2E*iac/((1-iac)*m)
  
  if(iac == 0){
    sig2E <- (totalvar - sig2CP)
    sig2 <- sig2E/m
    sigindiv <- 0
  }
  if(iac==1){
    sig2E <- 0*(totalvar - sig2CP)
    sig2 <- sig2E/m
    sigindiv <- (totalvar - sig2CP)/m
  }
  
  Xmat <- DesignMatrix(DesType, periods)
  T <- ncol(Xmat)
  #Xmat <- Xmat[sort(rep(1:nrow(Xmat), Krep)), ]
  K <- nrow(Xmat)
  Xvec <- as.vector(t(Xmat))
  
  #Variance matrix for one cluster, with decay in correlation over time
  Vi <- matrix(data=sigindiv, nrow=T, ncol=T) + diag(sig2,T) + sig2CP*(r^abs(matrix(1:T,nrow=T, ncol=T, byrow=FALSE) - matrix(1:T,nrow=T, ncol=T, byrow=TRUE)))

  vartheta <-  1/(t(Xvec)%*%(diag(1,K)%x%solve(Vi))%*%Xvec -colSums(Xmat)%*%solve(Vi)%*%(matrix(colSums(Xmat),nrow=T, ncol=1))/K )
  return(vartheta/Krep)
  
}

vartheta_Krep <- function(Krep, DesType, periods, m, icc, cac, iac, sd){
  #Assume Krep clusters per sequence
  #icc is the within-period ICC
  #cac is the cluster autocorrelation, 
  #what I have previously called r: decay between two periods
  #m is the number of clusters per period
  #iac is the individual-level autocorrelation
  
  totalvar <- sd^2
  sig2CP <- icc*totalvar
  r <- cac 
  sig2E <- (1-iac)*(totalvar - sig2CP)
  sig2 <- sig2E/m
  sigindiv <- sig2E*iac/((1-iac)*m)
  
  if(iac == 0){
    sig2E <- (totalvar - sig2CP)
    sig2 <- sig2E/m
    sigindiv <- 0
  }
  if(iac==1){
    sig2E <- 0*(totalvar - sig2CP)
    sig2 <- sig2E/m
    sigindiv <- (totalvar - sig2CP)/m
  }
  
  Xmat <- DesignMatrix(DesType, periods)
  T <- ncol(Xmat)
  #Xmat <- Xmat[sort(rep(1:nrow(Xmat), Krep)), ]
  K <- nrow(Xmat)
  Xvec <- as.vector(t(Xmat))
  
  #Variance matrix for one cluster, with decay in correlation over time
  Vi <- matrix(data=sigindiv, nrow=T, ncol=T) + diag(sig2,T) + sig2CP*(r^abs(matrix(1:T,nrow=T, ncol=T, byrow=FALSE) - matrix(1:T,nrow=T, ncol=T, byrow=TRUE)))
  
  vartheta <-  1/(t(Xvec)%*%(diag(1,K)%x%solve(Vi))%*%Xvec -colSums(Xmat)%*%solve(Vi)%*%(matrix(colSums(Xmat),nrow=T, ncol=1))/K )
  return(vartheta/Krep)
  
}

vartheta_Krep_desmat <- function(Krep, desmat, m, icc, cac, iac, sd, type, icc_treat){
  #This function uses the user-uploaded design matrix, which may include missing values
  #Assume Krep clusters per sequence
  #icc is the within-period ICC
  #cac is the cluster autocorrelation, 
  # what I have previously called r: decay between two periods
  #m is the number of clusters per period
  #iac is the individual-level autocorrelation
  #KH addition
  #tau_squared is treatment effect variation across clusters
  
  totalvar <- sd^2
  sig2CP <- icc*totalvar
  r <- cac 
  sig2E <- (1-iac)*(totalvar - sig2CP)
  sig2 <- sig2E/m
  sigindiv <- sig2E*iac/((1-iac)*m)
  
#JH change begin 
  #KH added to derive tau_squared from ICC
  #sig2CP_KH <- icc*totalvar/(1-icc)
  #tau_squared<-((icc_treat*(sig2CP_KH+totalvar))-sig2CP_KH)/(1-icc_treat)
  #KH done
  tau_squared<-icc_treat^2
#JH change end
  
  if(iac == 0){
    sig2E <- (totalvar - sig2CP)
    sig2 <- sig2E/m
    sigindiv <- 0
  }
  if(iac==1){
    sig2E <- 0*(totalvar - sig2CP)
    sig2 <- sig2E/m
    sigindiv <- (totalvar - sig2CP)/m
  }
  
  Xmat <- desmat
  T <- ncol(Xmat)
  K <- nrow(Xmat)
  
  Xvec <- as.vector(t(Xmat))
  stackI <- matrix(rep(diag(1,T)), nrow=K*T, ncol=T, byrow=TRUE)
  Zmat <- cbind(stackI[!is.na(Xvec),], Xvec[!is.na(Xvec)])
  
  #KH modification to allow for varying effects of treatment across clusters
  for(i in 1:K){
    Vdash<-matrix(0,nrow=T,ncol=T)
    for(a in 1:T){
      for(b in 1:T){
        if(is.na(Xmat[i,a]) | is.na(Xmat[i,b])){Vdash[a,b]<-0} 
        else if (Xmat[i,a]==1 & Xmat[i,b]==1){Vdash[a,b]<-tau_squared}
      }}
  #KH end 
  #Variance matrix for one cluster, with decay in correlation over time
  #Constant decay var if type==0
  if(type==0) { 
    Vi <-matrix(data=sigindiv, nrow=T, ncol=T) + diag(sig2 +(1-r)*sig2CP, T) + matrix(data=sig2CP*r, nrow=T, ncol=T)
  }
  #exponential decay structure
  if(type==1) { 
    Vi <- matrix(data=sigindiv, nrow=T, ncol=T) + diag(sig2,T) + sig2CP*(r^abs(matrix(1:T,nrow=T, ncol=T, byrow=FALSE) - matrix(1:T,nrow=T, ncol=T, byrow=TRUE)))
  }
  #Variance matrix for all clusters
  
  #KH modification to allow for varying effects of treatment across clusters
  #Vall <- kronecker(diag(1,K), Vi)
  if(i==1){
    Vi<-Vi+Vdash
    Vall<-Vi}
  else {
    Vi<-Vi+Vdash
    Vall<-adiag(Vall,Vi)
  }
  }
  #KH end
  Vall <- Vall[!is.na(Xvec),!is.na(Xvec)]

  vartheta <- solve((t(Zmat)%*%solve(Vall)%*%Zmat))[ncol(Zmat),ncol(Zmat)]
  
  #there will be problems if Zmat is not of full column rank
  #if(rankMatrix(Zmat)[1] < ncol(Zmat)) return(NA)
  #else 
  return(vartheta/Krep)
  
}

#JH change begin
#vartheta_m_desmat <- function(m, desmat, Krep, icc, cac, iac, sd, type){
vartheta_m_desmat <- function(m, desmat, Krep, icc, cac, iac, sd, type, icc_treat){
#JH change end
  #This function uses the user-uploaded design matrix, which may include missing values
  #Assume Krep clusters per sequence
  #icc is the within-period ICC
  #cac is the cluster autocorrelation, 
  # what I have previously called r: decay between two periods
  #m is the number of clusters per period
  #iac is the individual-level autocorrelation
  
  
  totalvar <- sd^2
  sig2CP <- icc*totalvar
  r <- cac 
  sig2E <- (1-iac)*(totalvar - sig2CP)
  sig2 <- sig2E/m
  sigindiv <- sig2E*iac/((1-iac)*m)
  
#JH change begin 
  tau_squared<-icc_treat^2
#JH change end
  
  if(iac == 0){
    sig2E <- (totalvar - sig2CP)
    sig2 <- sig2E/m
    sigindiv <- 0
  }
  if(iac==1){
    sig2E <- 0*(totalvar - sig2CP)
    sig2 <- sig2E/m
    sigindiv <- (totalvar - sig2CP)/m
  }
  
  Xmat <- desmat
  T <- ncol(Xmat)
  K <- nrow(Xmat)
  
  Xvec <- as.vector(t(Xmat))
  stackI <- matrix(rep(diag(1,T)), nrow=K*T, ncol=T, byrow=TRUE)
  Zmat <- cbind(stackI[!is.na(Xvec),], Xvec[!is.na(Xvec)])
 
#JH change begin  
  #KH modification to allow for varying effects of treatment across clusters
  for(i in 1:K){
    Vdash<-matrix(0,nrow=T,ncol=T)
    for(a in 1:T){
      for(b in 1:T){
        if(is.na(Xmat[i,a]) | is.na(Xmat[i,b])){Vdash[a,b]<-0} 
       else if (Xmat[i,a]==1 & Xmat[i,b]==1){Vdash[a,b]<-tau_squared}
        }}
    #KH end 
#JH change end
    
  #Variance matrix for one cluster, with decay in correlation over time
  #Constant decay var if type==0
  if(type==0) { 
    Vi <-matrix(data=sigindiv, nrow=T, ncol=T) + diag(sig2 +(1-r)*sig2CP, T) + matrix(data=sig2CP*r, nrow=T, ncol=T)
  }
  #exponential decay structure
  if(type==1) { 
    Vi <- matrix(data=sigindiv, nrow=T, ncol=T) + diag(sig2,T) + sig2CP*(r^abs(matrix(1:T,nrow=T, ncol=T, byrow=FALSE) - matrix(1:T,nrow=T, ncol=T, byrow=TRUE)))
  }
  #Variance matrix for all clusters
    #KH modification to allow for varying effects of treatment across clusters
    #Vall <- kronecker(diag(1,K), Vi)
    if(i==1){
      Vi<-Vi+Vdash
      Vall<-Vi}
    else {
      Vi<-Vi+Vdash
      Vall<-adiag(Vall,Vi)
    }
  }
  #KH end
  Vall <- Vall[!is.na(Xvec),!is.na(Xvec)]
  
  vartheta <- solve((t(Zmat)%*%solve(Vall)%*%Zmat))[ncol(Zmat),ncol(Zmat)]
  
  #there will be problems if Zmat is not of full column rank
  #if(rankMatrix(Zmat)[1] < ncol(Zmat)) return(NA)
  #else 
  return(vartheta/Krep)
}


