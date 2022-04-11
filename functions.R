

########################################### Functions - estimation #######################################

## The function coef.fun() returns a drift or a diffusion function

coef.fun<-function(fun.name,A){
   if(fun.name=="drift.model1") out<-function(x) (1-x)*(x>-A)*(x<A);                                                           
   if(fun.name=="drift.model2") out<-function(x) (1-x)*(x>-A)*(x<A);                                       
   if(fun.name=="drift.model3") out<-function(x) (5-2*x)*(-1<=x)*(x<1)*(x>-A)*(x<A)+3*(x>=1)*(x>-A)*(x<A);                  
   if(fun.name=="drift.model4") out<-function(x) tanh(x)*(x>-A)*(x<A);                                                             # estimate - [-1,1]
   if(fun.name=="drift.model5") out<-function(x) -2*x/(sqrt(1+x**2))*(x>-A)*(x<A)                                                  # estimate - [-1,1]
   if(fun.name=="drift.model6") out<-function(x) (3/sqrt(0.8*pi))*(exp(-(4*x-2)**2/(0.8))+exp(-(4*x+2)**2/(0.8)))*(x>-A)*(x<A);    # estimate - [-1,1] - N=2000, n=500
   if(fun.name=="drift.model7") out<-function(x) 0.6*(exp(-x**2)+cos(10*x)+sin(5*x))*(x>-A)*(x<A)                                  # estimate - [-1,1] - N=2000, n=500
   if(fun.name=="drift.model8") out<-function(x) (-1-x)*(x>-A)*(x<A)  
   if(fun.name=="drift.model9") out<-function(x) (1+x)*(x>-A)*(x<A) 
   if(fun.name=="drift.model10") out<-function(x) (-5.5+1*x)*(-1<=x)*(x<0.5)*(x>-A)*(x<A)-3.5*(x>=0.5)*(x>-A)*(x<A);
   if(fun.name=="drift.model11") out<-function(x) 0.1*(-sin(2*pi*x)+cos(2*pi*x)+16*sin(3*pi*x)-5*cos(3*pi*x))*(x>-A)*(x<A)         # estimate - [-2,2] - N=2000, n=500
   if(fun.name=="drift.model12") out<-function(x) 0.5*(cos(x)+sin(x))*(x>-A)*(x<A)                                                 # estimate - [-1,1]
   if(fun.name=="drift.model13") out<-function(x) -(2+exp(-x**2))*(x>-A)*(x<A)                                                     # estimate - [-2.5,0] - 
   if(fun.name=="drift.model14") out<-function(x) (x>-A)*(x<A)*(-x+0.3)/(1+x**2)                                                   # estimate - [-1,1]                              
   if(fun.name=="drift.model15") out<-function(x) 0.85*(3-cos(x))*(x>-A)*(x<A)                                                     # estimate - [0,2]
   if(fun.name=="drift.model16") out<-function(x) 2.5*(1+sin(x)**2)*(x>-A)*(x<A)                                                   # estimate - [0,4]
   if(fun.name=="drift.model17") out<-function(x) (-2+cos(x)**2)*(x>-A)*(x<A)                                                      # estimate - [-1,0]
   
   if(fun.name=="sig.model1") out<-function(x) sapply(x,function(a) 0.3); if(fun.name=="sigx.model1") out<-function(x) 0;
   if(fun.name=="sig.model2") out<-function(x) sqrt(x); if(fun.name=="sigx.model2") out<-function(x) 1/(2*sqrt(x));
   if(fun.name=="sig.model3") out<-function(x) 1-x**2; if(fun.name=="sigx.model3") out<-function(x) -2*x;
   if(fun.name=="sig.model4") out<-function(x) 0.1+0.9/(sqrt(1+x**2)); if(fun.name=="sigx.model4") out<-function(x) -0.9*x*(1+x**2)**(-1.5); # calibre
   if(fun.name=="sig.model5") out<-function(x) 0.1+0.9/(sqrt(1+x**2)); if(fun.name=="sigx.model5") out<-function(x) -0.9*x*(1+x**2)**(-1.5)  # calibre
   if(fun.name=="sig.model6") out<-function(x) 0.1+0.9/(sqrt(1+x**2)); if(fun.name=="sigx.model6") out<-function(x) -0.9*x*(1+x**2)**(-1.5)  # calibre
   if(fun.name=="sig.model7") out<-function(x) 0.1+0.9/(sqrt(1+x**2)); if(fun.name=="sigx.model7") out<-function(x) -0.9*x*(1+x**2)**(-1.5)  # calibre
   if(fun.name=="sig.model8") out<-function(x) 2; if(fun.name=="sigx.model8") out<-function(x) 0;                                  # calibre2
   if(fun.name=="sig.model9") out<-function(x) (2-cos(x))/2; if(fun.name=="sigx.model9") out<-function(x) sin(x)/2;                # calibre2
   if(fun.name=="sig.model10") out<-function(x) 0.5; if(fun.name=="sigx.model10") out<-function(x) 0;                              # calibre2
   if(fun.name=="sig.model11") out<-function(x) 2-cos(x); if(fun.name=="sigx.model11") out<-function(x) sin(x);                    # calibre2
   if(fun.name=="sig.model12") out<-function(x) 2-cos(x); if(fun.name=="sigx.model12") out<-function(x) sin(x);                    # calibre2
   
   if(fun.name=="drift") out<-function(x) drift(x);
   if(fun.name=="drift1") out<-function(x) drift1(x);
   if(fun.name=="drift2") out<-function(x) drift2(x);
   if(fun.name=="drift3") out<-function(x) drift3(x);
   if(fun.name=="drift4") out<-function(x) drift4(x);
   if(fun.name=="drift5") out<-function(x) drift5(x);
   if(fun.name=="sig") out<-function(x) sig(x);
   if(fun.name=="sig.x") out<-function(x) sig.x(x);
   if(fun.name=="sig.square") out<-function(x) sig(x)*sig(x)

   return(out)
}



## Simulation

model.sim<-function(n,N,model){
  d<-expression(drift(x))
  d1<-expression(drift1(x)); d4<-expression(drift4(x));
  d2<-expression(drift2(x)); d5<-expression(drift5(x));
  d3<-expression(drift3(x));
  s<-expression(sig(x)); s.x<-expression(sig.x(x))
  if(model=="model1") X=sde.sim(t0=0,T=1,X0=0,N=n+1,M=N,theta=c(1,1,1),model="OU")
  if(model=="model2") X=sde.sim(t0=0,T=1,X0=0,N=n+1,M=N,theta=c(1,1,1),model="CIR")
  if(model=="drift") X=sde.sim(t0=0,T=1,X0=0,N=n+1,M=N,drift=d,sigma=s,sigma.x=s.x)
  if(model=="drift1") X=sde.sim(t0=0,T=1,X0=0,N=n+1,M=N,drift=d1,sigma=s,sigma.x=s.x)
  if(model=="drift2") X=sde.sim(t0=0,T=1,X0=0,N=n+1,M=N,drift=d2,sigma=s,sigma.x=s.x)
  if(model=="drift3") X=sde.sim(t0=0,T=1,X0=0,N=n+1,M=N,drift=d3,sigma=s,sigma.x=s.x)
  if(model=="drift4") X=sde.sim(t0=0,T=1,X0=0,N=n+1,M=N,drift=d4,sigma=s,sigma.x=s.x)
  if(model=="drift5") X=sde.sim(t0=0,T=1,X0=0,N=n+1,M=N,drift=d5,sigma=s,sigma.x=s.x)

  return(X)
}


## Function Z.fun() compute the vector Z for the estimation of the drift function

Z.fun<-function(Mx,delta){
  if(ncol(Mx)==1) result <- diff(Mx[,1])[-1]/delta
  if(ncol(Mx)>1){
    result <- diff(Mx[,1])[-1]/delta
    for (i in 2:dim(Mx)[2]){
    result <- c(result, (diff(Mx[,i])[-1])/delta)
  }
}  
  return(result)
}


## Function U.fun() compute the vector U for the diffusion coefficient's estimation

U.fun<-function(Mx,delta){
  if(ncol(Mx)==1) result <- diff(Mx[,1])[-1]/delta
  if(ncol(Mx)>1){
    result <- diff(Mx[,1])[-1]/delta
    for (i in 2:dim(Mx)[2]){
    result <- c(result, (diff(Mx[,i])[-1])/delta)
  }
}
  
  return(delta*(result**2))
}

## Function returning the cloud of dots for the estimation of drift or diffusion coefficient

cloud.fun<-function(fun.name,X,Z){
  fun=coef.fun(fun.name);
  x<-as.vector(X[1:n,]); b<-fun(x);
  data<-data.frame(x=x,b.values=b,z=Z)
  plt<-ggplot(data)+geom_point(aes(x=x,y=z),colour="yellow")+geom_line(aes(x=x,y=b.values),colour="black",size=1.5)+ggtitle(fun.name)+xlab("x-axis")+ylab("y-axis")

  return(plt)
}


## The function lab.fun() return vector of 0 and 1

lab.fun <- function(x,lower,upper) sapply(x,function(a) (a >= lower)*(a <= upper))


## Function bound.fun() replace each component of x that is not in the interval [lower,upper] by lower

bound.fun <- function(x,lower,upper) sapply(x,function(a) (a-lower)*(a >= lower)*(a <= upper)+lower)


## The function bspline() evaluates the B-spline basis' functions

bspline <- function(x,lower,upper,K,M){
  y <- bound.fun(x,lower,upper)
  id <- diag(lab.fun(x,lower,upper))
  dm <- K+M
  bs_basis <- create.bspline.basis(rangeval=c(lower,upper),nbasis=dm)
  v.basis <- eval.basis(y, basisobj = bs_basis)
  if(length(id)!=0) result <- id%*%v.basis
  if(length(id)==0) result <- 0*v.basis
  
  return(result)
}


## The function B.fun() returns matrix B

B.fun <- function(Mx,lower,upper,K,M){
  N = ncol(Mx)                                             # size of the sub-sample
  n = nrow(Mx)                                             # size of each path
  df.X <- as.data.frame(Mx[2:(n-1),])
  l.X <- as.list(df.X)
  B. <- lapply(l.X,function(x) bspline(x,lower,upper,K,M))
  B.. <- lapply(B.,function(x) as.data.frame(x))
  B <- rbindlist(B..)

  return(as.matrix(B))
}


## Estimate of drift functions and diffusion coefficient

ridge <- function(x,matrix.B,vector.Z,K,M,L){
  upper_bd = (K+M)*L;
  u = ginv(t(matrix.B)%*%matrix.B + x*diag(K+M))%*%t(matrix.B)%*%vector.Z
  result = sum(u^2) - upper_bd

  return(result)
}


optim.fun <- function(matrix.B,vector.Z,K,M,L){
  upper_bound <- sqrt(L*(K+M))
  estimator <- ginv(t(matrix.B)%*%matrix.B)%*%t(matrix.B)%*%vector.Z;
  norm_estimator <- sqrt(sum(estimator^2))
  Px = t(matrix.B) %*% matrix.B
  if(det(Px) != 0 & norm_estimator <= upper_bound){
    result <- estimator
  }else{
    ridge.bis <- function(x) ridge(x,matrix.B,vector.Z,K,M,L)
    root = multiroot(ridge.bis, c(0.01));
    lambda <- root$root
    result <- ginv(t(matrix.B)%*%matrix.B+lambda*diag(K+M))%*%t(matrix.B)%*%vector.Z;
  }

  return(result)
}


## drift estimator

sign<-function(x) -1*(x<=0)+1*(x>0)
estim.bound<-function(x,L.N) sapply(x,function(a) a*(abs(a)<=sqrt(L.N))+sign(a)*sqrt(L.N)*(abs(a)>sqrt(L.N))) 

drift.bs <- function(x, a.ch, lower, upper, K, M, L.N){
  lab <- lab.fun(x, lower, upper)
  id.mat <- diag(lab)
  bsMat = bspline(x, lower, upper, K, M)                # Use of the bspline functions
  if(length(id.mat)!=0){
    bs.Mat <- id.mat%*%bsMat;
    b.values <- bs.Mat%*%a.ch
}
  if(length(id.mat)==0) b.values <- 0*a.ch;
  b.values<-estim.bound(b.values,L.N)  

  return(as.vector(b.values))                                                          
}


l2norm<-function(fun.name,a.ch,lower,upper,K,M,L.N,a,b,N){
  drift=coef.fun(fun.name)
  vect<-seq(a,b,1/N)
  f.v<-sapply(vect,function(x) (drift.bs(x, a.ch, lower, upper, K, M, L.N)-drift(x))**2)
  return(sum(f.v)*(b-a)/N)
}


## diffusion coefficient estimator

zero.fun <- function(x) sapply(x,function(a) (a-0.01)*(a>0)+0.01)

diffusion.bs <- function(x,alpha.ch,lower,upper,K,M){
  lab <- lab.fun(x, lower, upper)
  id.mat <- diag(lab)
  bsMat = bspline(x, lower, upper, K, M)                # Use of the bspline functions
  if(length(id.mat)!=0) bs.Mat <- id.mat%*%bsMat
  if(length(id.mat)==0) bs.Mat <- 0*bsMat
  s.values = bs.Mat%*%alpha.ch                            
  result <- zero.fun(as.vector(s.values))

  return(result) 
}


## The average error

squareerror<-function(coef.name,matrix.X,a.ch,lower,upper,K,M,L.N){
  coeff=coef.fun(coef.name);
  result<-apply(matrix.X,2,function(x) mean(((drift.bs(x,a.ch,lower,upper,K,M,L.N)-coeff(x))**2)*(x>=lower)*(x<=upper)));

  return(mean(result))
}


## The function gamma_pen() is the penalized gamma function

gamma_pen<-function(x,c,K,M,Z,B,n,N) (1/(n*N))*sum((Z-B%*%x)**2)+c*(log(N))*(K+M)/N;
gamma_pen2<-function(x,c,K,M,U,B,n,N) (1/(n*N))*sum((U-B%*%x)**2)+c*log(N)*(K+M)/(N*n);

## The function kerror_drift() returns the error estimate with optimal K for a given value of c (drift function)

kerror_drift<-function(fun.name,c,X,D,set.K,M,lower,upper,delta){
  n<-nrow(X)-1; N<-ncol(X); N.<-ncol(D); Z<-Z.fun(X,delta)
  B.ls<-lapply(set.K,function(x) B.fun(X,lower,upper,x,M))
  L.N<-log(N)
  a.ls<-lapply(1:length(set.K),function(x) optim.fun(B.ls[[x]],Z,set.K[x],M,L.N))
  gpen.vec<-sapply(1:length(set.K),function(x) gamma_pen(a.ls[[x]],c,set.K[x],M,Z,B.ls[[x]],n,N))
  i.min<-which.min(gpen.vec)
  K.ch<-set.K[i.min]
  a.ch<-a.ls[[i.min]]

  error<-squareerror(fun.name,D,a.ch,lower,upper,K.ch,M,L.N)
  result=c(K.ch,error)

  return(result)
}


## The function kerror_drift() returns the error estimate with optimal K for a given value of c (drift function)

kerror_diffusion<-function(fun.name,c,X,D,set.K,M,lower,upper,delta){
  n<-nrow(X)-1; N<-ncol(X); N.<-ncol(D); U<-U.fun(X,delta)
  B.ls<-lapply(set.K,function(x) B.fun(X,lower,upper,x,M))
  L.N<-log(N)
  a.ls<-lapply(1:length(set.K),function(x) optim.fun(B.ls[[x]],U,set.K[x],M,L.N))
  gpen.vec<-sapply(1:length(set.K),function(x) gamma_pen2(a.ls[[x]],c,set.K[x],M,U,B.ls[[x]],n,N))
  i.min<-which.min(gpen.vec)
  K.ch<-set.K[i.min]
  a.ch<-a.ls[[i.min]]

  error<-squareerror(fun.name,D,a.ch,lower,upper,K.ch,M,L.N)
  result=c(K.ch,error)

  return(result)
}


## Functions kerror.drift() and kerror.sig return the value of K minimizing the error estimate and the corresponding error

## Drift function

kerror.drift<-function(fun.name,X,D,set.K,M,lower,upper,delta){
  n<-nrow(X)-1; N<-ncol(X); Z<-Z.fun(X,delta); L.N<-log(N);
  B.ls<-lapply(set.K,function(x) B.fun(X,lower,upper,x,M))
  a.ls<-lapply(1:length(set.K),function(x) optim.fun(B.ls[[x]],Z,set.K[x],M,L.N))
  er.vec<-sapply(1:length(set.K),function(x) squareerror(fun.name,D,a.ls[[x]],lower,upper,set.K[x],M,L.N))
  i.min<-which.min(er.vec)
  K.ch<-set.K[i.min]
  a.ch<-a.ls[[i.min]]
  
  error<-squareerror(fun.name,D,a.ch,lower,upper,K.ch,M,L.N)
  result<-c(K.ch,error)

  return(result)
} 


## Diffusion coefficient

kerror.sig<-function(fun.name,X,D,set.K,M,lower,upper,delta){
  n<-nrow(X)-1; N<-ncol(X); U<-U.fun(X,delta); L.N<-log(N+1);
  B.ls<-lapply(set.K,function(x) B.fun(X,lower,upper,x,M))
  a.ls<-lapply(1:length(set.K),function(x) optim.fun(B.ls[[x]],U,set.K[x],M,L.N))
  er.vec<-sapply(1:length(set.K),function(x) squareerror(fun.name,D,a.ls[[x]],lower,upper,set.K[x],M,L.N))
  i.min<-which.min(er.vec)
  K.str<-set.K[i.min]
  a.ch<-a.ls[[i.min]]
  
  error<-squareerror(fun.name,D,a.ch,lower,upper,K.str,M,L.N)
  result<-c(K.str,error)

  return(result)
} 


## The function drift.error() below returns the estimate error knowing K and c

drift.error<-function(fun.name,X,D,K,M,lower,upper,delta){
  Z<-Z.fun(X,delta); B<-B.fun(X,lower,upper,K,M); L.N<-log(ncol(X))
  a.ch<-optim.fun(B,Z,K,M,L.N)
  result<-squareerror(fun.name,D,a.ch,lower,upper,K,M,L.N)

  return(result)
}


## The function sig.error() below returns the estimate error (diffusion coefficient)

sig.error<-function(fun.name,X,D,K,M,lower,upper,delta){
  U<-U.fun(X,delta); B<-B.fun(X,lower,upper,K,M); L.N<-log(ncol(X)+1)
  a.ch<-optim.fun(B,U,K,M,L.N)
  result<-squareerror(fun.name,D,a.ch,lower,upper,K,M,L.N)

  return(result)
}


################################################################# Functions - Classification, general case ###########################################################################

## Simulation of a paths knowing the associated classes

class.sim <- function(n,Y,model.names){
  result <- sapply(Y,function(x) model.sim(n,1,model.names[x]))

  return(result)
}


split.fun <- function(X,Y,nlab){
  train=list()
  lss=list()
  for(i in 1:nlab) lss[[i]] <- which(Y==i);
  for(i in 1:nlab) train[[i]] <- X[,lss[[i]]]

  return(train)
}

## Function F.fun related to real drift function and diffusion coefficient

F.fun <- function(drift.name, sigma.name, path, delta){
  drift <- coef.fun(drift.name); sig.square<-coef.fun(sigma.name)
  n=length(path)-2;
  result <- (drift(path[1:n])/sig.square(path[1:n]))*diff(path)[-1]-(delta/2)*(drift(path[1:n])*drift(path[1:n])/sig.square(path[1:n]))
  
  return(sum(result))
}

## Function phi_i for each label i

phi <- function(i,x,p){
  mx<-max(x)
  result=p*exp(x-mx)
  
  return(p[i]*exp(x[i]-mx)/sum(result));
} 

## Bayes classifier in discrete time

#drift.vec <- c("drift1","drift2","drift3")

g.bar <- function(path,drift.vec,sigma.name,p,nlab,delta){
  F_T<- sapply(1:nlab, function(x) F.fun(drift.vec[x],sigma.name,path,delta))
  prob=sapply(1:nlab, function(x) phi(x,F_T,p))

  return(which(prob==max(prob)))
}


## Assessment of the risk of the Bayes classifier

r.bayes<-function(drift.vec,sigma.name,X_path,set.labels,p,nlab,delta){
  pred.bayes<-apply(X_path,2,function(x) g.bar(x,drift.vec,sigma.name,p,nlab,delta))
  result<-length(which(pred.bayes-set.labels!=0))/length(set.labels)

  return(result)
}


##################################################################### Functions - Classification for B-spline ##########################################################################

## Function F.bs, estimation of F_b.ch with a known diffusion coefficient

F.bs <- function(sigma.name,path,a.ch,lower,upper,K,M,delta,L.N){
  sig.square<-coef.fun(sigma.name); n=length(path)-2;
  result <- (drift.bs(path[1:n],a.ch,lower,upper,K,M,L.N)/sig.square(path[1:n]))*diff(path)[-1]-(delta/ 2)*(drift.bs(path[1:n],a.ch,lower,upper,K,M,L.N)*drift.bs(path[1:n],a.ch,lower,upper,K,M,L.N)/sig.square(path[1:n]));
  
  return(sum(result));
}


## Function F_bs, estimation of F_{b.ch,sigsquare.ch}

F_bs <- function(path,a.ch, alpha.ch,lower,upper,K.b,K.s,M,delta,L.N){
   n=length(path)-2;
   result <- (drift.bs(path[1:n],a.ch,lower,upper,K.b,M,L.N)/diffusion.bs(path[1:n],alpha.ch,lower,upper,K.s,M))*diff(path)[-1]-(delta/2)*(drift.bs(path[1:n],a.ch,lower,upper,K.b,M,L.N)*drift.bs(path[1:n],a.ch,lower,upper,K.b,M,L.N)/diffusion.bs(path[1:n],alpha.ch,lower,upper,K.s,M));

   return(sum(result))
}


## Empirical classifier, B-spline basis with known diffusion coef

g.bs <- function(sigma.name, path,a.ch,p.ch,nlab,v.lower,v.upper,v.K,M,L.N){
  F. <- sapply(1:nlab,function(x) F.bs(sigma.name, path,a.ch[[x]],v.lower[x],v.upper[x],v.K[x],M,delta,L.N));
  prob=sapply(1:nlab, function(x) phi(x,F.,p.ch))
  
  return(which(prob==max(prob)))
}


## Empirical classifier, B-spline basis with unknown diffusion coef

g_bs <- function(path,a.ch,alpha.ch,p.ch,nlab,v.lower,v.upper,v.K,K.s,M,L.N){
  F. <- sapply(1:nlab,function(x) F_bs(path,a.ch[[x]],alpha.ch,v.lower[x],v.upper[x],v.K[x],K.s,M,delta,L.N));
  prob=sapply(1:nlab, function(x) phi(x,F.,p.ch))
  
  return(which(prob==max(prob)))
}


## Computation of the risk of the empirical classifier with known diffusion coefficient

r.bs<-function(X_path,set.labels,a.ch,p.ch,nlab,v.lower,v.upper,v.K,M,L.N){
  pred.bs<-apply(X_path,2,function(x) g.bs(x,a.ch,p.ch,nlab,v.lower,v.upper,v.K,M,L.N))
  result<-length(which(pred.bs-set.labels!=0))/length(set.labels)

  return(result)
}


## Computation of the risk of the empirical classifier with unknown diffusion coefficient

r_bs<-function(X_path,set.labels,a.ch,alpha.ch,p.ch,nlab,v.lower,v.upper,v.K,K.s,M,L.N){
  pred.bs<-apply(X_path,2,function(x) g_bs(x,a.ch,alpha.ch,p.ch,nlab,v.lower,v.upper,v.K,K.s,M,L.N))
  result<-length(which(pred.bs-set.labels!=0))/length(set.labels)

  return(result)
}


############################################################## Hermite extension, non compact supported drift and diffusion coefficients #############################################################

#### Factorielle
factoriel<-function(n){
  res=1
  if(n==0) res=1; if(n==1) res=1;
  while(n>1){
    res<-res*n;
    n<-n-1;
  }
  return(res)
}


###### The Hermite functions

hermite.fun<-function(x,j){
  result=1;
  if(j==0) result=(1/sqrt(sqrt(pi)))*exp(-0.5*x*x);
  if(j==1) result=x*(1/sqrt(2*sqrt(pi)))*exp(-0.5*x*x);
  if(j>1){
    c.j=1/sqrt(2**(j)*factoriel(j)*sqrt(pi)); 
    n=1;term1=1;term2=1
    while(n<j){
      term0<-term1
      term1<-term2
      term2<-2*x*term2-2*n*term0;
      n<-n+1
    }
    result<-c.j*term2*exp(-0.5*x*x)
  }
  return(result)
}


###### The Hermite basis

hermite.basis<-function(x,D){
  values=matrix(ncol=D,nrow=length(x));
  for(j in 1:D) values[,j]<-hermite.fun(x,j-1);
  return(values)
}

###### We multiply each basis function by sigma

sig <- function(x) 0.5*(1.5-cos(x))

hermite.sb<-function(x,D){
  values=matrix(ncol=D,nrow=length(x));
  for(j in 1:D) values[,j]<-sig(x)*hermite.fun(x,j-1);
  return(values)
}


###### Representation of Hermite functions

hermite.values<-function(x,D){
  values=hermite.fun(x,0);
  for(j in 2:D) values<-cbind(values,hermite.fun(x,j-1));
  return(values)
}


hermite.rep<-function(a,b,D){
  x=seq(a,b,0.01);
  plt<-matplot(x,hermite.values(x,D),type="l",col=rainbow(D),ylab="Hermite functions")
  return(plt)
}


## The function phi.fun() returns matrix Phi

hermite.phi <- function(Mx,D){
  N = ncol(Mx)                                             # size of the sub-sample
  n = nrow(Mx)                                             # size of each path
  df.X <- as.data.frame(Mx[2:(n-1),])
  l.X <- as.list(df.X)
  B. <- lapply(l.X,function(x) hermite.basis(x,D))
  B.. <- lapply(B.,function(x) as.data.frame(x))
  B <- rbindlist(B..)
  
  return(as.matrix(B))
}

hermite.phisg <- function(Mx,D){
  N = ncol(Mx)                                             # size of the sub-sample
  n = nrow(Mx)                                             # size of each path
  df.X <- as.data.frame(Mx[2:(n-1),])
  l.X <- as.list(df.X)
  B. <- lapply(l.X,function(x) hermite.sb(x,D))
  B.. <- lapply(B.,function(x) as.data.frame(x))
  B <- rbindlist(B..)
  
  return(as.matrix(B))
}


## Estimate of drift functions 

hermite.ridge <- function(x,Phi,Z,N,n,L){
  Psi=(1/(N*n))*t(Phi)%*%Phi; D<-ncol(Phi)
  upper_bd = D*L;
  u = (1/(N*n))*(Psi + x*diag(D))%*%t(Phi)%*%Z
  result = sum(u^2) - upper_bd
  
  return(result)
}


hermite.optim <- function(Phi,Z,N,n,L){
  Psi=(1/(N*n))*t(Phi)%*%Phi; D<-ncol(Phi)
  upper_bound <- sqrt(L*D)
  estimator <- (1/(N*n))*ginv(Psi)%*%t(Phi)%*%Z;
  norm_estimator <- sqrt(sum(estimator^2))
  if(det(Psi) != 0 & norm_estimator <= upper_bound){
    result <- estimator;
  }else{
    ridge.bis <- function(x) hermite.ridge(x,Phi,Z,N,n,L)
    root = multiroot(ridge.bis, c(0.01));
    lambda <- root$root
    result <- (1/(N*n))*ginv(Psi+lambda*diag(D))%*%t(Phi)%*%Z;
  }
  
  return(estimator)
}

#### Estimator

drift.estimator<-function(x,theta.chp,D,lower,upper){
  result<-hermite.basis(x,D)%*%theta.chp; result<-as.numeric(result)
  return(result*(x>=lower)*(x<=upper))
}


#### Mean Square Error

hermite.mse<-function(Xp,Z,Phi,N,n,D,lower,upper,fun.name){
  drift=coef.fun(fun.name)
  Phi<-hermite.phi(X,D)
  Psi=(1/(N*n))*t(Phi)%*%Phi
  theta.chp<-(1/(N*n))*ginv(Psi)%*%t(Phi)%*%Z
  result<-apply(Xp,2,function(x) (x>=lower)*(x<=upper)*(drift(x)-drift.estimator(x,theta.chp,D,lower,upper))**2)
  result<-(1/(N*n))*sum(result)
  return(result)
}


#### Mean square error for ridge estimators

hermite.ridge_mse<-function(Xp,Z,Phi,N,n,D,L,lower,upper,fun.name){
  drift=coef.fun(fun.name)
  theta.chp<-hermite.optim(Phi,Z,N,n,L)
  result<-apply(Xp,2,function(x) (x>=lower)*(x<=upper)*(drift(x)-drift.estimator(x,theta.chp,D,lower,upper))**2)
  result<-(1/(N*n))*sum(result)
  return(result)
}


#### D star

hermite.Dstar<-function(set.D,X,Xp,Z,N,n,lower,upper,fun.name){
  result=c()
  for(D in set.D){
    Phi<-hermite.phi(X,D)
    result<-c(result,hermite.mse(Xp,Z,Phi,N,n,D,lower,upper,fun.name))
  }
  i.min<-which.min(result)
  return(set.D[i.min])
}
  
#### operator norm

norm.op<-function(Mx){
  eigenvalue<-eigen(Mx)
  return(max(eigenvalue$values))
}

#### Empirical norm

norm.Nn<-function(Xp,theta,D,lower,upper){
  vecteur<-apply(Xp,2,function(x) (drift.estimator(x,theta,D,lower,upper))**2)
  normNn<-(1/(N*n))*sum(vecteur)
  return(normNn)
}

#### Norm Nn

hermite.selection<-function(Xp,D.max,step,M.theta,N,n,lower,upper,sigcar_inf){
  set.D<-seq(0,D.max,step); set.D<-set.D[set.D>0]; 
  values<-sapply(set.D,function(x) (sigcar_inf*x)/(N)-norm.Nn(Xp,M.theta[1:x,x],x,lower,upper))
  i.min<-which.min(values)
  return(set.D[i.min])
}


hte.ridge_selection<-function(c,Xp,D.max,step,M.theta,N,n,lower,upper){
  set.D<-seq(0,D.max,step); set.D<-set.D[set.D>0]; 
  values<-sapply(set.D,function(x) (c*x*log(N)*log(N)/N)-norm.Nn(Xp,as.matrix(M.theta[1:x,x]),x,lower,upper))
  i.min<-which.min(values)
  return(set.D[i.min])
}

######################################################################### Functions - Classification for Hermite basis ###########################################################################

## Function F.bs, estimation of F_b.ch with a known diffusion coefficient

hermite.F <- function(sigma.name,path,theta.chp,D,lower,upper,delta){
  sig.square<-coef.fun(sigma.name); n=length(path)-2;
  result <- (drift.estimator(path[1:n],theta.chp,D,lower,upper)/sig.square(path[1:n]))*diff(path)[-1]-(delta/ 2)*(drift.estimator(path[1:n],theta.chp,D,lower,upper)*drift.estimator(path[1:n],theta.chp,D,lower,upper)/sig.square(path[1:n]));
  
  return(sum(result));
}


## Function F_bs, estimation of F_{b.ch,sigsquare.ch}

F.hte_bs <- function(path,theta.chp,alpha.ch,lower,upper,D,K.s,M,delta){
  n=length(path)-2;
  result <- (drift.estimator(path[1:n],theta.chp,D,lower,upper)/diffusion.bs(path[1:n],alpha.ch,lower,upper,K.s,M))*diff(path)[-1]-(delta/2)*(drift.estimator(path[1:n],theta.chp,D,lower,upper)*drift.estimator(path[1:n],theta.chp,D,lower,upper)/diffusion.bs(path[1:n],alpha.ch,lower,upper,K.s,M));
  
  return(sum(result))
}


## Empirical classifier, B-spline basis with known diffusion coef

g.hermite <- function(sigma.name, path,theta.chp,p.ch,nlab,v.lower,v.upper,v.D){
  F. <- sapply(1:nlab,function(x) hermite.F(sigma.name, path,theta.chp[[x]],v.D[x],v.lower[x],v.upper[x],delta));
  prob=sapply(1:nlab, function(x) phi(x,F.,p.ch))
  
  return(which(prob==max(prob)))
}


## Empirical classifier, B-spline basis with unknown diffusion coef

g.hte_bs <- function(path,theta.chp,alpha.ch,p.ch,nlab,v.lower,v.upper,v.D,K.s,M,delta){
  F. <- sapply(1:nlab,function(x) F.hte_bs(path,theta.chp[[x]],alpha.ch,v.lower[x],v.upper[x],v.D[x],K.s,M,delta));
  prob=sapply(1:nlab, function(x) phi(x,F.,p.ch))
  
  return(which(prob==max(prob)))
}


## Computation of the risk of the empirical classifier with known diffusion coefficient

r.hermite<-function(sigma.name,X_path,set.labels,theta.chp,p.ch,nlab,v.lower,v.upper,v.D){
  pred.bs<-apply(X_path,2,function(x) g.hermite(sigma.name,x,theta.chp,p.ch,nlab,v.lower,v.upper,v.D))
  result<-length(which(pred.bs-set.labels!=0))/length(set.labels)
  
  return(result)
}


## Computation of the risk of the empirical classifier with unknown diffusion coefficient

r.hte_bs<-function(X_path,set.labels,theta.chp,alpha.ch,p.ch,nlab,v.lower,v.upper,v.D,K.s,M,delta){
  pred.bs<-apply(X_path,2,function(x) g.hte_bs(x,theta.chp,alpha.ch,p.ch,nlab,v.lower,v.upper,v.D,K.s,M,delta))
  result<-length(which(pred.bs-set.labels!=0))/length(set.labels)
  
  return(result)
}


############################################################################### Classification with depth methods ####################################################################################

depth.classif<-function(X.test,y.test,X,y,nlab,method){
  N.path<-ncol(X.test); n.<-nrow(X); 
  train<-list()
  for(i in 1:nlab){
    train[[i]]<-X[,which(y==i)]
  }

  ### Depth, prediction and performance
  if(method=="depth.mode"){
    hm<-matrix(nrow=ncol(X.test),ncol=nlab)
    for(j in 1:N.path){
      for(i in 1:nlab){
      xj<-fdata(X.test[,j],1:n.)
      C<-train[[i]]; fC<-fdata(C,1:n.)
      hm[j,i]<-depth.mode(xj,fC)$dep;
    }
    }
    hm<-scale(hm)
    ### Prediction
    y.pred<-numeric(N.path)
    for(j in 1:N.path){
      y.pred[j]<-which.max(hm[j,])
   }
   ### Performance
    risk.classif <- length(which(y.pred-y.test!=0))/length(y.test)
  }

  if(method=="depth.RP"){
    hm<-matrix(nrow=ncol(X.test),ncol=nlab)
    for(j in 1:N.path){
      for(i in 1:nlab){
      xj<-fdata(X.test[,j],1:n.)
      C<-train[[i]]; fC<-fdata(C,1:n.)
      hm[j,i]<-depth.RP(xj,fC)$dep;
    }
    }
    hm<-scale(hm)
    ### Prediction
    y.pred<-numeric(N.path)
    for(j in 1:N.path){
      y.pred[j]<-which.max(hm[j,])
   }
   ### Performance
    risk.classif <- length(which(y.pred-y.test!=0))/length(y.test)
  }
  
  if(method=="depth.RPD"){
    hm<-matrix(nrow=ncol(X.test),ncol=nlab)
    for(j in 1:N.path){
      for(i in 1:nlab){
      xj<-fdata(X.test[,j],1:n.)
      C<-train[[i]]; fC<-fdata(C,1:n.)
      hm[j,i]<-depth.RPD(xj,fC)$dep
    }
    }
    hm<-scale(hm)
    ### Prediction
    y.pred<-numeric(N.path)
    for(j in 1:N.path){
      y.pred[j]<-which.max(hm[j,])
   }
   ### Performance
    risk.classif <- length(which(y.pred-y.test!=0))/length(y.test)
  }

  if(method=="depth.FM"){
    hm<-matrix(nrow=ncol(X.test),ncol=nlab)
    for(j in 1:N.path){
      for(i in 1:nlab){
      xj<-fdata(X.test[,j],1:n.)
      C<-train[[i]]; fC<-fdata(C,1:n.)
      hm[j,i]<-depth.FM(xj,fC)$dep
    }
    }
    hm<-scale(hm)
    ### Prediction
    y.pred<-numeric(N.path)
    for(j in 1:N.path){
      y.pred[j]<-which.max(hm[j,])
   }
   ### Performance
    risk.classif <- length(which(y.pred-y.test!=0))/length(y.test)
  }

  result=list(depth=hm,pred=y.pred,risk=risk.classif)
  return(result)
}



tukey.classif<-function(X.test,y.test,X,y,nlab){
  N.path<-ncol(X.test); n.<-nrow(X); 
  train<-list()
  for(i in 1:nlab){
    train[[i]]<-X[,which(y==i)]
  }

  N1=ncol(train[[1]]); N2=ncol(train[[2]]); N3=ncol(train[[3]])
  c1=train[[1]]; c2=train[[2]]; c3=train[[3]]
  ls1=list(); ls2=list(); ls3=list(); 

  for(k in 1:N1){
    l=list(coords=cbind(as.matrix(c1[,k]))); ls1<-append(ls1,l)
  }
  class1<-list(ls1)

  for(k in 1:N2){
    l=list(coords=cbind(as.matrix(c2[,k]))); ls2<-append(ls2,l)
  }
  class2<-list(ls2)

  for(k in 1:N3){
    l=list(coords=cbind(as.matrix(c3[,k]))); ls3<-append(ls3,l)
  }
  class3<-list(ls3)

  ### Additional step for depth.curve.Tukey
  m=100
  smp1<-sample.curves(class1,m)
  smp2<-sample.curves(class2,m)
  smp3<-sample.curves(class3,m)

  ### Computation of depths

  depthc<-matrix(ncol=3,nrow=N.path)
  depth.curve<-matrix(ncol=3,nrow=N.path)
  for(k in 1:N.path){
    ls=list(coords=cbind(as.matrix(X.test[,k])))
    d.test<-list(ls); sp<-sample.curves(d.test,m)
    depthc[k,1]<-depthc.Tukey(d.test,class1,m=m)                  
    depthc[k,2]<-depthc.Tukey(d.test,class2,m=m) 
    depthc[k,3]<-depthc.Tukey(d.test,class3,m=m)

    depth.curve[k,1]<-depth.curve.Tukey(sp,smp1)     
    depth.curve[k,2]<-depth.curve.Tukey(sp,smp2)
    depth.curve[k,3]<-depth.curve.Tukey(sp,smp3)
  }
 

  ### Classification

  pred.c=rep(NA,N.path); pred.curve=rep(NA,N.path)
  for(k in 1:N.path){
    pred.c[k]<-which.max(depthc[k,]); pred.curve[k]<-which.max(depth.curve[k,])
  } 
  risk.c <- length(which(pred.c-y.test!=0))/length(y.test)
  risk.curve <- length(which(pred.curve-y.test!=0))/length(y.test)
  result=list(depthc=depthc,depth.curve=depth.curve,pred.c=pred.c, pred.curve=pred.curve,risk.c=risk.c,risk.curve=risk.curve)
  return(result)
}


knn.classif<-function(X.train,X.test,y.train,y.test,k.){
  test<-X.test
  train<-X.train
  knn_classif<-knn(train=t(train),test=t(test),cl=y.train,k=k.);
  knn=rep(NA,length(y.test))
  for(i in 1:length(y.test)) knn[i]=as.numeric(knn_classif[i])
  risk=length(which(knn-y.test!=0))/length(y.test) 
  result<-list(knn_classif=knn_classif, risk=risk)
  return(result)
}
