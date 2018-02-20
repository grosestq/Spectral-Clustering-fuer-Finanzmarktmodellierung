# 1. Erstellung der Änhlichkeitsmatrix S mit der gaußschen Ähnlichkeitsfunktion s(xi,xj)=exp(-||xi-xj||^2/(2*sig^2))

ähnlichkeit<-function(X,Y,Z,sigma){
  M<-nrow(X)
  N<-ncol(X)
  x<-0
  y<-0
  z<-1
  summ<-0
  S<-matrix(nrow=ncol(X),ncol=ncol(X))
  repeat{
    x=x+1
    y=y+1
    for(j in z){
      for(l in z){
        for(k in y){
          for(m in y){
            for(i in 1:M){
              S[l,m]<-(exp(-(sqrt(summ<-summ+(((X[i,j]-X[i,k])^2)+((Y[i,j]-Y[i,k])^2)+((Z[i,j]-Z[i,k])^2))))/(2*(sigma^2))))
            }
          }
        }
      }
    }
    if(i==M){
      summ=0
    }
    if(y==ncol(X)){
      y=0
      z=z+1
    }  
    if(x==(ncol(X)^2)){
      break
    }
  }
  S
}

S<-ähnlichkeit(X,Y,Z,1)
S

# 2. Erstellung der Adjazenzmatrix W mit dem k-Nächsten-Nachbargraph 

adjazenz<-function(X,k){
  for(i in 1:nrow(X)){
    X[i,i]<-X[i,i]-X[i,i]
  }
  x<-0
  N<-nrow(X)
  if(k>=N){
    W<-S
    }else{
      W<-matrix(0,nrow=N,ncol=N)
      for(i in 1:N){
        x<-sort(X[,i], decreasing=TRUE)[1:k]
        for(l in 1:k){
          for(j in 1:N){
            if(S[j,i]==x[l]){
              W[j,i]<-S[j,i]
              W[i,j]<-S[j,i]
            }
          }
        }
      }
    }
  W
}

W<-adjazenz(S,2)
W

# 3. Erstellung der Gradmatrix D

grad<-function(X){
  N<-nrow(X)
  D<-matrix(0,nrow=N,ncol=N)
  for(i in 1:N){
    D[i,i]<-sum(X[i,])
  }
  D
}

D<-grad(W)
D

# 4. Erstellung der Laplace Matrizen L und Lgen

# 4.1. Erstellung der nicht-normalisierten Laplace Matrix L

L<-D-W

# 4.2. Erstellung der verallgemeinerten Laplace Matrix Lgen

L<-solve(D)%*%L

# 5. Erstellung der Eigenvektoren und Eigenwerte für K-Cluster

eigenv<-function(X,K){
  e<-eigen(X)
  E<-e$vectors[,(ncol(e$vectors)-K+1):ncol(e$vectors)]
  E
}

E<-eigenv(L,10)
E

# 6. Erstellung der Cluster mit K-means

km<-kmeans(E,10,iter.max=10000,nstart=5000)
km

