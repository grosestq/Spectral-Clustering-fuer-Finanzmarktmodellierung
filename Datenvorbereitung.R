# Datenvorbereitung 
#
# Dateneingabe der Kursmartizen in Form von t x n
#
# t-Beobachtungszeitpunkt
# n-Beobachtungsobjekt
#
# OP-Eröffnungskurs
# HL-Höchstkurs
# CL-Schlusskurs
# LP-Tiefstkurs

# 1. Prozentuale Veränderungen

# R-OC(t)=(O(t)-C(t-1))/C(t-1)

ROC<-function(X,Y){
  N<-nrow(X)+1
  M<-ncol(X)
  T<-matrix(nrow=nrow(X), ncol=ncol(X))
  for (i in 2:N) {
    for (j in 1:M) {
      T[i-1,j]<-((X[i-1,j]-Y[i,j])/Y[i,j])
    }
  }
  T
}

ROC<-ROC(OP,CP)
X<-ROC

X<-X[-nrow(X),]

# R-CO(t)=C(t)-O(t)/O(t)

RCO<-function(X,Y){
  N<-nrow(X)
  M<-ncol(X)
  T<-matrix(nrow = nrow(X), ncol=ncol(X))
  for (i in 1:N) {
    for (j in 1:M) {
      T[i,j]<-((X[i,j]-Y[i,j])/Y[i,j])
    }
  }
  T
}

RCO<-RCO(CP,OP)
Y<-RCO

Y<-Y[-nrow(Y),]

# R-HL(t)=H(t)-L(t)/L(t)

RHL<-function(X,Y){
  N<-nrow(X)
  M<-ncol(X)
  T<-matrix(nrow = nrow(X), ncol=ncol(X))
  for (i in 1:N) {
    for (j in 1:M) {
      T[i,j]<-((X[i,j]-Y[i,j])/Y[i,j])
    }
  }
  T
}

RHL<-RHL(HP,LP)
Z<-RHL

Z<-Z[-nrow(Z),]

# 2. Logarithmierte Veränderungen

# S-OC(t)=lnO(t)-lnC(t-1)

SOC<-function(X,Y){
  N<-nrow(X)+1
  M<-ncol(X)
  T<-matrix(nrow=nrow(X), ncol=ncol(X))
  for (i in 2:N) {
    for (j in 1:M) {
      T[i-1,j]<-(log(X[i-1,j]))-(log(Y[i,j]))
    }
  }
  T
}

SOC<-SOC(OP,CP)
X<-SOC

X<-X[-nrow(X),]

# S-CO(t)=lnC(t)-lnO(t)

SCO<-function(X,Y){
  N<-nrow(X)
  M<-ncol(X)
  T<-matrix(nrow = nrow(X), ncol=ncol(X))
  for (i in 1:N) {
    for (j in 1:M) {
      T[i,j]<-(log(X[i,j]))-(log(Y[i,j]))
    }
  }
  T
}

SCO<-SCO(CP,OP)
Y<-SCO

Y<-Y[-nrow(Y),]

# S-HL(t)=lnH(t)-lnL(t)

SHL<-function(X,Y){
  N<-nrow(X)
  M<-ncol(X)
  T<-matrix(nrow = nrow(X), ncol=ncol(X))
  for (i in 1:N) {
    for (j in 1:M) {
      T[i,j]<-(log(X[i,j]))-(log(Y[i,j]))
    }
  }
  T
}

SHL<-SHL(HP,LP)
Z<-SHL

Z<-Z[-nrow(Z),]