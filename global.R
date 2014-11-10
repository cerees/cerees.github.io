library(igraph)
library(sna)
library(network)
library(tnet)
library(statnet)
library(randomNames)
library(shiny)
library(leafletR)
library(gridSVG)
library(d3Network)
library(ggvis)
library(devtools)
library(htmltools)
library(htmlwidgets)
library(networkD3)
library(reshape2)
library(rCharts)
library(dplyr)

data(MisLinks)
data(MisNodes)
sid<-0:249
sid<-as.numeric(sid)
name<-as.character(randomNames(250, name.order="first.last", name.sep=" "))
x<-runif(250, min=-73.879391, max=-73.736657)
y<-runif(250, min=42.630899, max=42.711451)
n     <- 250                   # length of vector
rho   <- 0.6                   # desired correlation = cos(angle)
theta <- acos(rho)             # corresponding angle
gpa.prob    <- rbeta(n, shape1=5, shape2=2)       # fixed given data
sat.prob.org    <- rbeta(n, shape1=7, shape2=2)    # new random data
X     <- cbind(gpa.prob, sat.prob.org)         # matrix
Xctr  <- scale(X, center=TRUE, scale=FALSE)   # centered columns (mean 0)

Id   <- diag(n)                               # identity matrix
Q    <- qr.Q(qr(Xctr[ , 1, drop=FALSE]))      # QR-decomposition, just matrix Q
P    <- tcrossprod(Q)          # = Q Q'       # projection onto space defined by x1
x2o  <- (Id-P) %*% Xctr[ , 2]                 # x2ctr made orthogonal to x1ctr
Xc2  <- cbind(Xctr[ , 1], x2o)                # bind to matrix
Y    <- Xc2 %*% diag(1/sqrt(colSums(Xc2^2)))  # scale columns to length 1

sat.prob <- Y[ , 2] + (1 / tan(theta)) * Y[ , 1]     # final new vector
cor(gpa.prob, sat.prob) 
gpa<-gpa.prob*100
sat.score<-round((sat.prob+abs(min(sat.prob)))*6000, 0)
sat.score[sat.score>2400]<-2400



n     <- 250                   # length of vector
rho   <- 0.85                   # desired correlation = cos(angle)
theta <- acos(rho)             # corresponding angle
attendence.prob.org    <- rbeta(n, shape1=20, shape2=.5)    # new random data
X     <- cbind(sat.prob, attendence.prob.org)         # matrix
Xctr  <- scale(X, center=TRUE, scale=FALSE)   # centered columns (mean 0)

Id   <- diag(n)                               # identity matrix
Q    <- qr.Q(qr(Xctr[ , 1, drop=FALSE]))      # QR-decomposition, just matrix Q
P    <- tcrossprod(Q)          # = Q Q'       # projection onto space defined by x1
x2o  <- (Id-P) %*% Xctr[ , 2]                 # x2ctr made orthogonal to x1ctr
Xc2  <- cbind(Xctr[ , 1], x2o)                # bind to matrix
Y    <- Xc2 %*% diag(1/sqrt(colSums(Xc2^2)))  # scale columns to length 1

attendence.prob <- Y[ , 2] + (1 / tan(theta)) * Y[ , 1]     # final new vector
cor(gpa.prob, attendence.prob)
attendence<-round((attendence.prob+abs(min(attendence.prob)))*150, 0)
attendence[attendence>100]<-100



n     <- 250                   # length of vector
rho   <- -0.55                   # desired correlation = cos(angle)
theta <- acos(rho)             # corresponding angle
vadir.prob.org    <- rnbinom(n, mu = 10, size = 1)    # new random data
X     <- cbind(sat.prob, vadir.prob.org)         # matrix
Xctr  <- scale(X, center=TRUE, scale=FALSE)   # centered columns (mean 0)

Id   <- diag(n)                               # identity matrix
Q    <- qr.Q(qr(Xctr[ , 1, drop=FALSE]))      # QR-decomposition, just matrix Q
P    <- tcrossprod(Q)          # = Q Q'       # projection onto space defined by x1
x2o  <- (Id-P) %*% Xctr[ , 2]                 # x2ctr made orthogonal to x1ctr
Xc2  <- cbind(Xctr[ , 1], x2o)                # bind to matrix
Y    <- Xc2 %*% diag(1/sqrt(colSums(Xc2^2)))  # scale columns to length 1

vadir.prob <- Y[ , 2] + (1 / tan(theta)) * Y[ , 1]     # final new vector
cor(sat.prob, vadir.prob)
vadirs<-round((vadir.prob+abs(min(vadir.prob)))*25, 0)

cor(vadirs, attendence)
ethnicities <- c("African American", "Hispanic", "Asian", "White", "Native American")
genders <- c("Female", "Male")
lunch.statuses<-c("Free", "Reduced", "Paid")
guardianship.statuses<-c("Single Parent", "Dual Parent")
psat.statuses<-c("PSAT Taken", "No-PSAT")
sat.statuses<-c("SAT Taken", "No-SAT")
tzip.statuses<-c("Inside Target Zip", "Outside Target Zip")
test.df <- data.frame(sex=sample(genders, 250, replace=TRUE),
                      race=sample(ethnicities, 250, replace=TRUE, prob=c(.25, .08, .06, .60, .1)),
                      lunch=sample(lunch.statuses, 250, replace=T, prob=c(.50, .25, .25)),
                      guardian=sample(guardianship.statuses, 250, replace=T, prob=c(.575, .425)),
                      psat.dummy=sample(psat.statuses, 250, replace=T, prob=c(.40, .6)),
                      sat.dummy=sample(sat.statuses, 250, replace=T, prob=c(.5, .5)),
                      target.zip=sample(tzip.statuses, 250, replace=T, prob=c(.5, .5)))
df<-cbind(name, sid, vadirs, gpa, sat.score, attendence, x, y, test.df)
df$year<-2014
df$sat.dummy[df$psat.dummy == "PSAT Taken"] <- "SAT Taken"
df$sat.score[df$sat.dummy == "No-SAT"] <- 0
df2014<-df
n     <- 250                   # length of vector
rho   <- 0.6                   # desired correlation = cos(angle)
theta <- acos(rho)             # corresponding angle
gpa.prob    <- rbeta(n, shape1=5, shape2=2)       # fixed given data
sat.prob.org    <- rbeta(n, shape1=7, shape2=2)    # new random data
X     <- cbind(gpa.prob, sat.prob.org)         # matrix
Xctr  <- scale(X, center=TRUE, scale=FALSE)   # centered columns (mean 0)

Id   <- diag(n)                               # identity matrix
Q    <- qr.Q(qr(Xctr[ , 1, drop=FALSE]))      # QR-decomposition, just matrix Q
P    <- tcrossprod(Q)          # = Q Q'       # projection onto space defined by x1
x2o  <- (Id-P) %*% Xctr[ , 2]                 # x2ctr made orthogonal to x1ctr
Xc2  <- cbind(Xctr[ , 1], x2o)                # bind to matrix
Y    <- Xc2 %*% diag(1/sqrt(colSums(Xc2^2)))  # scale columns to length 1

sat.prob <- Y[ , 2] + (1 / tan(theta)) * Y[ , 1]     # final new vector
cor(gpa.prob, sat.prob) 
gpa<-gpa.prob*100
sat.score<-round((sat.prob+abs(min(sat.prob)))*6000, 0)
sat.score[sat.score>2400]<-2400

n     <- 250                   # length of vector
rho   <- 0.85                   # desired correlation = cos(angle)
theta <- acos(rho)             # corresponding angle
attendence.prob.org    <- rbeta(n, shape1=20, shape2=.5)    # new random data
X     <- cbind(sat.prob, attendence.prob.org)         # matrix
Xctr  <- scale(X, center=TRUE, scale=FALSE)   # centered columns (mean 0)

Id   <- diag(n)                               # identity matrix
Q    <- qr.Q(qr(Xctr[ , 1, drop=FALSE]))      # QR-decomposition, just matrix Q
P    <- tcrossprod(Q)          # = Q Q'       # projection onto space defined by x1
x2o  <- (Id-P) %*% Xctr[ , 2]                 # x2ctr made orthogonal to x1ctr
Xc2  <- cbind(Xctr[ , 1], x2o)                # bind to matrix
Y    <- Xc2 %*% diag(1/sqrt(colSums(Xc2^2)))  # scale columns to length 1

attendence.prob <- Y[ , 2] + (1 / tan(theta)) * Y[ , 1]     # final new vector
cor(gpa.prob, attendence.prob)
attendence<-round((attendence.prob+abs(min(attendence.prob)))*150, 0)
attendence[attendence>100]<-100



n     <- 250                   # length of vector
rho   <- -0.55                   # desired correlation = cos(angle)
theta <- acos(rho)             # corresponding angle
vadir.prob.org    <- rnbinom(n, mu = 10, size = 1)    # new random data
X     <- cbind(sat.prob, vadir.prob.org)         # matrix
Xctr  <- scale(X, center=TRUE, scale=FALSE)   # centered columns (mean 0)

Id   <- diag(n)                               # identity matrix
Q    <- qr.Q(qr(Xctr[ , 1, drop=FALSE]))      # QR-decomposition, just matrix Q
P    <- tcrossprod(Q)          # = Q Q'       # projection onto space defined by x1
x2o  <- (Id-P) %*% Xctr[ , 2]                 # x2ctr made orthogonal to x1ctr
Xc2  <- cbind(Xctr[ , 1], x2o)                # bind to matrix
Y    <- Xc2 %*% diag(1/sqrt(colSums(Xc2^2)))  # scale columns to length 1

vadir.prob <- Y[ , 2] + (1 / tan(theta)) * Y[ , 1]     # final new vector
cor(sat.prob, vadir.prob)
vadirs<-round((vadir.prob+abs(min(vadir.prob)))*25, 0)

cor(vadirs, attendence)
df<-cbind(name, sid, vadirs, gpa, sat.score, attendence, x, y, test.df)
df$sat.dummy[df$psat.dummy == "PSAT Taken"] <- "SAT Taken"
df$sat.score[df$sat.dummy == "No-SAT"] <- 0
df$year<-2013
df2013<-df
n     <- 250                   # length of vector
rho   <- 0.6                   # desired correlation = cos(angle)
theta <- acos(rho)             # corresponding angle
gpa.prob    <- rbeta(n, shape1=5, shape2=2)       # fixed given data
sat.prob.org    <- rbeta(n, shape1=7, shape2=2)    # new random data
X     <- cbind(gpa.prob, sat.prob.org)         # matrix
Xctr  <- scale(X, center=TRUE, scale=FALSE)   # centered columns (mean 0)

Id   <- diag(n)                               # identity matrix
Q    <- qr.Q(qr(Xctr[ , 1, drop=FALSE]))      # QR-decomposition, just matrix Q
P    <- tcrossprod(Q)          # = Q Q'       # projection onto space defined by x1
x2o  <- (Id-P) %*% Xctr[ , 2]                 # x2ctr made orthogonal to x1ctr
Xc2  <- cbind(Xctr[ , 1], x2o)                # bind to matrix
Y    <- Xc2 %*% diag(1/sqrt(colSums(Xc2^2)))  # scale columns to length 1

sat.prob <- Y[ , 2] + (1 / tan(theta)) * Y[ , 1]     # final new vector
cor(gpa.prob, sat.prob) 
gpa<-gpa.prob*100
sat.score<-round((sat.prob+abs(min(sat.prob)))*6000, 0)
sat.score[sat.score>2400]<-2400

n     <- 250                   # length of vector
rho   <- 0.85                   # desired correlation = cos(angle)
theta <- acos(rho)             # corresponding angle
attendence.prob.org    <- rbeta(n, shape1=20, shape2=.5)    # new random data
X     <- cbind(sat.prob, attendence.prob.org)         # matrix
Xctr  <- scale(X, center=TRUE, scale=FALSE)   # centered columns (mean 0)

Id   <- diag(n)                               # identity matrix
Q    <- qr.Q(qr(Xctr[ , 1, drop=FALSE]))      # QR-decomposition, just matrix Q
P    <- tcrossprod(Q)          # = Q Q'       # projection onto space defined by x1
x2o  <- (Id-P) %*% Xctr[ , 2]                 # x2ctr made orthogonal to x1ctr
Xc2  <- cbind(Xctr[ , 1], x2o)                # bind to matrix
Y    <- Xc2 %*% diag(1/sqrt(colSums(Xc2^2)))  # scale columns to length 1

attendence.prob <- Y[ , 2] + (1 / tan(theta)) * Y[ , 1]     # final new vector
cor(gpa.prob, attendence.prob)
attendence<-round((attendence.prob+abs(min(attendence.prob)))*150, 0)
attendence[attendence>100]<-100



n     <- 250                   # length of vector
rho   <- -0.55                   # desired correlation = cos(angle)
theta <- acos(rho)             # corresponding angle
vadir.prob.org    <- rnbinom(n, mu = 10, size = 1)    # new random data
X     <- cbind(sat.prob, vadir.prob.org)         # matrix
Xctr  <- scale(X, center=TRUE, scale=FALSE)   # centered columns (mean 0)

Id   <- diag(n)                               # identity matrix
Q    <- qr.Q(qr(Xctr[ , 1, drop=FALSE]))      # QR-decomposition, just matrix Q
P    <- tcrossprod(Q)          # = Q Q'       # projection onto space defined by x1
x2o  <- (Id-P) %*% Xctr[ , 2]                 # x2ctr made orthogonal to x1ctr
Xc2  <- cbind(Xctr[ , 1], x2o)                # bind to matrix
Y    <- Xc2 %*% diag(1/sqrt(colSums(Xc2^2)))  # scale columns to length 1

vadir.prob <- Y[ , 2] + (1 / tan(theta)) * Y[ , 1]     # final new vector
cor(sat.prob, vadir.prob)
vadirs<-round((vadir.prob+abs(min(vadir.prob)))*25, 0)

cor(vadirs, attendence)
df<-cbind(name, sid, vadirs, gpa, sat.score, attendence, x, y, test.df)
df$sat.dummy[df$psat.dummy == "PSAT Taken"] <- "SAT Taken"
df$sat.score[df$sat.dummy == "No-SAT"] <- 0
df$year<-2012
df2012<-df
n     <- 250                   # length of vector
rho   <- 0.6                   # desired correlation = cos(angle)
theta <- acos(rho)             # corresponding angle
gpa.prob    <- rbeta(n, shape1=5, shape2=2)       # fixed given data
sat.prob.org    <- rbeta(n, shape1=7, shape2=2)    # new random data
X     <- cbind(gpa.prob, sat.prob.org)         # matrix
Xctr  <- scale(X, center=TRUE, scale=FALSE)   # centered columns (mean 0)

Id   <- diag(n)                               # identity matrix
Q    <- qr.Q(qr(Xctr[ , 1, drop=FALSE]))      # QR-decomposition, just matrix Q
P    <- tcrossprod(Q)          # = Q Q'       # projection onto space defined by x1
x2o  <- (Id-P) %*% Xctr[ , 2]                 # x2ctr made orthogonal to x1ctr
Xc2  <- cbind(Xctr[ , 1], x2o)                # bind to matrix
Y    <- Xc2 %*% diag(1/sqrt(colSums(Xc2^2)))  # scale columns to length 1

sat.prob <- Y[ , 2] + (1 / tan(theta)) * Y[ , 1]     # final new vector
cor(gpa.prob, sat.prob) 
gpa<-gpa.prob*100
sat.score<-round((sat.prob+abs(min(sat.prob)))*6000, 0)
sat.score[sat.score>2400]<-2400

n     <- 250                   # length of vector
rho   <- 0.85                   # desired correlation = cos(angle)
theta <- acos(rho)             # corresponding angle
attendence.prob.org    <- rbeta(n, shape1=20, shape2=.5)    # new random data
X     <- cbind(sat.prob, attendence.prob.org)         # matrix
Xctr  <- scale(X, center=TRUE, scale=FALSE)   # centered columns (mean 0)

Id   <- diag(n)                               # identity matrix
Q    <- qr.Q(qr(Xctr[ , 1, drop=FALSE]))      # QR-decomposition, just matrix Q
P    <- tcrossprod(Q)          # = Q Q'       # projection onto space defined by x1
x2o  <- (Id-P) %*% Xctr[ , 2]                 # x2ctr made orthogonal to x1ctr
Xc2  <- cbind(Xctr[ , 1], x2o)                # bind to matrix
Y    <- Xc2 %*% diag(1/sqrt(colSums(Xc2^2)))  # scale columns to length 1

attendence.prob <- Y[ , 2] + (1 / tan(theta)) * Y[ , 1]     # final new vector
cor(gpa.prob, attendence.prob)
attendence<-round((attendence.prob+abs(min(attendence.prob)))*150, 0)
attendence[attendence>100]<-100



n     <- 250                   # length of vector
rho   <- -0.55                   # desired correlation = cos(angle)
theta <- acos(rho)             # corresponding angle
vadir.prob.org    <- rnbinom(n, mu = 10, size = 1)    # new random data
X     <- cbind(sat.prob, vadir.prob.org)         # matrix
Xctr  <- scale(X, center=TRUE, scale=FALSE)   # centered columns (mean 0)

Id   <- diag(n)                               # identity matrix
Q    <- qr.Q(qr(Xctr[ , 1, drop=FALSE]))      # QR-decomposition, just matrix Q
P    <- tcrossprod(Q)          # = Q Q'       # projection onto space defined by x1
x2o  <- (Id-P) %*% Xctr[ , 2]                 # x2ctr made orthogonal to x1ctr
Xc2  <- cbind(Xctr[ , 1], x2o)                # bind to matrix
Y    <- Xc2 %*% diag(1/sqrt(colSums(Xc2^2)))  # scale columns to length 1

vadir.prob <- Y[ , 2] + (1 / tan(theta)) * Y[ , 1]     # final new vector
cor(sat.prob, vadir.prob)
vadirs<-round((vadir.prob+abs(min(vadir.prob)))*25, 0)

cor(vadirs, attendence)
df<-cbind(name, sid, vadirs, gpa, sat.score, attendence, x, y, test.df)
df$sat.dummy[df$psat.dummy == "PSAT Taken"] <- "SAT Taken"
df$sat.score[df$sat.dummy == "No-SAT"] <- 0
df$year<-2011
df2011<-df
n     <- 250                   # length of vector
rho   <- 0.6                   # desired correlation = cos(angle)
theta <- acos(rho)             # corresponding angle
gpa.prob    <- rbeta(n, shape1=5, shape2=2)       # fixed given data
sat.prob.org    <- rbeta(n, shape1=7, shape2=2)    # new random data
X     <- cbind(gpa.prob, sat.prob.org)         # matrix
Xctr  <- scale(X, center=TRUE, scale=FALSE)   # centered columns (mean 0)

Id   <- diag(n)                               # identity matrix
Q    <- qr.Q(qr(Xctr[ , 1, drop=FALSE]))      # QR-decomposition, just matrix Q
P    <- tcrossprod(Q)          # = Q Q'       # projection onto space defined by x1
x2o  <- (Id-P) %*% Xctr[ , 2]                 # x2ctr made orthogonal to x1ctr
Xc2  <- cbind(Xctr[ , 1], x2o)                # bind to matrix
Y    <- Xc2 %*% diag(1/sqrt(colSums(Xc2^2)))  # scale columns to length 1

sat.prob <- Y[ , 2] + (1 / tan(theta)) * Y[ , 1]     # final new vector
cor(gpa.prob, sat.prob) 
gpa<-gpa.prob*100
sat.score<-round((sat.prob+abs(min(sat.prob)))*6000, 0)
sat.score[sat.score>2400]<-2400

n     <- 250                   # length of vector
rho   <- 0.85                   # desired correlation = cos(angle)
theta <- acos(rho)             # corresponding angle
attendence.prob.org    <- rbeta(n, shape1=20, shape2=.5)    # new random data
X     <- cbind(sat.prob, attendence.prob.org)         # matrix
Xctr  <- scale(X, center=TRUE, scale=FALSE)   # centered columns (mean 0)

Id   <- diag(n)                               # identity matrix
Q    <- qr.Q(qr(Xctr[ , 1, drop=FALSE]))      # QR-decomposition, just matrix Q
P    <- tcrossprod(Q)          # = Q Q'       # projection onto space defined by x1
x2o  <- (Id-P) %*% Xctr[ , 2]                 # x2ctr made orthogonal to x1ctr
Xc2  <- cbind(Xctr[ , 1], x2o)                # bind to matrix
Y    <- Xc2 %*% diag(1/sqrt(colSums(Xc2^2)))  # scale columns to length 1

attendence.prob <- Y[ , 2] + (1 / tan(theta)) * Y[ , 1]     # final new vector
cor(gpa.prob, attendence.prob)
attendence<-round((attendence.prob+abs(min(attendence.prob)))*150, 0)
attendence[attendence>100]<-100



n     <- 250                   # length of vector
rho   <- -0.55                   # desired correlation = cos(angle)
theta <- acos(rho)             # corresponding angle
vadir.prob.org    <- rnbinom(n, mu = 10, size = 1)    # new random data
X     <- cbind(sat.prob, vadir.prob.org)         # matrix
Xctr  <- scale(X, center=TRUE, scale=FALSE)   # centered columns (mean 0)

Id   <- diag(n)                               # identity matrix
Q    <- qr.Q(qr(Xctr[ , 1, drop=FALSE]))      # QR-decomposition, just matrix Q
P    <- tcrossprod(Q)          # = Q Q'       # projection onto space defined by x1
x2o  <- (Id-P) %*% Xctr[ , 2]                 # x2ctr made orthogonal to x1ctr
Xc2  <- cbind(Xctr[ , 1], x2o)                # bind to matrix
Y    <- Xc2 %*% diag(1/sqrt(colSums(Xc2^2)))  # scale columns to length 1

vadir.prob <- Y[ , 2] + (1 / tan(theta)) * Y[ , 1]     # final new vector
cor(sat.prob, vadir.prob)
vadirs<-round((vadir.prob+abs(min(vadir.prob)))*25, 0)

cor(vadirs, attendence)
df<-cbind(name, sid, vadirs, gpa, sat.score, attendence, x, y, test.df)
df$sat.dummy[df$psat.dummy == "PSAT Taken"] <- "SAT Taken"
df$sat.score[df$sat.dummy == "No-SAT"] <- 0
df$year<-2010
df2010<-df
n     <- 250                   # length of vector
rho   <- 0.6                   # desired correlation = cos(angle)
theta <- acos(rho)             # corresponding angle
gpa.prob    <- rbeta(n, shape1=5, shape2=2)       # fixed given data
sat.prob.org    <- rbeta(n, shape1=7, shape2=2)    # new random data
X     <- cbind(gpa.prob, sat.prob.org)         # matrix
Xctr  <- scale(X, center=TRUE, scale=FALSE)   # centered columns (mean 0)

Id   <- diag(n)                               # identity matrix
Q    <- qr.Q(qr(Xctr[ , 1, drop=FALSE]))      # QR-decomposition, just matrix Q
P    <- tcrossprod(Q)          # = Q Q'       # projection onto space defined by x1
x2o  <- (Id-P) %*% Xctr[ , 2]                 # x2ctr made orthogonal to x1ctr
Xc2  <- cbind(Xctr[ , 1], x2o)                # bind to matrix
Y    <- Xc2 %*% diag(1/sqrt(colSums(Xc2^2)))  # scale columns to length 1

sat.prob <- Y[ , 2] + (1 / tan(theta)) * Y[ , 1]     # final new vector
cor(gpa.prob, sat.prob) 
gpa<-gpa.prob*100
sat.score<-round((sat.prob+abs(min(sat.prob)))*6000, 0)
sat.score[sat.score>2400]<-2400

n     <- 250                   # length of vector
rho   <- 0.85                   # desired correlation = cos(angle)
theta <- acos(rho)             # corresponding angle
attendence.prob.org    <- rbeta(n, shape1=20, shape2=.5)    # new random data
X     <- cbind(sat.prob, attendence.prob.org)         # matrix
Xctr  <- scale(X, center=TRUE, scale=FALSE)   # centered columns (mean 0)

Id   <- diag(n)                               # identity matrix
Q    <- qr.Q(qr(Xctr[ , 1, drop=FALSE]))      # QR-decomposition, just matrix Q
P    <- tcrossprod(Q)          # = Q Q'       # projection onto space defined by x1
x2o  <- (Id-P) %*% Xctr[ , 2]                 # x2ctr made orthogonal to x1ctr
Xc2  <- cbind(Xctr[ , 1], x2o)                # bind to matrix
Y    <- Xc2 %*% diag(1/sqrt(colSums(Xc2^2)))  # scale columns to length 1

attendence.prob <- Y[ , 2] + (1 / tan(theta)) * Y[ , 1]     # final new vector
cor(gpa.prob, attendence.prob)
attendence<-round((attendence.prob+abs(min(attendence.prob)))*150, 0)
attendence[attendence>100]<-100



n     <- 250                   # length of vector
rho   <- -0.55                   # desired correlation = cos(angle)
theta <- acos(rho)             # corresponding angle
vadir.prob.org    <- rnbinom(n, mu = 10, size = 1)    # new random data
X     <- cbind(sat.prob, vadir.prob.org)         # matrix
Xctr  <- scale(X, center=TRUE, scale=FALSE)   # centered columns (mean 0)

Id   <- diag(n)                               # identity matrix
Q    <- qr.Q(qr(Xctr[ , 1, drop=FALSE]))      # QR-decomposition, just matrix Q
P    <- tcrossprod(Q)          # = Q Q'       # projection onto space defined by x1
x2o  <- (Id-P) %*% Xctr[ , 2]                 # x2ctr made orthogonal to x1ctr
Xc2  <- cbind(Xctr[ , 1], x2o)                # bind to matrix
Y    <- Xc2 %*% diag(1/sqrt(colSums(Xc2^2)))  # scale columns to length 1

vadir.prob <- Y[ , 2] + (1 / tan(theta)) * Y[ , 1]     # final new vector
cor(sat.prob, vadir.prob)
vadirs<-round((vadir.prob+abs(min(vadir.prob)))*25, 0)

cor(vadirs, attendence)
df<-cbind(name, sid, vadirs, gpa, sat.score, attendence, x, y, test.df)
df$sat.dummy[df$psat.dummy == "PSAT Taken"] <- "SAT Taken"
df$sat.score[df$sat.dummy == "No-SAT"] <- 0
df$year<-2009
df2009<-df
n     <- 250                   # length of vector
rho   <- 0.6                   # desired correlation = cos(angle)
theta <- acos(rho)             # corresponding angle
gpa.prob    <- rbeta(n, shape1=5, shape2=2)       # fixed given data
sat.prob.org    <- rbeta(n, shape1=7, shape2=2)    # new random data
X     <- cbind(gpa.prob, sat.prob.org)         # matrix
Xctr  <- scale(X, center=TRUE, scale=FALSE)   # centered columns (mean 0)

Id   <- diag(n)                               # identity matrix
Q    <- qr.Q(qr(Xctr[ , 1, drop=FALSE]))      # QR-decomposition, just matrix Q
P    <- tcrossprod(Q)          # = Q Q'       # projection onto space defined by x1
x2o  <- (Id-P) %*% Xctr[ , 2]                 # x2ctr made orthogonal to x1ctr
Xc2  <- cbind(Xctr[ , 1], x2o)                # bind to matrix
Y    <- Xc2 %*% diag(1/sqrt(colSums(Xc2^2)))  # scale columns to length 1

sat.prob <- Y[ , 2] + (1 / tan(theta)) * Y[ , 1]     # final new vector
cor(gpa.prob, sat.prob) 
gpa<-gpa.prob*100
sat.score<-round((sat.prob+abs(min(sat.prob)))*6000, 0)
sat.score[sat.score>2400]<-2400

n     <- 250                   # length of vector
rho   <- 0.85                   # desired correlation = cos(angle)
theta <- acos(rho)             # corresponding angle
attendence.prob.org    <- rbeta(n, shape1=20, shape2=.5)    # new random data
X     <- cbind(sat.prob, attendence.prob.org)         # matrix
Xctr  <- scale(X, center=TRUE, scale=FALSE)   # centered columns (mean 0)

Id   <- diag(n)                               # identity matrix
Q    <- qr.Q(qr(Xctr[ , 1, drop=FALSE]))      # QR-decomposition, just matrix Q
P    <- tcrossprod(Q)          # = Q Q'       # projection onto space defined by x1
x2o  <- (Id-P) %*% Xctr[ , 2]                 # x2ctr made orthogonal to x1ctr
Xc2  <- cbind(Xctr[ , 1], x2o)                # bind to matrix
Y    <- Xc2 %*% diag(1/sqrt(colSums(Xc2^2)))  # scale columns to length 1

attendence.prob <- Y[ , 2] + (1 / tan(theta)) * Y[ , 1]     # final new vector
cor(gpa.prob, attendence.prob)
attendence<-round((attendence.prob+abs(min(attendence.prob)))*150, 0)
attendence[attendence>100]<-100


n     <- 250                   # length of vector
rho   <- -0.55                   # desired correlation = cos(angle)
theta <- acos(rho)             # corresponding angle
vadir.prob.org    <- rnbinom(n, mu = 10, size = 1)    # new random data
X     <- cbind(sat.prob, vadir.prob.org)         # matrix
Xctr  <- scale(X, center=TRUE, scale=FALSE)   # centered columns (mean 0)

Id   <- diag(n)                               # identity matrix
Q    <- qr.Q(qr(Xctr[ , 1, drop=FALSE]))      # QR-decomposition, just matrix Q
P    <- tcrossprod(Q)          # = Q Q'       # projection onto space defined by x1
x2o  <- (Id-P) %*% Xctr[ , 2]                 # x2ctr made orthogonal to x1ctr
Xc2  <- cbind(Xctr[ , 1], x2o)                # bind to matrix
Y    <- Xc2 %*% diag(1/sqrt(colSums(Xc2^2)))  # scale columns to length 1

vadir.prob <- Y[ , 2] + (1 / tan(theta)) * Y[ , 1]     # final new vector
cor(sat.prob, vadir.prob)
vadirs<-round((vadir.prob+abs(min(vadir.prob)))*25, 0)

cor(vadirs, attendence)
df<-cbind(name, sid, vadirs, gpa, sat.score, attendence, x, y, test.df)
df$sat.dummy[df$psat.dummy == "PSAT Taken"] <- "SAT Taken"
df$sat.score[df$sat.dummy == "No-SAT"] <- 0
df$year<-2008
df2008<-df
n     <- 250                   # length of vector
rho   <- 0.6                   # desired correlation = cos(angle)
theta <- acos(rho)             # corresponding angle
gpa.prob    <- rbeta(n, shape1=5, shape2=2)       # fixed given data
sat.prob.org    <- rbeta(n, shape1=7, shape2=2)    # new random data
X     <- cbind(gpa.prob, sat.prob.org)         # matrix
Xctr  <- scale(X, center=TRUE, scale=FALSE)   # centered columns (mean 0)

Id   <- diag(n)                               # identity matrix
Q    <- qr.Q(qr(Xctr[ , 1, drop=FALSE]))      # QR-decomposition, just matrix Q
P    <- tcrossprod(Q)          # = Q Q'       # projection onto space defined by x1
x2o  <- (Id-P) %*% Xctr[ , 2]                 # x2ctr made orthogonal to x1ctr
Xc2  <- cbind(Xctr[ , 1], x2o)                # bind to matrix
Y    <- Xc2 %*% diag(1/sqrt(colSums(Xc2^2)))  # scale columns to length 1

sat.prob <- Y[ , 2] + (1 / tan(theta)) * Y[ , 1]     # final new vector
cor(gpa.prob, sat.prob) 
gpa<-gpa.prob*100
sat.score<-round((sat.prob+abs(min(sat.prob)))*6000, 0)
sat.score[sat.score>2400]<-2400

n     <- 250                   # length of vector
rho   <- 0.85                   # desired correlation = cos(angle)
theta <- acos(rho)             # corresponding angle
attendence.prob.org    <- rbeta(n, shape1=20, shape2=.5)    # new random data
X     <- cbind(sat.prob, attendence.prob.org)         # matrix
Xctr  <- scale(X, center=TRUE, scale=FALSE)   # centered columns (mean 0)

Id   <- diag(n)                               # identity matrix
Q    <- qr.Q(qr(Xctr[ , 1, drop=FALSE]))      # QR-decomposition, just matrix Q
P    <- tcrossprod(Q)          # = Q Q'       # projection onto space defined by x1
x2o  <- (Id-P) %*% Xctr[ , 2]                 # x2ctr made orthogonal to x1ctr
Xc2  <- cbind(Xctr[ , 1], x2o)                # bind to matrix
Y    <- Xc2 %*% diag(1/sqrt(colSums(Xc2^2)))  # scale columns to length 1

attendence.prob <- Y[ , 2] + (1 / tan(theta)) * Y[ , 1]     # final new vector
cor(gpa.prob, attendence.prob)
attendence<-round((attendence.prob+abs(min(attendence.prob)))*150, 0)
attendence[attendence>100]<-100



n     <- 250                   # length of vector
rho   <- -0.55                   # desired correlation = cos(angle)
theta <- acos(rho)             # corresponding angle
vadir.prob.org    <- rnbinom(n, mu = 10, size = 1)    # new random data
X     <- cbind(sat.prob, vadir.prob.org)         # matrix
Xctr  <- scale(X, center=TRUE, scale=FALSE)   # centered columns (mean 0)

Id   <- diag(n)                               # identity matrix
Q    <- qr.Q(qr(Xctr[ , 1, drop=FALSE]))      # QR-decomposition, just matrix Q
P    <- tcrossprod(Q)          # = Q Q'       # projection onto space defined by x1
x2o  <- (Id-P) %*% Xctr[ , 2]                 # x2ctr made orthogonal to x1ctr
Xc2  <- cbind(Xctr[ , 1], x2o)                # bind to matrix
Y    <- Xc2 %*% diag(1/sqrt(colSums(Xc2^2)))  # scale columns to length 1

vadir.prob <- Y[ , 2] + (1 / tan(theta)) * Y[ , 1]     # final new vector
cor(sat.prob, vadir.prob)
vadirs<-round((vadir.prob+abs(min(vadir.prob)))*25, 0)

cor(vadirs, attendence)
df<-cbind(name, sid, vadirs, gpa, sat.score, attendence, x, y, test.df)
df$sat.dummy[df$psat.dummy == "PSAT Taken"] <- "SAT Taken"
df$sat.score[df$sat.dummy == "No-SAT"] <- 0
df$year<-2007
df2007<-df
n     <- 250                   # length of vector
rho   <- 0.6                   # desired correlation = cos(angle)
theta <- acos(rho)             # corresponding angle
gpa.prob    <- rbeta(n, shape1=5, shape2=2)       # fixed given data
sat.prob.org    <- rbeta(n, shape1=7, shape2=2)    # new random data
X     <- cbind(gpa.prob, sat.prob.org)         # matrix
Xctr  <- scale(X, center=TRUE, scale=FALSE)   # centered columns (mean 0)

Id   <- diag(n)                               # identity matrix
Q    <- qr.Q(qr(Xctr[ , 1, drop=FALSE]))      # QR-decomposition, just matrix Q
P    <- tcrossprod(Q)          # = Q Q'       # projection onto space defined by x1
x2o  <- (Id-P) %*% Xctr[ , 2]                 # x2ctr made orthogonal to x1ctr
Xc2  <- cbind(Xctr[ , 1], x2o)                # bind to matrix
Y    <- Xc2 %*% diag(1/sqrt(colSums(Xc2^2)))  # scale columns to length 1

sat.prob <- Y[ , 2] + (1 / tan(theta)) * Y[ , 1]     # final new vector
cor(gpa.prob, sat.prob) 
gpa<-gpa.prob*100
sat.score<-round((sat.prob+abs(min(sat.prob)))*6000, 0)
sat.score[sat.score>2400]<-2400

n     <- 250                   # length of vector
rho   <- 0.85                   # desired correlation = cos(angle)
theta <- acos(rho)             # corresponding angle
attendence.prob.org    <- rbeta(n, shape1=20, shape2=.5)    # new random data
X     <- cbind(sat.prob, attendence.prob.org)         # matrix
Xctr  <- scale(X, center=TRUE, scale=FALSE)   # centered columns (mean 0)

Id   <- diag(n)                               # identity matrix
Q    <- qr.Q(qr(Xctr[ , 1, drop=FALSE]))      # QR-decomposition, just matrix Q
P    <- tcrossprod(Q)          # = Q Q'       # projection onto space defined by x1
x2o  <- (Id-P) %*% Xctr[ , 2]                 # x2ctr made orthogonal to x1ctr
Xc2  <- cbind(Xctr[ , 1], x2o)                # bind to matrix
Y    <- Xc2 %*% diag(1/sqrt(colSums(Xc2^2)))  # scale columns to length 1

attendence.prob <- Y[ , 2] + (1 / tan(theta)) * Y[ , 1]     # final new vector
cor(gpa.prob, attendence.prob)
attendence<-round((attendence.prob+abs(min(attendence.prob)))*150, 0)
attendence[attendence>100]<-100



n     <- 250                   # length of vector
rho   <- -0.55                   # desired correlation = cos(angle)
theta <- acos(rho)             # corresponding angle
vadir.prob.org    <- rnbinom(n, mu = 10, size = 1)    # new random data
X     <- cbind(sat.prob, vadir.prob.org)         # matrix
Xctr  <- scale(X, center=TRUE, scale=FALSE)   # centered columns (mean 0)

Id   <- diag(n)                               # identity matrix
Q    <- qr.Q(qr(Xctr[ , 1, drop=FALSE]))      # QR-decomposition, just matrix Q
P    <- tcrossprod(Q)          # = Q Q'       # projection onto space defined by x1
x2o  <- (Id-P) %*% Xctr[ , 2]                 # x2ctr made orthogonal to x1ctr
Xc2  <- cbind(Xctr[ , 1], x2o)                # bind to matrix
Y    <- Xc2 %*% diag(1/sqrt(colSums(Xc2^2)))  # scale columns to length 1

vadir.prob <- Y[ , 2] + (1 / tan(theta)) * Y[ , 1]     # final new vector
cor(sat.prob, vadir.prob)
vadirs<-round((vadir.prob+abs(min(vadir.prob)))*25, 0)

cor(vadirs, attendence)
df<-cbind(name, sid, vadirs, gpa, sat.score, attendence, x, y, test.df)
df$sat.dummy[df$psat.dummy == "PSAT Taken"] <- "SAT Taken"
df$sat.score[df$sat.dummy == "No-SAT"] <- 0
df$year<-2006
df2006<-df
n     <- 250                   # length of vector
rho   <- 0.6                   # desired correlation = cos(angle)
theta <- acos(rho)             # corresponding angle
gpa.prob    <- rbeta(n, shape1=5, shape2=2)       # fixed given data
sat.prob.org    <- rbeta(n, shape1=7, shape2=2)    # new random data
X     <- cbind(gpa.prob, sat.prob.org)         # matrix
Xctr  <- scale(X, center=TRUE, scale=FALSE)   # centered columns (mean 0)

Id   <- diag(n)                               # identity matrix
Q    <- qr.Q(qr(Xctr[ , 1, drop=FALSE]))      # QR-decomposition, just matrix Q
P    <- tcrossprod(Q)          # = Q Q'       # projection onto space defined by x1
x2o  <- (Id-P) %*% Xctr[ , 2]                 # x2ctr made orthogonal to x1ctr
Xc2  <- cbind(Xctr[ , 1], x2o)                # bind to matrix
Y    <- Xc2 %*% diag(1/sqrt(colSums(Xc2^2)))  # scale columns to length 1

sat.prob <- Y[ , 2] + (1 / tan(theta)) * Y[ , 1]     # final new vector
cor(gpa.prob, sat.prob) 
gpa<-gpa.prob*100
sat.score<-round((sat.prob+abs(min(sat.prob)))*6000, 0)
sat.score[sat.score>2400]<-2400

n     <- 250                   # length of vector
rho   <- 0.85                   # desired correlation = cos(angle)
theta <- acos(rho)             # corresponding angle
attendence.prob.org    <- rbeta(n, shape1=20, shape2=.5)    # new random data
X     <- cbind(sat.prob, attendence.prob.org)         # matrix
Xctr  <- scale(X, center=TRUE, scale=FALSE)   # centered columns (mean 0)

Id   <- diag(n)                               # identity matrix
Q    <- qr.Q(qr(Xctr[ , 1, drop=FALSE]))      # QR-decomposition, just matrix Q
P    <- tcrossprod(Q)          # = Q Q'       # projection onto space defined by x1
x2o  <- (Id-P) %*% Xctr[ , 2]                 # x2ctr made orthogonal to x1ctr
Xc2  <- cbind(Xctr[ , 1], x2o)                # bind to matrix
Y    <- Xc2 %*% diag(1/sqrt(colSums(Xc2^2)))  # scale columns to length 1

attendence.prob <- Y[ , 2] + (1 / tan(theta)) * Y[ , 1]     # final new vector
cor(gpa.prob, attendence.prob)
attendence<-round((attendence.prob+abs(min(attendence.prob)))*150, 0)
attendence[attendence>100]<-100



n     <- 250                   # length of vector
rho   <- -0.55                   # desired correlation = cos(angle)
theta <- acos(rho)             # corresponding angle
vadir.prob.org    <- rnbinom(n, mu = 10, size = 1)    # new random data
X     <- cbind(sat.prob, vadir.prob.org)         # matrix
Xctr  <- scale(X, center=TRUE, scale=FALSE)   # centered columns (mean 0)

Id   <- diag(n)                               # identity matrix
Q    <- qr.Q(qr(Xctr[ , 1, drop=FALSE]))      # QR-decomposition, just matrix Q
P    <- tcrossprod(Q)          # = Q Q'       # projection onto space defined by x1
x2o  <- (Id-P) %*% Xctr[ , 2]                 # x2ctr made orthogonal to x1ctr
Xc2  <- cbind(Xctr[ , 1], x2o)                # bind to matrix
Y    <- Xc2 %*% diag(1/sqrt(colSums(Xc2^2)))  # scale columns to length 1

vadir.prob <- Y[ , 2] + (1 / tan(theta)) * Y[ , 1]     # final new vector
cor(sat.prob, vadir.prob)
vadirs<-round((vadir.prob+abs(min(vadir.prob)))*25, 0)

cor(vadirs, attendence)
df<-cbind(name, sid, vadirs, gpa, sat.score, attendence, x, y, test.df)
df$sat.dummy[df$psat.dummy == "PSAT Taken"] <- "SAT Taken"
df$sat.score[df$sat.dummy == "No-SAT"] <- 0
df$year<-2005
df2005<-df
n     <- 250                   # length of vector
rho   <- 0.6                   # desired correlation = cos(angle)
theta <- acos(rho)             # corresponding angle
gpa.prob    <- rbeta(n, shape1=5, shape2=2)       # fixed given data
sat.prob.org    <- rbeta(n, shape1=7, shape2=2)    # new random data
X     <- cbind(gpa.prob, sat.prob.org)         # matrix
Xctr  <- scale(X, center=TRUE, scale=FALSE)   # centered columns (mean 0)

Id   <- diag(n)                               # identity matrix
Q    <- qr.Q(qr(Xctr[ , 1, drop=FALSE]))      # QR-decomposition, just matrix Q
P    <- tcrossprod(Q)          # = Q Q'       # projection onto space defined by x1
x2o  <- (Id-P) %*% Xctr[ , 2]                 # x2ctr made orthogonal to x1ctr
Xc2  <- cbind(Xctr[ , 1], x2o)                # bind to matrix
Y    <- Xc2 %*% diag(1/sqrt(colSums(Xc2^2)))  # scale columns to length 1

sat.prob <- Y[ , 2] + (1 / tan(theta)) * Y[ , 1]     # final new vector
cor(gpa.prob, sat.prob) 
gpa<-gpa.prob*100
sat.score<-round((sat.prob+abs(min(sat.prob)))*6000, 0)
sat.score[sat.score>2400]<-2400

n     <- 250                   # length of vector
rho   <- 0.85                   # desired correlation = cos(angle)
theta <- acos(rho)             # corresponding angle
attendence.prob.org    <- rbeta(n, shape1=20, shape2=.5)    # new random data
X     <- cbind(sat.prob, attendence.prob.org)         # matrix
Xctr  <- scale(X, center=TRUE, scale=FALSE)   # centered columns (mean 0)

Id   <- diag(n)                               # identity matrix
Q    <- qr.Q(qr(Xctr[ , 1, drop=FALSE]))      # QR-decomposition, just matrix Q
P    <- tcrossprod(Q)          # = Q Q'       # projection onto space defined by x1
x2o  <- (Id-P) %*% Xctr[ , 2]                 # x2ctr made orthogonal to x1ctr
Xc2  <- cbind(Xctr[ , 1], x2o)                # bind to matrix
Y    <- Xc2 %*% diag(1/sqrt(colSums(Xc2^2)))  # scale columns to length 1

attendence.prob <- Y[ , 2] + (1 / tan(theta)) * Y[ , 1]     # final new vector
cor(gpa.prob, attendence.prob)
attendence<-round((attendence.prob+abs(min(attendence.prob)))*150, 0)
attendence[attendence>100]<-100



n     <- 250                   # length of vector
rho   <- -0.55                   # desired correlation = cos(angle)
theta <- acos(rho)             # corresponding angle
vadir.prob.org    <- rnbinom(n, mu = 10, size = 1)    # new random data
X     <- cbind(sat.prob, vadir.prob.org)         # matrix
Xctr  <- scale(X, center=TRUE, scale=FALSE)   # centered columns (mean 0)

Id   <- diag(n)                               # identity matrix
Q    <- qr.Q(qr(Xctr[ , 1, drop=FALSE]))      # QR-decomposition, just matrix Q
P    <- tcrossprod(Q)          # = Q Q'       # projection onto space defined by x1
x2o  <- (Id-P) %*% Xctr[ , 2]                 # x2ctr made orthogonal to x1ctr
Xc2  <- cbind(Xctr[ , 1], x2o)                # bind to matrix
Y    <- Xc2 %*% diag(1/sqrt(colSums(Xc2^2)))  # scale columns to length 1

vadir.prob <- Y[ , 2] + (1 / tan(theta)) * Y[ , 1]     # final new vector
cor(sat.prob, vadir.prob)
vadirs<-round((vadir.prob+abs(min(vadir.prob)))*25, 0)

cor(vadirs, attendence)
df<-cbind(name, sid, vadirs, gpa, sat.score, attendence, x, y, test.df)
df$sat.dummy[df$psat.dummy == "PSAT Taken"] <- "SAT Taken"
df$sat.score[df$sat.dummy == "No-SAT"] <- 0
df$year<-2004
df2004<-df
n     <- 250                   # length of vector
rho   <- 0.6                   # desired correlation = cos(angle)
theta <- acos(rho)             # corresponding angle
gpa.prob    <- rbeta(n, shape1=5, shape2=2)       # fixed given data
sat.prob.org    <- rbeta(n, shape1=7, shape2=2)    # new random data
X     <- cbind(gpa.prob, sat.prob.org)         # matrix
Xctr  <- scale(X, center=TRUE, scale=FALSE)   # centered columns (mean 0)

Id   <- diag(n)                               # identity matrix
Q    <- qr.Q(qr(Xctr[ , 1, drop=FALSE]))      # QR-decomposition, just matrix Q
P    <- tcrossprod(Q)          # = Q Q'       # projection onto space defined by x1
x2o  <- (Id-P) %*% Xctr[ , 2]                 # x2ctr made orthogonal to x1ctr
Xc2  <- cbind(Xctr[ , 1], x2o)                # bind to matrix
Y    <- Xc2 %*% diag(1/sqrt(colSums(Xc2^2)))  # scale columns to length 1

sat.prob <- Y[ , 2] + (1 / tan(theta)) * Y[ , 1]     # final new vector
cor(gpa.prob, sat.prob) 
gpa<-gpa.prob*100
sat.score<-round((sat.prob+abs(min(sat.prob)))*6000, 0)
sat.score[sat.score>2400]<-2400

n     <- 250                   # length of vector
rho   <- 0.85                   # desired correlation = cos(angle)
theta <- acos(rho)             # corresponding angle
attendence.prob.org    <- rbeta(n, shape1=20, shape2=.5)    # new random data
X     <- cbind(sat.prob, attendence.prob.org)         # matrix
Xctr  <- scale(X, center=TRUE, scale=FALSE)   # centered columns (mean 0)

Id   <- diag(n)                               # identity matrix
Q    <- qr.Q(qr(Xctr[ , 1, drop=FALSE]))      # QR-decomposition, just matrix Q
P    <- tcrossprod(Q)          # = Q Q'       # projection onto space defined by x1
x2o  <- (Id-P) %*% Xctr[ , 2]                 # x2ctr made orthogonal to x1ctr
Xc2  <- cbind(Xctr[ , 1], x2o)                # bind to matrix
Y    <- Xc2 %*% diag(1/sqrt(colSums(Xc2^2)))  # scale columns to length 1

attendence.prob <- Y[ , 2] + (1 / tan(theta)) * Y[ , 1]     # final new vector
cor(gpa.prob, attendence.prob)
attendence<-round((attendence.prob+abs(min(attendence.prob)))*150, 0)
attendence[attendence>100]<-100



n     <- 250                   # length of vector
rho   <- -0.55                   # desired correlation = cos(angle)
theta <- acos(rho)             # corresponding angle
vadir.prob.org    <- rnbinom(n, mu = 10, size = 1)    # new random data
X     <- cbind(sat.prob, vadir.prob.org)         # matrix
Xctr  <- scale(X, center=TRUE, scale=FALSE)   # centered columns (mean 0)

Id   <- diag(n)                               # identity matrix
Q    <- qr.Q(qr(Xctr[ , 1, drop=FALSE]))      # QR-decomposition, just matrix Q
P    <- tcrossprod(Q)          # = Q Q'       # projection onto space defined by x1
x2o  <- (Id-P) %*% Xctr[ , 2]                 # x2ctr made orthogonal to x1ctr
Xc2  <- cbind(Xctr[ , 1], x2o)                # bind to matrix
Y    <- Xc2 %*% diag(1/sqrt(colSums(Xc2^2)))  # scale columns to length 1

vadir.prob <- Y[ , 2] + (1 / tan(theta)) * Y[ , 1]     # final new vector
cor(sat.prob, vadir.prob)
vadirs<-round((vadir.prob+abs(min(vadir.prob)))*25, 0)

cor(vadirs, attendence)
df<-cbind(name, sid, vadirs, gpa, sat.score, attendence, x, y, test.df)
df$sat.dummy[df$psat.dummy == "PSAT Taken"] <- "SAT Taken"
df$sat.score[df$sat.dummy == "No-SAT"] <- 0
df$year<-2003
df2003<-df
n     <- 250                   # length of vector
rho   <- 0.6                   # desired correlation = cos(angle)
theta <- acos(rho)             # corresponding angle
gpa.prob    <- rbeta(n, shape1=5, shape2=2)       # fixed given data
sat.prob.org    <- rbeta(n, shape1=7, shape2=2)    # new random data
X     <- cbind(gpa.prob, sat.prob.org)         # matrix
Xctr  <- scale(X, center=TRUE, scale=FALSE)   # centered columns (mean 0)

Id   <- diag(n)                               # identity matrix
Q    <- qr.Q(qr(Xctr[ , 1, drop=FALSE]))      # QR-decomposition, just matrix Q
P    <- tcrossprod(Q)          # = Q Q'       # projection onto space defined by x1
x2o  <- (Id-P) %*% Xctr[ , 2]                 # x2ctr made orthogonal to x1ctr
Xc2  <- cbind(Xctr[ , 1], x2o)                # bind to matrix
Y    <- Xc2 %*% diag(1/sqrt(colSums(Xc2^2)))  # scale columns to length 1

sat.prob <- Y[ , 2] + (1 / tan(theta)) * Y[ , 1]     # final new vector
cor(gpa.prob, sat.prob) 
gpa<-gpa.prob*100
sat.score<-round((sat.prob+abs(min(sat.prob)))*6000, 0)
sat.score[sat.score>2400]<-2400

n     <- 250                   # length of vector
rho   <- 0.85                   # desired correlation = cos(angle)
theta <- acos(rho)             # corresponding angle
attendence.prob.org    <- rbeta(n, shape1=20, shape2=.5)    # new random data
X     <- cbind(sat.prob, attendence.prob.org)         # matrix
Xctr  <- scale(X, center=TRUE, scale=FALSE)   # centered columns (mean 0)

Id   <- diag(n)                               # identity matrix
Q    <- qr.Q(qr(Xctr[ , 1, drop=FALSE]))      # QR-decomposition, just matrix Q
P    <- tcrossprod(Q)          # = Q Q'       # projection onto space defined by x1
x2o  <- (Id-P) %*% Xctr[ , 2]                 # x2ctr made orthogonal to x1ctr
Xc2  <- cbind(Xctr[ , 1], x2o)                # bind to matrix
Y    <- Xc2 %*% diag(1/sqrt(colSums(Xc2^2)))  # scale columns to length 1

attendence.prob <- Y[ , 2] + (1 / tan(theta)) * Y[ , 1]     # final new vector
cor(gpa.prob, attendence.prob)
attendence<-round((attendence.prob+abs(min(attendence.prob)))*150, 0)
attendence[attendence>100]<-100



n     <- 250                   # length of vector
rho   <- -0.55                   # desired correlation = cos(angle)
theta <- acos(rho)             # corresponding angle
vadir.prob.org    <- rnbinom(n, mu = 10, size = 1)    # new random data
X     <- cbind(sat.prob, vadir.prob.org)         # matrix
Xctr  <- scale(X, center=TRUE, scale=FALSE)   # centered columns (mean 0)

Id   <- diag(n)                               # identity matrix
Q    <- qr.Q(qr(Xctr[ , 1, drop=FALSE]))      # QR-decomposition, just matrix Q
P    <- tcrossprod(Q)          # = Q Q'       # projection onto space defined by x1
x2o  <- (Id-P) %*% Xctr[ , 2]                 # x2ctr made orthogonal to x1ctr
Xc2  <- cbind(Xctr[ , 1], x2o)                # bind to matrix
Y    <- Xc2 %*% diag(1/sqrt(colSums(Xc2^2)))  # scale columns to length 1

vadir.prob <- Y[ , 2] + (1 / tan(theta)) * Y[ , 1]     # final new vector
cor(sat.prob, vadir.prob)
vadirs<-round((vadir.prob+abs(min(vadir.prob)))*25, 0)

cor(vadirs, attendence)
df<-cbind(name, sid, vadirs, gpa, sat.score, attendence, x, y, test.df)
df$sat.dummy[df$psat.dummy == "PSAT Taken"] <- "SAT Taken"
df$sat.score[df$sat.dummy == "No-SAT"] <- 0
df$year<-2002
df2002<-df

df.long<-rbind(df2002, df2003, df2004, df2005, 
               df2006, df2007, df2008, df2009, 
               df2010, df2011, df2012, df2013, 
               df2014)
df.long$name<-as.character(df.long$name)
df.long<-df.long[order(df.long$name),]
# setwd("/Users/cerees/Google Drive/Active Projects/Center for Human Services Research/Albany Networks/Hypothetical Example")


igraph.options(vertex.size=5, vertex.label=NA, edge.width=1, edge.color="black", edge.arrow.size=0)

df.graph<-erdos.renyi.game(250, 0.006, type=c("gnp", "gnm"),
                            directed = F, loops = F, set.seed=1984) #make sure to include trailing zero on pobability of tie 
df.graph.friends<- erdos.renyi.game(250, 0.006, type=c("gnp", "gnm"),
                            directed = F, loops = F, set.seed=1984) #make sure to include trailing zero on pobability of tie 
df.graph.cooff<- erdos.renyi.game(250, 0.003, type=c("gnp", "gnm"),
                            directed = F, loops = F, set.seed=1984) #make sure to include trailing zero on pobability of tie 
df.graph.enemies<- erdos.renyi.game(250, 0.002, type=c("gnp", "gnm"),
                            directed = T, loops = F, set.seed=1984) #make sure to include trailing zero on pobability of tie 
V(df.graph)$gender <- df2014$sex
V(df.graph)$color <- ifelse(V(df.graph)$gender==1, "red", "blue")
V(df.graph)$size<-(scale(df2014$gpa)+abs(min(scale(df2014$gpa))))
df.graph.el<-get.edgelist(df.graph)
df.graph.el<-data.frame(df.graph.el)
df.graph.el$weight<-sample(1:2, length(df.graph.el$X1), TRUE)
df2014$group<-sample(1:6, 250, T)
df.graph.el<-df.graph.el[,c(2,1,3)]
colnames(df.graph.el) <- c("source","target", "value")
links<-df.graph.el
nodes<-df2014
links$source<-(links$source-(min(links$source))+1)
links$target<-(links$target-(min(links$source))+1)

x_bar <- "x&#772;"
sigma_hat <- "&sigma;&#770;"

# brushed_summary <- function(items, session, page_loc, ...) {
#   if (nrow(items) == 0) return()
#   
#   items$key__ <- NULL
#   lines <- Map(function(name, vals) {c
#                                      paste0(x_bar, " = ", round(mean(vals), 2), "; ",
#                                             sigma_hat, " = ", round(sd(vals), 2)
#                                      )
#   }, names(items), items)
#   html <- paste(c((paste0(biv.xvar.name, ": ",lines[1])), (paste0(biv.yvar.name,": ",lines[2]))), collapse = "<br />\n")
#   
#   show_tooltip(session, page_loc$r + 5, page_loc$t, html)
# }

tooltipFunc <- function(index, value) {
  function(x) {
    if (is.null(x)) return(NULL)
    else {
      strwrap(
        paste(index[ (value >= x$xmin_ ) & (value <= x$xmax_) ],
              collapse=', '),
        width = 30)
    }
  }
}

all_values <- function(x) {
  if(is.null(x)) return(NULL)
  paste0(names(x), ": ", format(x), collapse = "<br />")
}

values <- function(x) {
  if(is.null(x)) return(NULL)
  paste0("Bin Span: ",round(x$xmin, 2)," to ",round(x$xmax, 2),"<br />",
         "Mean Bin Value: ",round(((x$xmin+x$xmax)/2), 2),"<br />", 
         "Count: ",x$stack_upr_)
}
