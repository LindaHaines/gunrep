## Table 6 A-optimal designs
rm(list=ls())
library(MASS)
library(gunrep)

# Example 5.2.1
m=9
nc=3
# auxiliary block to square array
abmat=as.matrix(rbind(c(1,2,3,4,5,6,7,8,9),
                      c(4,8,9,3,1,7,5,6,2),
                      c(7,5,6,8,9,2,3,1,4)))
#abmat
invisible(abmat)
Ac=gvabd(m,nc,abmat)
Amet=gac(m,nc,Ac)
round(c(m,nc,Amet),4)

# ============================

# Example 5.3
m=10
nc=3
# auxiliary block to square array
abmat=as.matrix(rbind(rbind(c(1,8,4,5,10,3,2,9,7,6),
                            c(2,9,7,6,3,1,8,4,5,10),
                            c(5,10,1,8,4,6,3,2,9,7))))
#abmat
Ac=gvabd(m,nc,abmat)
Amet=gac(m,nc,Ac)
round(c(m,nc,Amet),4)

# ============================

# Example 5.1
m=12
nc=3
abmat=matrix(c(2,4,9,12,11,5,6,7,1,10,3,8,
               3,5,8,11,1,12,9,10,7,2,4,6,
               1,6,7,10,8,2,3,4,5,9,11,12
),ncol=12,byrow=TRUE) 
#abmat
Ac=gvabd(m,nc,abmat)
Amet=gac(m,nc,Ac)
round(c(m,nc,Amet),4)

# ============================

# Example 5.2.2
m=16
nc=4
abmat=as.matrix(rbind(rbind(
  c(1,8,10,15,9,2,7,16,6,12,3,13,14,11,5,4), 
  c(4,5,11,14,1,10,15,8,16,2,9,7,12,13,3,6), 
  c(3,6,12,13,5,14,11,4,1,15,8,10,7,2,16,9),
  c(2,7,9,16,13,6,3,12,11,5,14,4,1,8,10,15))))
#abmat
Ac=gvabd(m,nc,abmat)
Amet=gac(m,nc,Ac)
round(c(m,nc,Amet),4)

# ============================

# Example 5.2.3
m=25
nc=5
# auxiliary block to square array
abmat=as.matrix(rbind(rbind(
  c(1,10,14,18,22,6,2,23,19,15,7,20,3,11,24,8,25,12,4,16,9,13,17,21,5),
  c(5,9,13,17,21,1,22,18,14,10,19,2,15,23,6,24,11,3,20,7,12,16,25,4,8),
  c(4,8,12,16,25,21,17,13,9,5,1,14,22,10,18,15,2,19,6,23,20,24,3,7,11),
  c(3,7,11,20,24,16,12,8,4,25,13,21,9,17,5,1,18,10,22,14,23,2,6,15,19),
  c(2,6,15,19,23,11,7,3,24,20,25,8,16,4,12,17,9,21,13,5,1,10,14,18,22))))
#abmat
Ac=gvabd(m,nc,abmat)
Amet=gac(m,nc,Ac)
round(c(m,nc,Amet),4)
