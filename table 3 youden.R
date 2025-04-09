# metrics for Youden squares
rm(list=ls())

# setting
m=7
nc=3
lam=1

m=13
nc=4
lam=1

m=16
nc=6
lam=2

m=21
nc=5
lam=1

m=31
nc=6
lam=1

# calculate Ac, Acc, Act, Att
abd=2*nc/(lam*m)
acc=2/m
act=1+1/m+2*nc/(lam*m)
att=2+4*nc*(m-nc)/((m*(m-nc)-1)*lam)
round(c(abd,acc,act,att),4)
