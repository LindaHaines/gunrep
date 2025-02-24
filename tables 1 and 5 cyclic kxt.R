# generate all designs for all CYCLIC k X t abd
# for ease of programming take m=t and k=nc
# NOTE: package ibd could also be used here
rm(list=ls())
library(tictoc)
library(gunrep)
library(dplyr)

tic()

# setting
m=25
nc=3

# enumerate
num=choose(m,nc) 

# coordinates for cycling through all designs 
gt=t(combn(m,nc))
ngt=nrow(gt)

# counter for number of cycles
nprint=10000

# set up matrix for saving results 
# nc design coordinates, nc spacings and metric Ac
gtot=matrix(0,num,(2*nc+4))

# cycle over all designs
itc=1
for(ndes in 1:ngt)
{
# generate cyclic auxiliary block design from 
# initial block coordinates
  gc=gt[ndes,] # coordinates
  abmat=gencabd(m,nc,gc)
# evaluate A_c
  Ac=gvabd(m,nc,abmat)
  gs=gc2s(m,nc,gc) # spacing
# calculate Acc, Act and Att
  if(Ac==0)
    {gtot[ndes,]=c(gc,gs,0,0,0,0)}else{
     gmet=gac(m,nc,Ac)
     gtot[ndes,]=c(gc,gs,gmet)}
# monitor cycles
  if(itc==floor(itc/nprint)*nprint)
  {print(itc)}
  itc=itc+1
}

# complete equivalence classes
gvec=round(gtot,12)
g1=as.data.frame(table(gvec[,(2*nc+1)]))
gtot=gtot[order(gtot[,(2*nc+1)]), ]

# print mt for each class: vary mt if needed: error if mt too large, reduce
print(g1)
mt=trunc(m/2)
mt=5
print(c("Coords Spacing Ac Acc Act Att"))
results=round(gcount(g1,mt,gtot),6)
print(results[(mt+1):(2*mt),])

toc()
