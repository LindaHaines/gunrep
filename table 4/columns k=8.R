# columns for Table 4 k=8
rm(list=ls())
library(gunrep)

# setting
nc=8
for(m in 27:30)
{if(m==27)
{gc=c(0,1,2,3,6,10,16,21)+1}
if(m==28)
{gc=c(0,1,2,3,6,11,15,22)+1}
if(m==29)
{gc=c(0,1,2,4,7,12,16,22)+1}
if(m==30)
{gc=c(0,1,2,5,8,10,17,21)+1} 
  # cyclic design
  abmat=gencabd(m,nc,gc)
  # evaluate A_c and Att
  Ac=gvabd(m,nc,abmat)
  m1=m*(m-nc)
  Att=2+2*m*(m-1)*(Ac-2/m)/(m1-1)
  print(c(m,nc,Att))
}