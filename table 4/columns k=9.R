# columns for Table 4 k=9
rm(list=ls())
library(gunrep)

# setting
nc=9
for(m in 30)
{if(m==30)
{gc=c(0,1,2,3,7,11,13,16,25)+1}
  # cyclic design
  abmat=gencabd(m,nc,gc)
  # evaluate A_c and Att
  Ac=gvabd(m,nc,abmat)
  m1=m*(m-nc)
  Att=2+2*m*(m-1)*(Ac-2/m)/(m1-1)
  print(c(m,nc,Att))
}