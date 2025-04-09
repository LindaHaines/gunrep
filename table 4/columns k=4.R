# columns for Table 4 k=4
rm(list=ls())
library(gunrep)

# setting
nc=4
for(m in 14:26)
{if(m==14)
{gc=c(0,1,4,6)+1}
if(m==15 | m==17|m==18)
{gc=c(0,1,3,7)+1}
if(m==16)
{gc=c(0,1,3,12)+1}
if(m==19)
{gc=c(0,1,3,8)+1}
if(m==20)
{gc=c(0,1,3,14)+1}
if(m==21 | m==22)
{gc=c(0,1,3,9)+1}
if(m==23 | m==24|m==25)
{gc=c(0,1,3,10)+1}
if(m==26)
{gc=c(0,1,3,11)+1}
  # cyclic design
  abmat=gencabd(m,nc,gc)
  # evaluate A_c and Att
  Ac=gvabd(m,nc,abmat)
  m1=m*(m-nc)
  Att=2+2*m*(m-1)*(Ac-2/m)/(m1-1)
  print(c(m,nc,Att))
}


