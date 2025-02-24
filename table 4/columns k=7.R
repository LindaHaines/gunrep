# columns for Table 4 k=7
rm(list=ls())
library(gunrep)

# setting
nc=7
for(m in 24:30)
{
if(m==24)
{gc=c(0,1,2,4,7,15,19)+1}
if(m==25)
{gc=c(0,1,2,4,10,15,22)+1}
if(m==26)
{gc=c(0,1,2,5,7,17,20)+1}  
if(m==27)
{gc=c(0,1,2,4,8,13,19)+1}
if(m==28)
{gc=c(0,1,2,4,9,15,20)+1}
if(m==29)
{gc=c(0,1,2,4,7,17,22)+1}
if(m==30)
{gc=c(0,1,2,6,18,21,23)+1} 
  # cyclic design
  abmat=gencabd(m,nc,gc)
  # evaluate A_c and Att
  Ac=gvabd(m,nc,abmat)
  m1=m*(m-nc)
  Att=2+2*m*(m-1)*(Ac-2/m)/(m1-1)
  print(c(m,nc,Att))
}