# columns for Table 4 k=3
rm(list=ls())

library(gunrep)

# setting
nc=3
for(m in 10:20)
{
  if(m==10 | m==11)
  {gc=c(0,1,3)+1}
  if(m >=12 & m <= 18)
  {gc=c(0,1,4)+1}
  if(m==19)
  {gc=c(0,1,8)+1}
  if(m==20)
  {gc=c(0,1,5)+1}
  
  # cyclic design
  abmat=gencabd(m,nc,gc)
  # evaluate A_c and Att
  Ac=gvabd(m,nc,abmat)
  m1=m*(m-nc)
  Att=2+2*m*(m-1)*(Ac-2/m)/(m1-1)
  print(c(m,nc,Att))
}
