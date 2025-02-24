#' Converts coordinates to spacing for cyclic designs
#'
#' @param m    Number of treatments
#' @param nc   Number of rows/controls
#' @param gc   Coordinates -  vector
#'
#' @return gs Spacing
#'
gc2s=function(m,nc,gc)
{if(length(gc) != nc)
{print("invalid nc or length of length(gc)")
  gs=0}else{gs=c(gc[2]-gc[1])
  for(i in 3:nc)
  {gs=c(gs,c(gc[i]-gc[(i-1)]))}
  gs=c(gs,m+gc[1]-gc[nc])}

  return(gs)
}

#' Converts spacing to coordinates for cyclic designs
#'
#' @param m      Number of treatments
#' @param i1     Starting coordinate in first row
#' @param space  Spacing - vector
#' @param nc     Number of rows/controls
#'
#' @return gc    Coordinates
#'
gs2c=function(m,nc,i1,space)
{
  if(sum(space) != m)
  {print("invalid space or m")
    gc=0}else{
      gc0=as.matrix(c(cumsum(space[1:(nc-1)])))
      gc=c(i1,gc0+i1)
      for(i in 2:nc)
      {if(gc[i]>m){gc[i]=gc[i]%%m}}
    }
  return(gc)}

#' Generate a cyclic auxiliary block design from an initial block
#'
#' @param m   Number of treatments
#' @param nc. Number of controls
#' @param ib  Initial block - vector
#'
#' @return abd Augmented block design. Otherwise
#'             0 for an invalid entry
#'
gencabd=function(m,nc,ib)
{
  if(length(ib) != nc)
  {print("invalid nc or length of ib")
    abd=0}
  if(max(ib) > m)
  {print("invalid number of treatments in ib")
    abd=0}
  if(length(ib)== nc & max(ib) <= m)
  {abd=matrix(0,nc,m)
  for(i in 1:nc)
  {
    if(ib[i]==1){abd[i,]=seq(1:m)}else
    {abd[i,]=(c(seq(ib[i],m),seq(1:(ib[i]-1))))}
  }
  }
  return(abd)
}

#' Calculates treatment information matrix C and metric Aabd
#' for an auxiliary block design
#'
#' @param m     Number of treatments
#' @param nc    Number of rows
#' @param abmat Auxiliary block design r x t
#'
#' @return.     Metric Aabd  0 if matrix C is not connected
#'
gvabd=function(m,nc,abmat)
{
  # requisite matrices for calculating the N, R, C matrix
  nr=nrow(abmat)
  ncl=ncol(abmat)
  imat=diag(m)
  jmat=matrix(1,m,m)
  ijmat=imat-(jmat/m)
  # calculate incidence matrix nmat and rmat
  rmat=nc*diag(m)
  nmat=matrix(0,m,m)
  for(r in 1:nr)
  {for(j in 1:ncl)
    nmat[j,abmat[r,j]]=1
  }
  # calculate C mat
  cmat=rmat-(nmat)%*%t(nmat)/nc
  crank=qr(cmat)$rank
  cmatmp=ginv(cmat)
  if(crank<(m-1))
  {gac=0}
  if(crank==(m-1))
  {
    # variances
    gac=2*sum(diag(cmatmp%*%ijmat))/(m-1)
  }
  return(gac)
}

#' isometric classes
#'
#' @param g1   number of classes
#' @param mt   number to print per class
#' @param gtot num X 2nc+4 matrix ordered
#'             with cols gc,gs,Ac,Acc,Act,Att
#'
#' @returns    matrix with top mt from gtot for each class
#'
gcount=function(g1,mt,gtot)
{
  # classes
  giso=as.matrix(g1[,2])
  niso=nrow(giso)
  ni=cumsum(c(1,giso))
  # save
  nf=niso*mt
  nfin=c(1,seq(1,niso)*mt+1)
  gfin=matrix(0,nf,ncol(gtot))
  #cycle
  for(i in 1:(niso))
  {
    gtemp=(gtot[ni[i]:(ni[(i+1)]-1),])
    gfin[nfin[i]:(nfin[i+1]-1),]=gtemp[1:mt,]
  }
  return(gfin)
}

#' Metrics from Ac
#'
#'
#' @param m    number of treatments
#' @param nc   number of controls
#' @param Ac   Ac
#'
#' @returns    Ac,Acc,Act,Att
#'
gac=function(m,nc,Ac)
{
Acc=2/m
Act=1+1/m+(m-1)*(Ac-2/m)/(m-nc)
aterm=(nc-1)/(nc*m)+1/(nc*(m-nc))
m1=m*(m-nc)
Att=2+2*m*(m-1)*(Ac-2/m)/(m1-1)
avec=c(Ac,Acc,Act,Att)
return(avec)
}

#' Converts an auxiliary block design to a
#' square array design
#'
#' @param m   Number of blocks m
#' @param nc  Number of rows
#' @param abd Auxiliary block design
#'
#' @return    sqmat square array design
#'
gab2sq=function(m,nc,abd)
{
  #convert to square array
  sqmat=matrix(0,m,m)
  for(r in 1:nc)
  {for(j in 1:m)
    sqmat[j,abd[r,j]]=r}
  return(sqmat)}
