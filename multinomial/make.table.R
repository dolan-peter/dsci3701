make.table=function(n,p,q){
  r=1-p-q
  count.tab=matrix(0,nrow=n+1,ncol=n+1) # Need space for 0 to n
  index.tab=matrix(-1,nrow=n+1,ncol=n+1)
  m=log(base=2,n)+1 # 
  m=ceiling(m) # round it up if necessary
  s=2^m # This will be our scaling factor
  
  for(num in 0:n){
    for(den in 0:n){
      if(num+den>n){next;}
      count.tab[num+1,den+1]=dmultinom(c(num,den,n-num-den),prob=c(p,q,r))
      if(den!=0){
        index.tab[num+1,den+1]=floor(num*s/den)
      } else {
        index.tab[num+1,den+1]=Inf
      }
    }
  }
  attr(count.tab,"n")=n
  attr(count.tab,"p")=p
  attr(count.tab,"q")=q
  attr(index.tab,"scale")=s #Divide by this number to estimate fraction
  list(count=count.tab,index=index.tab)
}
