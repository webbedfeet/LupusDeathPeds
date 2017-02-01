find.cens <- function(x,n,target, a,b){
  j=0
  p=prod(1-1/rev((n-length(x)+1):n))
  while(round(p,2) >= target & j <n-length(x)){
    j=j+1
    y = a + ppoints(j)*(b-a)
    o = order(c(x,y))
    st = c(rep(1,length(x)),rep(0,length(y)))[o]
    p = prod(1 - st/(n-(1:length(st))+1))
    #     print(p)
  }
  return(max(0,j-1))
}
