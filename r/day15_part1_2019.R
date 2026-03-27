run <- function(s, inp) {
  m <- s$m; ip <- s$ip; rb <- s$rb
  g <- function(n) {
    mo <- modes[n]; v <- m[ip+n+1]
    i <- if(mo==0) v+1 else if(mo==2) v+rb+1 else -1
    if(mo==1) return(v)
    if(i<1 || i>length(m)) return(0)
    res <- m[i]; if(is.na(res)) 0 else res
  }
  while (T) {
    inst <- m[ip+1]; op <- inst%%100; modes <- floor(inst/c(100,1000,10000))%%10
    if(op==1){ i<-if(modes[3]==2) m[ip+4]+rb+1 else m[ip+4]+1; m[i]<-g(1)+g(2); ip<-ip+4 }
    else if(op==2){ i<-if(modes[3]==2) m[ip+4]+rb+1 else m[ip+4]+1; m[i]<-g(1)*g(2); ip<-ip+4 }
    else if(op==3){ i<-if(modes[1]==2) m[ip+2]+rb+1 else m[ip+2]+1; m[i]<-inp; ip<-ip+2 }
    else if(op==4){ o<-g(1); ip<-ip+2; return(list(st=list(m=m,ip=ip,rb=rb),out=o)) }
    else if(op==5){ ip<-if(g(1)!=0) g(2) else ip+3 }
    else if(op==6){ ip<-if(g(1)==0) g(2) else ip+3 }
    else if(op==7){ i<-if(modes[3]==2) m[ip+4]+rb+1 else m[ip+4]+1; m[i]<-as.numeric(g(1)<g(2)); ip<-ip+4 }
    else if(op==8){ i<-if(modes[3]==2) m[ip+4]+rb+1 else m[ip+4]+1; m[i]<-as.numeric(g(1)==g(2)); ip<-ip+4 }
    else if(op==9){ rb<-rb+g(1); ip<-ip+2 }
    else if(op==99) return(NULL)
  }
}

p <- as.numeric(strsplit(readLines("input.txt", warn=F), ",")[[1]])
m <- numeric(1e4); m[1:length(p)] <- p
v <- new.env(); v[["0 0"]] <- 1
q <- list(list(x=0,y=0,d=0,st=list(m=m,ip=0,rb=0)))
h <- 1; dx <- c(0,0,-1,1); dy <- c(1,-1,0,0)
while(h <= length(q)){
  c <- q[[h]]; h <- h + 1
  for(i in 1:4){
    nx <- c$x+dx[i]; ny <- c$y+dy[i]; k <- paste(nx,ny)
    if(is.null(v[[k]])){
      v[[k]] <- 1; res <- run(c$st, i)
      if(!is.null(res) && res$out != 0){
        if(res$out == 2){ cat(c$d+1, "\n"); h <- 1e9; break }
        q[[length(q)+1]] <- list(x=nx,y=ny,d=c$d+1,st=res$st)
      }
    }
  }
}