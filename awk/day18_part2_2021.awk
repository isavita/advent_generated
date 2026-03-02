
function parse(s,v,d,   i,c,dp,n,val,len){
    delete v;delete d;dp=0;n=0
    for(i=1;i<=length(s);i++){
        c=substr(s,i,1)
        if(c=="[")dp++
        else if(c=="]")dp--
        else if(c~/[0-9]/){
            val=substr(s,i);match(val,/^[0-9]+/);len=RLENGTH
            v[++n]=substr(val,1,len);d[n]=dp;i+=len-1
        }
    }
    return n
}

function do_explode(v,d,n,  i,j){
    for(i=1;i<n;i++){
        if(d[i]>4&&d[i]==d[i+1]){
            if(i>1)v[i-1]+=v[i]
            if(i+2<=n)v[i+2]+=v[i+1]
            v[i]=0;d[i]--
            for(j=i+1;j<n;j++){v[j]=v[j+1];d[j]=d[j+1]}
            return n-1
        }
    }
    return 0
}

function do_split(v,d,n,  i,j){
    for(i=1;i<=n;i++){
        if(v[i]>=10){
            for(j=n;j>=i+1;j--){v[j+1]=v[j];d[j+1]=d[j]}
            d[i]++;d[i+1]=d[i]
            v[i+1]=int((v[i]+1)/2);v[i]=int(v[i]/2)
            return n+1
        }
    }
    return 0
}

function mag(v,d,n,  tv,td,tn,i,j,m,k){
    delete tv;delete td;for(k=1;k<=n;k++){tv[k]=v[k];td[k]=d[k]}
    tn=n
    while(tn>1){
        m=0;for(i=1;i<=tn;i++)if(td[i]>m)m=td[i]
        for(i=1;i<tn;i++){
            if(td[i]==m&&td[i+1]==m){
                tv[i]=3*tv[i]+2*tv[i+1];td[i]--
                for(j=i+1;j<tn;j++){tv[j]=tv[j+1];td[j]=td[j+1]}
                tn--;break
            }
        }
    }
    return tv[1]
}

function solve(idx1,idx2,  v,d,n,i,res){
    delete v;delete d;n=0
    for(i=1;i<=pn[idx1];i++){n++;v[n]=vc[idx1,i];d[n]=dc[idx1,i]+1}
    for(i=1;i<=pn[idx2];i++){n++;v[n]=vc[idx2,i];d[n]=dc[idx2,i]+1}
    while(1){
        if(res=do_explode(v,d,n)){n=res;continue}
        if(res=do_split(v,d,n)){n=res;continue}
        break
    }
    return mag(v,d,n)
}

BEGIN {
    while((getline < "input.txt")>0){
        c++;n=parse($0,vv,dd);pn[c]=n
        for(k=1;k<=n;k++){vc[c,k]=vv[k];dc[c,k]=dd[k]}
    }
    for(i=1;i<=c;i++)
        for(j=1;j<=c;j++)
            if(i!=j){m=solve(i,j);if(m>max)max=m}
    print max
}
