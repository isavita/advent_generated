
#!/usr/bin/awk -f
BEGIN {
    lineCnt=0
    maxW=0
    while ((getline l < "input.txt") > 0) {
        lineCnt++
        lines[lineCnt]=l
        if (length(l)>maxW) maxW=length(l)
    }
    close("input.txt")

    for (col=1; col<=maxW; col++) {
        isSep[col]=1
        for (row=1; row<=lineCnt; row++) {
            if (col <= length(lines[row]) && substr(lines[row],col,1) !~ /[[:space:]]/) {
                isSep[col]=0
                break
            }
        }
    }

    grandTotal="0"
    inBlock=0
    start=0
    for (col=1; col<=maxW; col++) {
        if (isSep[col]==0) {
            if (inBlock==0) { inBlock=1; start=col }
        } else {
            if (inBlock==1) { processBlock(start,col-1); inBlock=0 }
        }
    }
    if (inBlock==1) processBlock(start,maxW)

    print "Grand total: " grandTotal
}

function addStr(a,b,   i,da,db,sum,carr,res) {
    i=length(a)
    j=length(b)
    carr=0
    res=""
    while (i>0 || j>0 || carr>0) {
        if (i>0) { da=substr(a,i,1)+0; i-- } else { da=0 }
        if (j>0) { db=substr(b,j,1)+0; j-- } else { db=0 }
        sum=da+db+carr
        res=(sum%10) res
        carr=int(sum/10)
    }
    return res
}

function mulStr(a,b,   i,da,j,db,tmp,tmpLen,k,sum,carr) {
    if (a=="0" || b=="0") return "0"
    la=length(a); lb=length(b)
    tmpLen=la+lb
    for (i=1;i<=tmpLen;i++) tmp[i]=0
    for (i=1;i<=la;i++) {
        da=substr(a,la-i+1,1)+0
        for (j=1;j<=lb;j++) {
            db=substr(b,lb-j+1,1)+0
            tmp[i+j-1]+=da*db
        }
    }
    carr=0
    for (k=1;k<=tmpLen;k++) {
        sum=tmp[k]+carr
        tmp[k]=sum%10
        carr=int(sum/10)
    }
    if (carr>0) { tmp[tmpLen+1]=carr; tmpLen++ }
    while (tmpLen>1 && tmp[tmpLen]==0) tmpLen--
    res=""
    for (i=tmpLen;i>=1;i--) res=res tmp[i]
    return res
}

function processBlock(start,end,   col,row,char,buf,op,idx,num,i,blockRes) {
    op="+"
    delete nums
    idx=0
    for (col=start; col<=end; col++) {
        buf=""
        for (row=1; row<=lineCnt; row++) {
            if (col <= length(lines[row])) {
                char=substr(lines[row],col,1)
                if (char ~ /[0-9]/) buf=buf char
                else if (char=="+" || char=="*") op=char
            }
        }
        if (buf!="") { idx++; nums[idx]=buf }
    }
    if (idx==0) return
    if (op=="*") {
        blockRes="1"
        for (i=1;i<=idx;i++) blockRes=mulStr(blockRes,nums[i])
    } else {
        blockRes="0"
        for (i=1;i<=idx;i++) blockRes=addStr(blockRes,nums[i])
    }
    grandTotal=addStr(grandTotal,blockRes)
}
