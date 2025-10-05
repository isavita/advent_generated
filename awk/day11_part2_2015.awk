#!/usr/bin/awk -f
BEGIN{
    s="abcdefghijklmnopqrstuvwxyz"
    cmd="input.txt"
    while((getline line < cmd)==1){
        password=line
        break
    }
    close(cmd)
}
function hasStraight(p,i,ch1,ch2,ch3,pos1,pos2,pos3){
    for(i=1;i<=length(p)-2;i++){
        ch1=substr(p,i,1); ch2=substr(p,i+1,1); ch3=substr(p,i+2,1)
        pos1=index(s,ch1); pos2=index(s,ch2); pos3=index(s,ch3)
        if(pos1 && pos2==pos1+1 && pos3==pos1+2) return 1
    }
    return 0
}
function containsInvalidLetters(p,i,ch){
    for(i=1;i<=length(p);i++){
        ch=substr(p,i,1)
        if(ch=="i"||ch=="o"||ch=="l") return 1
    }
    return 0
}
function hasTwoPairs(p,i,ch,count){
    count=0
    for(i=1;i<length(p);i++){
        ch=substr(p,i,1)
        if(ch==substr(p,i+1,1)){
            count++
            i++
        }
    }
    return count>=2
}
function isValidPassword(p){
    return hasStraight(p) && !containsInvalidLetters(p) && hasTwoPairs(p)
}
function incrementPassword(p,i,len,ch,pos,newPos,newCh){
    len=length(p)
    for(i=len;i>=1;i--){
        ch=substr(p,i,1)
        pos=index(s,ch)
        if(pos==26){
            p=substr(p,1,i-1)"a"substr(p,i+1)
        } else {
            newPos=pos+1
            newCh=substr(s,newPos,1)
            if(newCh=="i"||newCh=="o"||newCh=="l"){
                newPos+=1
                if(newPos>26) newPos=1
                newCh=substr(s,newPos,1)
            }
            p=substr(p,1,i-1)newCh substr(p,i+1)
            break
        }
    }
    return p
}
function findNextPassword(p){
    do{
        p=incrementPassword(p)
    } while(!isValidPassword(p))
    return p
}
END{
    password=findNextPassword(password)
    password=findNextPassword(password)
    print password
}