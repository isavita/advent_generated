#!/usr/bin/awk -f
BEGIN{
    while((getline line < "input.txt")>0) instr[++n]=line
    close("input.txt")
    pw="fbgdceah"
    for(i=n;i>=1;i--){
        split(instr[i],a," ")
        if(a[1]=="swap"){
            if(a[2]=="position") swapPositions(a[3]+0,a[6]+0)
            else swapLetters(a[3],a[6])
        }else if(a[1]=="rotate"){
            if(a[2]=="based") derotateLetter(a[7])
            else{
                steps=a[3]+0
                if(a[2]=="left") steps=-steps
                steps=-steps
                rotate(steps)
            }
        }else if(a[1]=="reverse"){
            reverse(a[3]+0,a[5]+0)
        }else if(a[1]=="move"){
            x=a[3]+0; y=a[6]+0
            tmp=x; x=y; y=tmp
            move(x,y)
        }
    }
    print pw
}
function index0(s,ch,   pos){pos=index(s,ch);return pos?pos-1:-1}
function swapPositions(x,y,   tmp,pre,mid,post){
    if(x==y) return
    if(x>y){t=x;x=y;y=t}
    pre=substr(pw,1,x)
    mid=substr(pw,x+2,y-x-1)
    post=substr(pw,y+2)
    pw=pre substr(pw,y+1,1) mid substr(pw,x+1,1) post
}
function swapLetters(c1,c2,   i1,i2){
    i1=index0(pw,c1)
    i2=index0(pw,c2)
    swapPositions(i1,i2)
}
function rotate(steps,   len,s){
    len=length(pw)
    s=((steps%len)+len)%len
    if(s==0) return
    pw=substr(pw,len-s+1) substr(pw,1,len-s)
}
function rotateLetter(ch,   idx,steps){
    idx=index0(pw,ch)
    steps=(idx>=4)?idx+2:idx+1
    rotate(steps)
}
function derotateLetter(ch,   idx,rot){
    idx=index0(pw,ch)
    if(idx%2==1) rot=-(idx+1)/2
    else if(idx!=0) rot=(6-idx)/2
    else rot=-1
    rotate(rot)
}
function reverse(x,y,   pre,mid,post,rev,i){
    pre=substr(pw,1,x)
    mid=substr(pw,x+1,y-x+1)
    post=substr(pw,y+2)
    rev=""
    for(i=length(mid);i>=1;i--) rev=rev substr(mid,i,1)
    pw=pre rev post
}
function move(x,y,   ch,pre,mid,post){
    ch=substr(pw,x+1,1)
    if(x<y){
        pre=substr(pw,1,x)
        mid=substr(pw,x+2,y-x)
        post=substr(pw,y+2)
        pw=pre mid ch post
    }else{
        pre=substr(pw,1,y)
        mid=substr(pw,y+1,x-y)
        post=substr(pw,x+2)
        pw=pre ch mid post
    }
}