
BEGIN{
    x[0]=1; cnt=0
    while((getline line < "input.txt")>0){
        if(line=="noop"){
            cnt++; x[cnt]=x[cnt-1]
        }else{
            split(line,a," "); n=a[2]+0
            cnt++; x[cnt]=x[cnt-1]
            cnt++; x[cnt]=x[cnt-1]+n
        }
    }
}
function abs(v){return v<0?-v:v}
END{
    for(y=0;y<6;y++){
        for(col=0;col<40;col++){
            idx=y*40+col
            printf ( (idx in x) && abs(col-x[idx])<=1 ? "#" : "." )
        }
        print ""
    }
}
