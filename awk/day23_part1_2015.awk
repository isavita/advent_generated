#!/usr/bin/awk -f
BEGIN{
    while ((getline line < "input.txt") > 0) instr[++n]=line
    close("input.txt")
    reg["a"]=0; reg["b"]=0
    pc=1
    while (pc<=n){
        split(instr[pc], p, " ")
        op=p[1]
        if (op=="hlf") reg[p[2]]/=2
        else if (op=="tpl") reg[p[2]]*=3
        else if (op=="inc") reg[p[2]]++
        else if (op=="jmp") { pc+=p[2]; continue }
        else if (op=="jie"){
            r=substr(p[2],1,1)
            if (reg[r]%2==0){ pc+=p[3]; continue }
        }
        else if (op=="jio"){
            r=substr(p[2],1,1)
            if (reg[r]==1){ pc+=p[3]; continue }
        }
        pc++
    }
    print reg["b"]
}