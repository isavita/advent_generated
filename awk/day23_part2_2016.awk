#!/usr/bin/awk -f
BEGIN{
    n=0
    while ((getline line < "input.txt") > 0) instr[n++] = line
    close("input.txt")
    regs["a"]=12; regs["b"]=0; regs["c"]=0; regs["d"]=0
    i=0
    while (i < n) {
        if (i+5 < n) {
            split(instr[i], p0, " ")
            split(instr[i+1], p1, " ")
            split(instr[i+2], p2, " ")
            split(instr[i+3], p3, " ")
            split(instr[i+4], p4, " ")
            split(instr[i+5], p5, " ")
            if (p0[1]=="cpy" && p1[1]=="inc" && p2[1]=="dec" && p3[1]=="jnz" && p4[1]=="dec" && p5[1]=="jnz") {
                inc_a=p1[2]; dec_c=p2[2]; jnz_c=p3[2]; jnz_c_off=p3[3]
                dec_d=p4[2]; jnz_d=p5[2]; jnz_d_off=p5[3]
                cpy_x=p0[2]; cpy_y=p0[3]
                if (inc_a=="a" && dec_c==cpy_y && jnz_c==cpy_y && jnz_c_off==-2 && dec_d=="d" && jnz_d=="d" && jnz_d_off==-5) {
                    x = (cpy_x ~ /^[abcd]$/) ? regs[cpy_x] : cpy_x+0
                    regs["a"] += x * regs["d"]
                    regs[cpy_y]=0
                    regs["d"]=0
                    i+=6
                    continue
                }
            }
        }
        split(instr[i], parts, " ")
        cmd=parts[1]
        if (cmd=="tgl") {
            x = (parts[2] ~ /^[abcd]$/) ? regs[parts[2]] : parts[2]+0
            target=i+x
            if (target>=0 && target<n) {
                nf=split(instr[target], tp, " ")
                if (nf==2) tp[1]=(tp[1]=="inc")?"dec":(tp[1]=="dec")?"inc":tp[1]
                else if (nf==3) tp[1]=(tp[1]=="jnz")?"cpy":(tp[1]=="cpy")?"jnz":tp[1]
                instr[target]=tp[1]" "tp[2]" "tp[3]
            }
            i++
        } else if (cmd=="cpy") {
            x=parts[2]; y=parts[3]
            if (y ~ /^[abcd]$/) {
                val = (x ~ /^[abcd]$/) ? regs[x] : x+0
                regs[y]=val
            }
            i++
        } else if (cmd=="inc") {
            x=parts[2]
            if (x ~ /^[abcd]$/) regs[x]++
            i++
        } else if (cmd=="dec") {
            x=parts[2]
            if (x ~ /^[abcd]$/) regs[x]--
            i++
        } else if (cmd=="jnz") {
            x=parts[2]; y=parts[3]
            xv = (x ~ /^[abcd]$/) ? regs[x] : x+0
            yv = (y ~ /^[abcd]$/) ? regs[y] : y+0
            if (xv!=0) i+=yv
            else i++
        } else i++
    }
    print regs["a"]
    exit
}