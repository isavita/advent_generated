
#!/usr/bin/awk -f

function solve(n,    l,r,o){
    if (hasVal[n]) return val[n]
    if (left[n] && right[n]) {
        l=solve(left[n]); r=solve(right[n])
        if (l!="" && r!="") {
            o=op[n]
            if (o=="+") return l+r
            if (o=="-") return l-r
            if (o=="*") return l*r
            if (o=="/") return int(l/r)
            if (o=="==") return (l==r?0:1)
        }
    }
    return ""
}
function expect(n,t,    l,r,o){
    if (n=="humn") return t
    if (left[n] && right[n]) {
        l=solve(left[n]); r=solve(right[n]); o=op[n]
        if (l=="") {
            if (o=="+") return expect(left[n],t-r)
            if (o=="-") return expect(left[n],t+r)
            if (o=="*") return expect(left[n],int(t/r))
            if (o=="/") return expect(left[n],t*r)
            if (o=="==") return expect(left[n],r)
        }
        if (r=="") {
            if (o=="+") return expect(right[n],t-l)
            if (o=="-") return expect(right[n],l-t)
            if (o=="*") return expect(right[n],int(t/l))
            if (o=="/") return expect(right[n],int(l/t))
            if (o=="==") return expect(right[n],l)
        }
    }
}

BEGIN{
    while ((getline line < "input.txt") > 0) {
        split(line, p, ": ")
        name=p[1]; rest=p[2]
        if (rest ~ /^[0-9]+$/) { val[name]=rest+0; hasVal[name]=1 }
        else {
            split(rest, t, " ")
            left[name]=t[1]; op[name]=t[2]; right[name]=t[3]
        }
    }
    hasVal["humn"]=0
    op["root"]="=="
    print expect("root",0)
}
