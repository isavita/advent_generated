
#!/usr/bin/env bash
awk -f - <<'EOF'
BEGIN{
    key_pad[0]="789"
    key_pad[1]="456"
    key_pad[2]="123"
    key_pad[3]=" 0A"
    robot_pad[0]=" ^A"
    robot_pad[1]="<v>"
    MAX_ROBOTS=26
    total=0
    while((getline line<"input.txt")>0){
        sub(/\r?\n$/,"",line)
        if(line=="")continue
        numeric=0
        for(i=1;i<=length(line);i++){
            ch=substr(line,i,1)
            if(ch~/[0-9]/)numeric=numeric*10+(ch+0)
        }
        if(numeric>0)total+=solve(line,MAX_ROBOTS)*numeric
    }
    print total
    exit
}
function solve(code,robots,   key,ret,i,ch,moves,r,c,pad,rows,cols){
    key=code"|"robots
    if(key in memo)return memo[key]
    if(robots<=0)return length(code)
    if(robots==MAX_ROBOTS){
        r=3;c=2;pad="key";rows=4;cols=3
    }else{
        r=0;c=2;pad="robot";rows=2;cols=3
    }
    ret=0
    for(i=1;i<=length(code);i++){
        ch=substr(code,i,1)
        moves=generate_moves(r,c,ch,pad,rows,cols)
        find_position(pad,rows,ch)
        r=find_r;c=find_c
        moves=moves"A"
        ret+=solve(moves,robots-1)
    }
    memo[key]=ret
    return ret
}
function generate_moves(r,c,obj,pad,rows,cols,   obj_r,obj_c,moves,i){
    find_position(pad,rows,obj)
    obj_r=find_r;obj_c=find_c
    moves=""
    if(c>obj_c)for(i=0;i<c-obj_c;i++)moves=moves"<"
    if(r>obj_r)for(i=0;i<r-obj_r;i++)moves=moves"^"
    if(r<obj_r)for(i=0;i<obj_r-r;i++)moves=moves"v"
    if(c<obj_c)for(i=0;i<obj_c-c;i++)moves=moves">"
    if(is_ok(pad,rows,cols,r,c,moves))return moves
    moves=""
    if(c<obj_c)for(i=0;i<obj_c-c;i++)moves=moves">"
    if(r>obj_r)for(i=0;i<r-obj_r;i++)moves=moves"^"
    if(r<obj_r)for(i=0;i<obj_r-r;i++)moves=moves"v"
    if(c>obj_c)for(i=0;i<c-obj_c;i++)moves=moves"<"
    return moves
}
function is_ok(pad,rows,cols,r,c,seq,   i,move,rr,cc,line,ch){
    rr=r;cc=c
    for(i=1;i<=length(seq);i++){
        if(rr<0||rr>=rows||cc<0||cc>=cols)return 0
        line=(pad=="key")?key_pad[rr]:robot_pad[rr]
        ch=substr(line,cc+1,1)
        if(ch==" ")return 0
        move=substr(seq,i,1)
        if(move=="^")rr--
        else if(move=="v")rr++
        else if(move=="<")cc--
        else if(move==">")cc++
    }
    return 1
}
function find_position(pad,rows,ch,   i,pos,line){
    for(i=0;i<rows;i++){
        line=(pad=="key")?key_pad[i]:robot_pad[i]
        pos=index(line,ch)
        if(pos){
            find_r=i
            find_c=pos-1
            return
        }
    }
    find_r=-1;find_c=-1
}
EOF
