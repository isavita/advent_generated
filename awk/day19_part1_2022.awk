
function max(a,b){return a>b?a:b}
function dfs(t,r1,r2,r3,o1,o2,o3,  k,r){
    if(t<=1)return 0
    if(o1>t*m_or)o1=t*m_or
    if(o2>t*c_ob_c)o2=t*c_ob_c
    if(o3>t*c_ge_ob)o3=t*c_ge_ob
    k=t" "r1" "r2" "r3" "o1" "o2" "o3
    if(k in memo)return memo[k]
    if(o1>=c_ge_or&&o3>=c_ge_ob)
        r=(t-1)+dfs(t-1,r1,r2,r3,o1+r1-c_ge_or,o2+r2,o3+r3-c_ge_ob)
    else{
        r=dfs(t-1,r1,r2,r3,o1+r1,o2+r2,o3+r3)
        if(t>=16&&r1<c_ob_or*2&&o1>=c_or_or)r=max(r,dfs(t-1,r1+1,r2,r3,o1+r1-c_or_or,o2+r2,o3+r3))
        if(t>=8&&r2<c_ob_c&&o1>=c_cl_or)r=max(r,dfs(t-1,r1,r2+1,r3,o1+r1-c_cl_or,o2+r2,o3+r3))
        if(t>=4&&r3<c_ge_ob&&o1>=c_ob_or&&o2>=c_ob_c)r=max(r,dfs(t-1,r1,r2,r3+1,o1+r1-c_ob_or,o2+r2-c_ob_c,o3+r3))
    }
    return memo[k]=r
}
BEGIN{
    while((getline<"input.txt")>0){
        id=$2;sub(/:/,"",id)
        c_or_or=$7;c_cl_or=$13;c_ob_or=$19;c_ob_c=$22;c_ge_or=$28;c_ge_ob=$31
        m_or=c_or_or;if(c_cl_or>m_or)m_or=c_cl_or;if(c_ob_or>m_or)m_or=c_ob_or;if(c_ge_or>m_or)m_or=c_ge_or
        delete memo;ans+=id*dfs(24,1,0,0,0,0,0)
    }
    print ans
}
