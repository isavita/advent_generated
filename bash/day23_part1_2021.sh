
#!/bin/bash
awk '
function abs(x){return x<0?-x:x}
function move(s,i,j,a,b,t){if(i>j){t=i;i=j;j=t}a=substr(s,i,1);b=substr(s,j,1);return substr(s,1,i-1)b substr(s,i+1,j-i-1)a substr(s,j+1)}
function pc(h1,h2,s,i,st){st=(h1<h2?1:-1);for(i=h1;;i+=st){if(substr(s,i,1)!=".")return 0;if(i==h2)break}return 1}
function push(v,s,i,p,t){if(s in d&&v>=d[s])return;d[s]=v;hv[++hn]=v;hs[hn]=s;i=hn;while(i>1&&hv[i]<hv[p=int(i/2)]){t=hv[i];hv[i]=hv[p];hv[p]=t;t=hs[i];hs[i]=hs[p];hs[p]=t;i=p}}
function pop(ts,i,c,v,sv){ts=hs[1];dv=hv[1];hv[1]=hv[hn];hs[1]=hs[hn--];i=1;while((c=2*i)<=hn){if(c<hn&&hv[c+1]<hv[c])c++;if(hv[i]<=hv[c])break;v=hv[i];hv[i]=hv[c];hv[c]=v;sv=hs[i];hs[i]=hs[c];hs[c]=sv;i=c}return ts}
BEGIN{E["A"]=1;E["B"]=10;E["C"]=100;E["D"]=1000;for(i=12;i<=13;i++)C[i]="A";for(i=14;i<=15;i++)C[i]="B";for(i=16;i<=17;i++)C[i]="C";for(i=18;i<=19;i++)C[i]="D";for(i=12;i<=19;i++)D[i]=2*(int((i-12)/2))+3;T="...........AABBCCDD"}
NR==3{r12=substr($0,4,1);r14=substr($0,6,1);r16=substr($0,8,1);r18=substr($0,10,1)}
NR==4{r13=substr($0,4,1);r15=substr($0,6,1);r17=substr($0,8,1);r19=substr($0,10,1)}
END{s="..........."r12 r13 r14 r15 r16 r17 r18 r19;push(0,s);while(hn>0){s=pop();u=dv;if(u>d[s])continue;if(s==T){print u;exit}for(h=1;h<=11;h++){char=substr(s,h,1);if(char==".")continue;rt=(char=="A"?12:char=="B"?14:char=="C"?16:18);rb=rt+1;door=D[rt];if(pc(h+(h<door?1:-1),door,s)){if(substr(s,rt,1)=="."&&substr(s,rb,1)==".")push(u+(abs(door-h)+2)*E[char],move(s,h,rb));else if(substr(s,rt,1)=="."&&substr(s,rb,1)==char)push(u+(abs(door-h)+1)*E[char],move(s,h,rt))}}for(i=12;i<=19;i++){char=substr(s,i,1);if(char==".")continue;if(i%2==1){if(char==C[i])continue}else if(char==C[i]&&substr(s,i+1,1)==C[i])continue;if(i%2==1&&substr(s,i-1,1)!=".")continue;door=D[i];sh=(i%2==0?1:2);for(h=1;h<=11;h++){if(h==3||h==5||h==7||h==9)continue;if(pc(door,h,s))push(u+(sh+abs(door-h))*E[char],move(s,i,h))}}}}' input.txt
