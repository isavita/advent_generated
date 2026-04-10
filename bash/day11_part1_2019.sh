
#!/usr/bin/env bash
awk -F, '
function get_mode(o){return int(mem[ip]/(10^(1+o)))%10}
function get_param(o){m=get_mode(o);a=ip+o;return m==1?mem[a]:mem[mem[a]]}
function get_addr(o){return mem[ip+o]}
function add_input(v){input_q[input_tail++]=v}
function has_input(){return input_head<input_tail}
function read_input(){return input_q[input_head++]}
function add_output(v){output_q[output_tail++]=v}
function clear_output(){output_tail=0}
function run(){clear_output();while(1){op=mem[ip]%100;if(op==1){v1=get_param(1);v2=get_param(2);mem[get_addr(3)]=v1+v2;ip+=4}
else if(op==2){v1=get_param(1);v2=get_param(2);mem[get_addr(3)]=v1*v2;ip+=4}
else if(op==3){if(!has_input())return;mem[get_addr(1)]=read_input();ip+=2}
else if(op==4){add_output(get_param(1));ip+=2}
else if(op==5){v=get_param(1);t=get_param(2);ip=v?t:ip+3}
else if(op==6){v=get_param(1);t=get_param(2);ip=v?ip+3:t}
else if(op==7){mem[get_addr(3)]=(get_param(1)<get_param(2))?1:0;ip+=4}
else if(op==8){mem[get_addr(3)]=(get_param(1)==get_param(2))?1:0;ip+=4}
else if(op==99){halted=1;return}
else{halted=1;return}}}
function turn_and_move(d){if(d==0){rd=(rd+3)%4}else{rd=(rd+1)%4}
if(rd==0)ry--;else if(rd==1)rx++;else if(rd==2)ry++;else rx--}
{
if(NR==1){n=split($0,parts,",");for(i=1;i<=n;i++)mem[i-1]=parts[i]+0}
}
END{
ip=0;halted=0;input_head=0;input_tail=0;output_tail=0;rx=0;ry=0;rd=0
while(!halted){
pos=rx","ry;cur=(pos in grid)?grid[pos]:0;add_input(cur);run()
if(!halted){
if(output_tail==2){grid[pos]=output_q[0];turn_and_move(output_q[1])}
else if(output_tail!=0){halted=1}
}
}
cnt=0;for(k in grid)cnt++;print cnt
}' input.txt
