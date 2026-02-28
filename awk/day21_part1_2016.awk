
#!/usr/bin/awk -f
BEGIN{
    password="abcdefgh"
    while ((getline line < "input.txt") > 0) {
        split(line, f, " ")
        if (f[1]=="swap") {
            if (f[2]=="position")
                password=swap_position(password, f[3]+0, f[6]+0)
            else
                password=swap_letter(password, f[3], f[6])
        } else if (f[1]=="rotate") {
            if (f[2]=="left")
                password=rotate_left(password, f[3]+0)
            else if (f[2]=="right")
                password=rotate_right(password, f[3]+0)
            else
                password=rotate_based(password, f[7])
        } else if (f[1]=="reverse")
            password=reverse_positions(password, f[3]+0, f[5]+0)
        else if (f[1]=="move")
            password=move_position(password, f[3]+0, f[6]+0)
    }
    close("input.txt")
    print password
}
function split_chars(s, a,    n){n=split(s,a,"");return n}
function join(a, n,    i,r){r="";for(i=1;i<=n;i++)r=r a[i];return r}
function swap_position(p,x,y,    a,n,t){n=split_chars(p,a);t=a[x+1];a[x+1]=a[y+1];a[y+1]=t;return join(a,n)}
function swap_letter(p,x,y,    a,n,i){n=split_chars(p,a);for(i=1;i<=n;i++)if(a[i]==x)a[i]=y;else if(a[i]==y)a[i]=x;return join(a,n)}
function rotate_left(p,steps,    n){n=length(p);steps=steps%n;return substr(p,steps+1) substr(p,1,steps)}
function rotate_right(p,steps,    n){n=length(p);steps=steps%n;return substr(p,n-steps+1) substr(p,1,n-steps)}
function rotate_based(p,x,    idx,steps){idx=index(p,x)-1;steps=1+idx;if(idx>=4)steps++;return rotate_right(p,steps)}
function reverse_positions(p,x,y,    a,n,t){n=split_chars(p,a);while(x<y){t=a[x+1];a[x+1]=a[y+1];a[y+1]=t;x++;y--}return join(a,n)}
function move_position(p,x,y,    a,n,chr,i){n=split_chars(p,a);chr=a[x+1];for(i=x+1;i<n;i++)a[i]=a[i+1];n--;for(i=n;i>y;i--)a[i+1]=a[i];a[y+1]=chr;n++;return join(a,n)}
