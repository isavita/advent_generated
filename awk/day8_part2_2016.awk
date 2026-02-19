
#!/usr/bin/awk -f
BEGIN{
    WIDTH=50; HEIGHT=6;
    while ((getline line < "input.txt") > 0) {
        if (line ~ /rect [0-9]+x[0-9]+/) {
            split(line, m, /rect |x/);
            a=m[2]; b=m[3];
            for (y=0; y<b; y++) for (x=0; x<a; x++) screen[y","x]=1;
        } else if (line ~ /rotate row y=[0-9]+ by [0-9]+/) {
            split(line, m, /rotate row y=| by /);
            r=m[2]; s=m[3];
            for (i=0; i<WIDTH; i++) tmp[i]=screen[r","i];
            for (i=0; i<WIDTH; i++) screen[r","((i+s)%WIDTH)]=tmp[i];
        } else if (line ~ /rotate column x=[0-9]+ by [0-9]+/) {
            split(line, m, /rotate column x=| by /);
            c=m[2]; s=m[3];
            for (i=0; i<HEIGHT; i++) tmp[i]=screen[i","c];
            for (i=0; i<HEIGHT; i++) screen[((i+s)%HEIGHT)","c]=tmp[i];
        }
    }
    close("input.txt");
    for (y=0; y<HEIGHT; y++) {
        row="";
        for (x=0; x<WIDTH; x++) row = row ((screen[y","x]) ? "#" : ".");
        print row;
    }
    cnt=0;
    for (k in screen) if (screen[k]) cnt++;
    print "Number of lit pixels: " cnt;
}
