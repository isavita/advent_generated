
#!/bin/bash
awk '{a[n++]=$1}END{s=0;v[0]=1;while(1)for(i=0;i<n;i++){s+=a[i];if(v[s]++){print s;exit}}}' input.txt
