
#!/usr/bin/env bash
#  solve.sh – reads input.txt and prints the required sum

awk '
BEGIN{ RS="" }                     # paragraph mode (blank line separates sections)

# ---------- 1st paragraph – workflows ----------
NR==1{
    split($0, lines, "\n")
    for(i in lines){
        if(lines[i] ~ /^[^{}]+{.*}$/){
            name = substr(lines[i],1,index(lines[i],"{")-1)
            rules_str = substr(lines[i],index(lines[i],"{")+1,
                                length(lines[i])-index(lines[i],"{")-1)
            split(rules_str, rules, ",")
            wf_cnt[name] = length(rules)
            for(j=1;j<=wf_cnt[name];j++) wf[name,j] = rules[j]
        }
    }
}

# ---------- 2nd paragraph – parts ----------
NR==2{
    split($0, lines, "\n")
    for(i in lines){
        if(lines[i] ~ /{.*}/){
            gsub(/[{}]/,"",lines[i])
            split(lines[i], kv, ",")
            for(k in kv){
                split(kv[k], kvp, "=")
                part[kvp[1]] = kvp[2] + 0
            }

            # evaluate this part
            name="in"
            while(1){
                n = wf_cnt[name]
                matched=0
                for(j=1;j<=n;j++){
                    r = wf[name,j]
                    if(index(r,":")>0){
                        split(r, arr, ":")
                        cond = arr[1]
                        dest = arr[2]
                        cat  = substr(cond,1,1)
                        op   = substr(cond,2,1)
                        val  = substr(cond,3)+0
                        if(op=="<" && part[cat] < val){ name=dest; matched=1; break }
                        if(op==">" && part[cat] > val){ name=dest; matched=1; break }
                    }else{
                        name = r
                        matched=1
                        break
                    }
                }
                if(!matched) break
                if(name=="A"){ sum += part["x"]+part["m"]+part["a"]+part["s"]; break }
                if(name=="R") break
            }
        }
    }
}

END{ print sum }
' input.txt
