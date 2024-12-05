with open('input.txt') as f:
    lines=[l.strip() for l in f if l.strip()]

rules=[]
updates=[]
i=0
while i<len(lines) and '|' in lines[i]:
    x,y = lines[i].split('|')
    rules.append((int(x),int(y)))
    i+=1

while i<len(lines):
    updates.append([int(x) for x in lines[i].split(',')])
    i+=1

def is_correct(update, rules):
    pos={p:i for i,p in enumerate(update)}
    for x,y in rules:
        if x in pos and y in pos and pos[x]>pos[y]:
            return False
    return True

def correct_order(update, rules):
    # Extract relevant rules and build a graph
    pages=set(update)
    adj={p:[] for p in update}
    indeg={p:0 for p in update}
    for x,y in rules:
        if x in pages and y in pages:
            adj[x].append(y)
            indeg[y]+=1
    # Topological sort
    q=[p for p in update if indeg[p]==0]
    out=[]
    while q:
        n=q.pop()
        out.append(n)
        for nxt in adj[n]:
            indeg[nxt]-=1
            if indeg[nxt]==0:
                q.append(nxt)
    return out

incorrect=[]
for u in updates:
    if not is_correct(u, rules):
        fixed=correct_order(u, rules)
        incorrect.append(fixed[len(fixed)//2])
print(sum(incorrect))
