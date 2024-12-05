with open('input.txt') as f:
    lines = [l.strip() for l in f if l.strip()]

rules = []
updates = []
# Separate rules and updates: first section rules with '|', then lines without '|'
i=0
while i<len(lines) and '|' in lines[i]:
    x,y = lines[i].split('|')
    rules.append((int(x),int(y)))
    i+=1

while i<len(lines):
    updates.append([int(x) for x in lines[i].split(',')])
    i+=1

def is_correct(update):
    pos = {p:i for i,p in enumerate(update)}
    for x,y in rules:
        if x in pos and y in pos:
            if pos[x]>pos[y]:
                return False
    return True

s=0
for u in updates:
    if is_correct(u):
        s+=u[len(u)//2]
print(s)
