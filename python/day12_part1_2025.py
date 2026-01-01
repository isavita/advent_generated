
import sys
from collections import deque

sys.setrecursionlimit(10**6)

def normalize(pts):
    if not pts: return ()
    mr=min(r for r,_ in pts); mc=min(c for _,c in pts)
    norm=sorted(((r-mr,c-mc) for r,c in pts))
    return tuple(norm)

def rotate(pts):
    return tuple((c,-r) for r,c in pts)

def flip(pts):
    return tuple((r,-c) for r,c in pts)

def variations(base):
    s=set()
    cur=base
    for _ in range(4):
        n=normalize(cur); s.add(n)
        s.add(normalize(flip(cur)))
        cur=rotate(cur)
    return list(s)

def can_place(grid, cols, p, r, c):
    for dr,dc in p:
        nr=r+dr; nc=c+dc
        if nr<0 or nr>=len(grid)//cols or nc<0 or nc>=cols: return False
        if grid[nr*cols+nc]: return False
    return True

def place(grid, cols, p, r, c, v):
    for dr,dc in p:
        grid[(r+dr)*cols+(c+dc)]=v

def island_ok(rows, cols, grid, cnt, shapes, slack_idx):
    min_real=10**9
    any_real=False
    for i,c in enumerate(cnt):
        if i!=slack_idx and c>0:
            any_real=True
            min_real=min(min_real, len(shapes[i]))
    if not any_real: return True
    slack=cnt[slack_idx]
    n=rows*cols
    vis=[False]*n
    for i in range(n):
        if grid[i] or vis[i]: continue
        q=deque([i]); vis[i]=True; size=0
        while q:
            cur=q.popleft(); size+=1
            r=cur//cols; c=cur%cols
            for nr,nc in ((r-1,c),(r+1,c),(r,c-1),(r,c+1)):
                if 0<=nr<rows and 0<=nc<cols:
                    idx=nr*cols+nc
                    if not grid[idx] and not vis[idx]:
                        vis[idx]=True; q.append(idx)
        if size<min_real:
            if slack>=size: slack-=size
            else: return False
    return True

def backtrack(rows, cols, grid, cnt, ids, varr, varcnt, slack_idx, shapes):
    try: empty=grid.index(False)
    except ValueError: return True
    r=empty//cols; c=empty%cols
    if not island_ok(rows, cols, grid, cnt, shapes, slack_idx): return False
    for id_ in ids:
        if cnt[id_]==0: continue
        cnt[id_]-=1
        for v in range(varcnt[id_]):
            p=varr[id_][v]
            if can_place(grid, cols, p, r, c):
                place(grid, cols, p, r, c, True)
                if backtrack(rows, cols, grid, cnt, ids, varr, varcnt, slack_idx, shapes): return True
                place(grid, cols, p, r, c, False)
        cnt[id_]+=1
    return False

def main():
    raw=open("input.txt").read().splitlines()
    max_id=-1
    for ln in raw:
        s=ln.strip()
        if s and s.endswith(":"):
            try: id_=int(s[:-1])
            except: continue
            max_id=max(max_id,id_)
    arr_sz=max_id+2
    slack_idx=max_id+1
    shapes=[()] * arr_sz
    cur_id=-1; cur=[]
    region=[]
    parsing=True
    for ln in raw:
        s=ln.strip()
        if not s: continue
        if parsing and 'x' in s and ':' in s:
            parsing=False
        if parsing:
            if s.endswith(":"):
                if cur_id!=-1 and cur:
                    pts=[(r,c) for r,row in enumerate(cur) for c,ch in enumerate(row) if ch=='#']
                    shapes[cur_id]=normalize(pts)
                    cur=[]
                cur_id=int(s[:-1])
            else:
                cur.append(s)
        else:
            region.append(s)
    if cur_id!=-1 and cur:
        pts=[(r,c) for r,row in enumerate(cur) for c,ch in enumerate(row) if ch=='#']
        shapes[cur_id]=normalize(pts)
    shapes[slack_idx]=((0,0),)
    varr=[[] for _ in range(arr_sz)]
    varcnt=[0]*arr_sz
    for i in range(arr_sz):
        if shapes[i]:
            vars=variations(shapes[i])
            varr[i]=vars
            varcnt[i]=len(vars)
    solved=0
    for line in region:
        if ':' not in line: continue
        dims,counts=line.split(':',1)
        w,h=map(int, dims.strip().lower().replace('x',' ').split())
        grid_sz=w*h
        cnt=[0]*arr_sz
        total=0
        for idx,tok in enumerate(counts.split()):
            c=int(tok) if tok.isdigit() else 0
            if c and idx<arr_sz-1:
                cnt[idx]=c
                total+=c*len(shapes[idx])
        if total>grid_sz: continue
        slack=grid_sz-total
        if slack: cnt[slack_idx]=slack
        ids=[i for i,c in enumerate(cnt) if c]
        ids.sort(key=lambda i:-len(shapes[i]))
        grid=[False]*grid_sz
        if backtrack(h,w,grid,cnt,ids,varr,varcnt,slack_idx,shapes):
            solved+=1
    print(f"Number of regions that fit all presents: {solved}")

if __name__=="__main__":
    main()
