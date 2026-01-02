
local function read_input()
    local f = io.open("input.txt", "r")
    if not f then os.exit(1) end
    local pts, xs, ys = {}, {}, {}
    for line in f:lines() do
        line = line:gsub(",", " ")
        local x, y = line:match("^(%-?%d+)%s+(%-?%d+)$")
        if x then
            x = tonumber(x); y = tonumber(y)
            pts[#pts + 1] = {x, y}
            xs[#xs + 1] = x
            ys[#ys + 1] = y
        end
    end
    f:close()
    return pts, xs, ys
end

local function uniq_sort(t)
    table.sort(t)
    local u = {}
    local last
    for _,v in ipairs(t) do
        if v~=last then u[#u+1]=v; last=v end
    end
    return u
end

local pts, xs, ys = read_input()
if #pts==0 then print("Largest valid area: 0") return end

xs = uniq_sort(xs); ys = uniq_sort(ys)
local ux, uy = #xs, #ys
local xidx, yidx = {}, {}
for i,v in ipairs(xs) do xidx[v]=i-1 end
for i,v in ipairs(ys) do yidx[v]=i-1 end

local W, H = 2*ux+1, 2*uy+1
local colW, rowH = {}, {}
colW[0]=1
for i=0,ux-1 do
    colW[2*i+1]=1
    if i+1<ux then
        local gap = xs[i+2]-xs[i+1]-1
        if gap<0 then gap=0 end
        colW[2*i+2]=gap
    else colW[2*i+2]=1 end
end
rowH[0]=1
for i=0,uy-1 do
    rowH[2*i+1]=1
    if i+1<uy then
        local gap = ys[i+2]-ys[i+1]-1
        if gap<0 then gap=0 end
        rowH[2*i+2]=gap
    else rowH[2*i+2]=1 end
end

local grid = {}
for y=0,H-1 do
    grid[y]={}
    for x=0,W-1 do grid[y][x]=0 end
end

local n=#pts
for i=1,n do
    local a=pts[i]
    local b=pts[i% n +1]
    local gx1=2*xidx[a[1]]+1
    local gy1=2*yidx[a[2]]+1
    local gx2=2*xidx[b[1]]+1
    local gy2=2*yidx[b[2]]+1
    if gx1==gx2 then
        local y0, y1 = gy1, gy2
        if y0>y1 then y0,y1=y1,y0 end
        for y=y0,y1 do if rowH[y]>0 then grid[y][gx1]=1 end end
    else
        local x0, x1 = gx1, gx2
        if x0>x1 then x0,x1=x1,x0 end
        for x=x0,x1 do if colW[x]>0 then grid[gy1][x]=1 end end
    end
end

local qx, qy = {}, {}
local qs, qe = 1, 0
grid[0][0]=2
qe=qe+1; qx[qe]=0; qy[qe]=0
local dirs={{0,1},{0,-1},{1,0},{-1,0}}
while qs<=qe do
    local cx,cy=qx[qs],qy[qs]; qs=qs+1
    for _,d in ipairs(dirs) do
        local nx,ny=cx+d[1],cy+d[2]
        if nx>=0 and nx<W and ny>=0 and ny<H and grid[ny][nx]==0 then
            grid[ny][nx]=2
            qe=qe+1; qx[qe]=nx; qy[qe]=ny
        end
    end
end

local P = {}
for y=0,H-1 do
    P[y]={}
    local rowsum=0
    for x=0,W-1 do
        local val = (grid[y][x]~=2) and (colW[x]*rowH[y]) or 0
        rowsum=rowsum+val
        local above = (y>0) and P[y-1][x] or 0
        P[y][x]=rowsum+above
    end
end

local maxArea=0
for i=1,n do
    for j=i,n do
        local w=math.abs(pts[i][1]-pts[j][1])+1
        local h=math.abs(pts[i][2]-pts[j][2])+1
        local area=w*h
        if area>maxArea then
            local gx1=2*xidx[pts[i][1]]+1
            local gy1=2*yidx[pts[i][2]]+1
            local gx2=2*xidx[pts[j][1]]+1
            local gy2=2*yidx[pts[j][2]]+1
            if gx1>gx2 then gx1,gx2=gx2,gx1 end
            if gy1>gy2 then gy1,gy2=gy2,gy1 end
            local total=P[gy2][gx2]
            local left = (gx1>0) and P[gy2][gx1-1] or 0
            local up   = (gy1>0) and P[gy1-1][gx2] or 0
            local diag = (gx1>0 and gy1>0) and P[gy1-1][gx1-1] or 0
            if total-left-up+diag==area then maxArea=area end
        end
    end
end

print("Largest valid area: "..maxArea)
