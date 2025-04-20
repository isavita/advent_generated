fs=require 'fs'
lines=fs.readFileSync('input.txt','utf8').split '\n'
nodes={}
re=/-x(\d+)-y(\d+)/
for line in lines.slice 2 when line?
  f=line.split /\s+/
  [,x,y]=re.exec f[0]
  nodes["#{x},#{y}"]={used:parseInt f[2],avail:parseInt f[3]}
neighbors=[[0,1],[0,-1],[1,0],[-1,0]]
dim=(ns)->
  ks=Object.keys ns
  xs=ks.map (k)-> +k.split(',')[0]
  ys=ks.map (k)-> +k.split(',')[1]
  [Math.max.apply(null,xs),Math.max.apply(null,ys)]
moves=(ns,goal,from,to,w,h)->
  q=[from]
  dists={[from]:0}
  while q.length
    p=q.shift()
    return dists[p] if p==to
    [x,y]=p.split(',').map Number
    nd=dists[p]+1
    for [dx,dy] in neighbors
      nx=x+dx; ny=y+dy; k="#{nx},#{ny}"
      continue if nx<0 or ny<0 or nx>w or ny>h or k==goal or ns[k].used>400
      unless dists[k]?
        dists[k]=nd
        q.push k
  0
main= ->
  [w,h]=dim nodes
  goal="#{w},0"
  hole=Object.keys(nodes).find (k)-> nodes[k].used==0
  ans=0
  while goal!="0,0"
    [gx,_]=goal.split(',').map Number
    nxt="#{gx-1},0"
    ans+=moves(nodes,goal,hole,nxt,w,h)
    hole=nxt
    ans+=1
    [goal,hole]=[hole,goal]
  console.log ans
main()