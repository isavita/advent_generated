with open('input.txt') as f:
    grid=[l.strip() for l in f]

r=len(grid)
c=len(grid[0])
word="XMAS"
L=len(word)
dirs=[(1,0),(-1,0),(0,1),(0,-1),(1,1),(1,-1),(-1,1),(-1,-1)]

count=0
for i in range(r):
    for j in range(c):
        if grid[i][j]==word[0]:
            for dx,dy in dirs:
                x,y=i, j
                k=0
                while 0<=x<r and 0<=y<c and k<L and grid[x][y]==word[k]:
                    x+=dx
                    y+=dy
                    k+=1
                if k==L:
                    count+=1

print(count)
