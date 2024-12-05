with open('input.txt') as f:
    grid=[l.strip() for l in f]

r=len(grid)
c=len(grid[0])

def check(d):
    return d=="MAS" or d=="SAM"

count=0
for x in range(1,r-1):
    for y in range(1,c-1):
        if grid[x][y]=="A":
            d1=grid[x-1][y-1]+grid[x][y]+grid[x+1][y+1]
            d2=grid[x-1][y+1]+grid[x][y]+grid[x+1][y-1]
            if check(d1) and check(d2):
                count+=1

print(count)
