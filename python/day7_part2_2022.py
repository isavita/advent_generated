
import sys
from collections import defaultdict

def main():
    root = [""]
    dirs = defaultdict(int)
    files = defaultdict(int)
    curr = []
    
    with open("input.txt", "r") as file:
        for line in file:
            txt = line.split()
            if txt[0] == "$":
                if txt[1] == "cd":
                    if txt[2] == "/":
                        curr = root
                    elif txt[2] == "..":
                        curr = curr[:-1]
                    else:
                        curr.append(txt[2])
                    dirs["/".join(curr)] = 0
            else:
                if txt[0] != "dir":
                    files["/".join(curr + [txt[1]])] = int(txt[0])
    
    for f, s in files.items():
        path = f.split("/")
        for i in range(1, len(path)):
            dirs["/".join(path[:i])] += s
    
    sorted_sizes = sorted(dirs.values())
    
    total, want = 70000000, 30000000
    available = total - dirs[""]
    print(sorted_sizes[next(x[0] for x in enumerate(sorted_sizes) if x[1] >= want - available)])

if __name__ == "__main__":
    main()
