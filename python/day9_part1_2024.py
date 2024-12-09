
def solve():
    with open("input.txt", "r") as f:
        line = f.readline().strip()

    disk = []
    file_id = 0
    is_file = True
    for char in line:
        length = int(char)
        if is_file:
            disk.extend([str(file_id)] * length)
            file_id += 1
        else:
            disk.extend(['.'] * length)
        is_file = not is_file

    while True:
        lfree = -1
        for i, val in enumerate(disk):
            if val == '.':
                lfree = i
                break
        if lfree == -1:
            break
        rfile = -1
        for i in range(len(disk) - 1, lfree, -1):
            if disk[i] != '.':
                rfile = i
                break
        if rfile == -1:
            break
        disk[lfree], disk[rfile] = disk[rfile], '.'

    checksum = 0
    for i, val in enumerate(disk):
        if val != '.':
            checksum += i * int(val)
    print(checksum)

solve()
