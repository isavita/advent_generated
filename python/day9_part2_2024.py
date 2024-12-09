class FileSegment:
    def __init__(self, id, start, end):
        self.id = id
        self.start = start
        self.end = end

def solve():
    with open("input.txt", "r") as f:
        line = f.readline().strip()

    # Create initial disk layout
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

    # Build file segments list
    files = []
    curr_id = None
    start = 0
    
    for i, val in enumerate(disk):
        if val == '.':
            curr_id = None
            continue
        
        file_id = int(val)
        if file_id != curr_id:
            curr_id = file_id
            start = i
            
        if i == len(disk) - 1 or (i + 1 < len(disk) and disk[i + 1] != val):
            files.append(FileSegment(file_id, start, i))

    # Process files from highest ID to lowest
    for file in reversed(files):
        file_len = file.end - file.start + 1
        leftmost_span = -1
        span_len = 0
        
        # Find leftmost suitable free space
        for i in range(file.start):
            if disk[i] == '.':
                if span_len == 0:
                    leftmost_span = i
                span_len += 1
                if span_len == file_len:
                    break
            else:
                span_len = 0
                leftmost_span = -1

        # Move file if we found suitable space
        if leftmost_span != -1 and span_len == file_len:
            # Clear old location
            for i in range(file.start, file.end + 1):
                disk[i] = '.'
            # Copy file to new location
            for i in range(file_len):
                disk[leftmost_span + i] = str(file.id)

    # Calculate checksum
    checksum = 0
    for i, val in enumerate(disk):
        if val != '.':
            checksum += i * int(val)
    print(checksum)

solve()
