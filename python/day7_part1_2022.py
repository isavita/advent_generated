class File:
    def __init__(self, size):
        self.size = size

class Directory:
    def __init__(self):
        self.files = {}
        self.directories = {}

    def total_size(self):
        size = 0
        for f in self.files.values():
            size += f.size
        for d in self.directories.values():
            size += d.total_size()
        return size

def main():
    with open("input.txt", "r") as file:
        root = Directory()
        current_dir = root
        directory_stack = [root]

        for line in file:
            line = line.strip()
            if line.startswith("$ cd"):
                path = line[5:]
                if path == "/":
                    current_dir = root
                    directory_stack = [root]
                elif path == "..":
                    directory_stack.pop()
                    current_dir = directory_stack[-1]
                else:
                    if path not in current_dir.directories:
                        current_dir.directories[path] = Directory()
                    current_dir = current_dir.directories[path]
                    directory_stack.append(current_dir)
            elif line.startswith("dir"):
                dir_name = line[4:]
                current_dir.directories[dir_name] = Directory()
            else:
                try:
                    size, name = line.split()
                    current_dir.files[name] = File(int(size))
                except ValueError:
                    # Ignore lines that do not represent a file size
                    pass

    sum_sizes = 0

    def calculate_sizes(directory):
        nonlocal sum_sizes
        dir_size = directory.total_size()
        if dir_size <= 100000:
            sum_sizes += dir_size
        for dir in directory.directories.values():
            calculate_sizes(dir)

    calculate_sizes(root)
    print(sum_sizes)

if __name__ == "__main__":
    main()