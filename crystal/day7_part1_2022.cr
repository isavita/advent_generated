# Read input from file
input = File.read_lines("input.txt")

# Initialize variables
dir_sizes = Hash(String, Int64).new
current_dir = "/"
total_size = 0

# Traverse the filesystem
input.each do |line|
  case line
  when /^\$ cd (.+)$/
    dir = $1
    if dir == "/"
      current_dir = "/"
    elsif dir == ".."
      current_dir = current_dir.rpartition("/")[0] || "/"
    else
      current_dir = "#{current_dir}/#{dir}"
    end
  when /^\$ ls$/
    # Do nothing
  when /^dir (.+)$/
    # Do nothing
  when /^(\d+) (.+)$/
    size = $1.to_i64
    file = $2
    dir_path = current_dir
    while dir_path != "/"
      dir_sizes[dir_path] = (dir_sizes[dir_path]? || 0_i64) + size
      dir_path = dir_path.rpartition("/")[0] || "/"
    end
    dir_sizes["/"] = (dir_sizes["/"]? || 0_i64) + size
  end
end

# Calculate the sum of the total sizes of directories with size <= 100000
dir_sizes.each_value do |size|
  total_size += size if size <= 100_000_i64
end

puts total_size