class File
  attr_accessor :size, :name

  def initialize(size, name)
    @size = size
    @name = name
  end
end

class Directory
  attr_accessor :files, :directories, :name

  def initialize(name)
    @files = {}
    @directories = {}
    @name = name
  end

  def total_size
    size = 0
    @files.each { |_, file| size += file.size }
    @directories.each { |_, dir| size += dir.total_size }
    size
  end
end

root = Directory.new('/')
current_dir = root
directory_stack = [root]

File.readlines('input.txt').each do |line|
  if line.start_with?('$ cd')
    path = line.strip[5..-1]
    if path == '/'
      current_dir = root
      directory_stack = [root]
    elsif path == '..'
      directory_stack.pop
      current_dir = directory_stack.last
    else
      unless current_dir.directories.key?(path)
        current_dir.directories[path] = Directory.new(path)
      end
      current_dir = current_dir.directories[path]
      directory_stack.push(current_dir)
    end
  elsif line.start_with?('dir')
    dir_name = line.strip[4..-1]
    current_dir.directories[dir_name] = Directory.new(dir_name)
  else
    parts = line.split
    if parts.length == 2
      size = parts[0].to_i
      file_name = parts[1]
      current_dir.files[file_name] = File.new(size, file_name)
    end
  end
end

sum_sizes = 0
calculate_sizes = lambda do |dir|
  dir_size = dir.total_size
  if dir_size <= 100_000
    sum_sizes += dir_size
  end
  dir.directories.each_value { |d| calculate_sizes.call(d) }
end
calculate_sizes.call(root)

puts sum_sizes