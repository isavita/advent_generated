
Mirror = Struct.new(:rows, :cols)

def parse_input(input)
  mirrors = []

  mirror_str = []
  input.each do |line|
    if line == ""
      mirrors << parse_mirror(mirror_str)
      mirror_str = []
    else
      mirror_str << line
    end
  end
  mirrors << parse_mirror(mirror_str)

  mirrors
end

def parse_mirror(mirror_str)
  mirror = Mirror.new(Array.new(mirror_str.length, 0), Array.new(mirror_str[0].length, 0))

  mirror_str.each_with_index do |line, y|
    line.each_char.with_index do |char, x|
      mirror.rows[y] <<= 1
      mirror.cols[x] <<= 1
      if char == '#'
        mirror.rows[y] += 1
        mirror.cols[x] += 1
      end
    end
  end

  mirror
end

def get_mirror_axis(lines)
  (1...lines.length).each do |i|
    is_mirror = true

    (0...[i, lines.length - i].min).each do |j|
      if lines[i - 1 - j] != lines[i + j]
        is_mirror = false
      end
    end

    return i if is_mirror
  end

  0
end

def get_mirror_axis_with_one_smudge(lines)
  (1...lines.length).each do |i|
    is_mirror = true
    num_smudges = 0

    (0...[i, lines.length - i].min).each do |j|
      if lines[i - 1 - j] != lines[i + j]
        if num_smudges > 0
          is_mirror = false
        else
          dif = lines[i - 1 - j] ^ lines[i + j]
          is_only_one_smudge = (dif & (dif - 1)) == 0
          if is_only_one_smudge
            num_smudges += 1
          else
            is_mirror = false
          end
        end
      end
    end

    return i if is_mirror && num_smudges == 1
  end

  0
end

def solve(input)
  mirrors = parse_input(input)

  res = 0
  mirrors.each do |mirror|
    res += get_mirror_axis(mirror.cols)
    res += get_mirror_axis(mirror.rows) * 100
  end
  res
end

def read_file(file_name)
  File.readlines(file_name).map(&:chomp)
end

input = read_file("input.txt")
puts solve(input)
