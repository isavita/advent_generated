
#!/usr/bin/env crystal
require "json"

alias LL = Int64

class Queue
  @data = [] of LL
  def initialize; end
  def <<(v : LL) @data << v end
  def pop : LL
    raise "underflow" if empty?
    @data.shift
  end
  def empty? : Bool; @data.empty?; end
  def size : Int32; @data.size; end
  def each(&block : LL ->) ; @data.each { |v| block.call(v) } ; end
end

class Intcode
  @mem = [] of LL
  @ptr = 0_i64
  @rel = 0_i64
  @in  = Queue.new
  @out = Queue.new
  @halted = false
  @waiting = false

  getter :out, :halted, :waiting, :in

  def initialize(prog : Array(LL))
    @mem = prog.dup
    @mem.concat(Array.new(10_000) { 0_i64 })
  end

  def mem(idx : Int64) : LL
    raise "neg addr" if idx < 0
    @mem[idx.to_i] = 0_i64 if idx >= @mem.size
    @mem[idx.to_i]
  end

  def mem_set(idx : Int64, v : LL)
    raise "neg addr" if idx < 0
    @mem[idx.to_i] = v if idx >= @mem.size
    @mem[idx.to_i] = v
  end

  def get_param(mode : Int32, addr : Int64) : LL
    case mode
    when 0 then mem(mem(addr))
    when 1 then mem(addr)
    when 2 then mem(@rel + mem(addr))
    else raise "mode"
    end
  end

  def set_param(mode : Int32, addr : Int64, v : LL)
    case mode
    when 0 then mem_set(mem(addr), v)
    when 2 then mem_set(@rel + mem(addr), v)
    else raise "write mode"
    end
  end

  def enqueue_input(v : LL) ; @in << v ; end

  def run
    @waiting = false
    while !@halted && !@waiting
      instr = mem(@ptr)
      op = (instr % 100).to_i
      modes = [ (instr/100).to_i % 10, (instr/1000).to_i % 10, (instr/10000).to_i % 10 ]

      case op
      when 1
        a = get_param(modes[0], @ptr+1)
        b = get_param(modes[1], @ptr+2)
        set_param(modes[2], @ptr+3, a+b)
        @ptr += 4
      when 2
        a = get_param(modes[0], @ptr+1)
        b = get_param(modes[1], @ptr+2)
        set_param(modes[2], @ptr+3, a*b)
        @ptr += 4
      when 3
        if @in.empty?
          @waiting = true
        else
          v = @in.pop
          set_param(modes[0], @ptr+1, v)
          @ptr += 2
        end
      when 4
        v = get_param(modes[0], @ptr+1)
        @out << v
        @ptr += 2
      when 5
        if get_param(modes[0], @ptr+1) != 0
          @ptr = get_param(modes[1], @ptr+2)
        else
          @ptr += 3
        end
      when 6
        if get_param(modes[0], @ptr+1) == 0
          @ptr = get_param(modes[1], @ptr+2)
        else
          @ptr += 3
        end
      when 7
        set_param(modes[2], @ptr+3,
          get_param(modes[0], @ptr+1) < get_param(modes[1], @ptr+2) ? 1_i64 : 0_i64)
        @ptr += 4
      when 8
        set_param(modes[2], @ptr+3,
          get_param(modes[0], @ptr+1) == get_param(modes[1], @ptr+2) ? 1_i64 : 0_i64)
        @ptr += 4
      when 9
        @rel += get_param(modes[0], @ptr+1)
        @ptr += 2
      when 99
        @halted = true
      else
        raise "bad opcode #{op}"
      end
    end
  end
end

def read_program : Array(LL)
  File.read("input.txt").split(',').map(&.to_i64)
end

def parse_map(outputs : Queue) : Array(Array(Char))
  grid = [] of Array(Char)
  row = [] of Char
  outputs.each do |v|
    if v == 10
      unless row.empty?
        grid << row
        row = [] of Char
      end
    else
      row << v.chr
    end
  end
  grid << row unless row.empty?
  grid
end

def alignment_sum(grid : Array(Array(Char))) : LL
  sum = 0_i64
  (1...grid.size-1).each do |r|
    (1...grid[0].size-1).each do |c|
      if grid[r][c] == '#' &&
         grid[r-1][c] == '#' && grid[r+1][c] == '#' &&
         grid[r][c-1] == '#' && grid[r][c+1] == '#'
        sum += (r * c).to_i64
      end
    end
  end
  sum
end

def find_robot(grid) : {Int32,Int32,Char}
  grid.each_with_index do |row,y|
    row.each_with_index do |ch,x|
      return {x,y,ch} if "^v<>".includes?(ch)
    end
  end
  raise "no robot"
end

def turn_left(d) : Char
  case d when '^' then '<' when '<' then 'v' when 'v' then '>' else '^' end
end
def turn_right(d) : Char
  case d when '^' then '>' when '>' then 'v' when 'v' then '<' else '^' end
end
def move(x,y,d) : {Int32,Int32}
  case d
  when '^' then {x, y-1}
  when 'v' then {x, y+1}
  when '<' then {x-1, y}
  else {x+1, y}
  end
end

def scaffold?(grid,x,y) : Bool
  y >= 0 && y < grid.size && x >= 0 && x < grid[0].size && grid[y][x] == '#'
end

def build_path(grid, start_x, start_y, start_dir) : Array(String)
  tokens = [] of String
  x = start_x; y = start_y; dir = start_dir; steps = 0
  loop do
    nx, ny = move(x,y,dir)
    if scaffold?(grid,nx,ny)
      steps += 1
      x = nx; y = ny
    else
      tokens << steps.to_s if steps > 0
      steps = 0
      ld = turn_left(dir)
      lx, ly = move(x,y,ld)
      if scaffold?(grid,lx,ly)
        tokens << "L"
        dir = ld
        next
      end
      rd = turn_right(dir)
      rx, ry = move(x,y,rd)
      if scaffold?(grid,rx,ry)
        tokens << "R"
        dir = rd
        next
      end
      break
    end
  end
  tokens
end

def join(tokens : Array(String), start : Int32, len : Int32) : String
  tokens[start, len].join(',')
end

def match?(tokens : Array(String), i : Int32, pat : Array(String)) : Bool
  return false if i + pat.size > tokens.size
  (0...pat.size).all? { |k| tokens[i+k] == pat[k] }
end

def compress(tokens : Array(String), max_len = 20) : {String,String,String,String}?
  (1..10).each do |a_len|
    a = join(tokens,0,a_len)
    next if a.bytesize > max_len
    b_start = 0
    while b_start < tokens.size && match?(tokens,b_start,tokens[0,a_len])
      b_start += a_len
    end
    next if b_start >= tokens.size
    (1..10).each do |b_len|
      b = join(tokens,b_start,b_len)
      next if b.bytesize > max_len
      c_start = 0
      while c_start < tokens.size
        if match?(tokens,c_start,tokens[0,a_len])
          c_start += a_len
        elsif match?(tokens,c_start,tokens[b_start,b_len])
          c_start += b_len
        else
          break
        end
      end
      next if c_start >= tokens.size
      (1..10).each do |c_len|
        c = join(tokens,c_start,c_len)
        next if c.bytesize > max_len
        main = [] of String
        pos = 0
        while pos < tokens.size
          if match?(tokens,pos,tokens[0,a_len])
            main << "A"; pos += a_len
          elsif match?(tokens,pos,tokens[b_start,b_len])
            main << "B"; pos += b_len
          elsif match?(tokens,pos,tokens[c_start,c_len])
            main << "C"; pos += c_len
          else
            break
          end
        end
        next unless pos == tokens.size
        main_str = main.join(',')
        next if main_str.bytesize > max_len
        return {main_str,a,b,c}
      end
    end
  end
  nil
end

prog = read_program

# Part 1
comp1 = Intcode.new(prog)
comp1.run
grid = parse_map(comp1.out)
puts "Part One: #{alignment_sum(grid)}"

# Part 2
prog[0] = 2_i64
comp2 = Intcode.new(prog)
rx, ry, rdir = find_robot(grid)
tokens = build_path(grid, rx, ry, rdir)
compressed = compress(tokens) || raise "no compression"
main, a, b, c = compressed
[main,a,b,c,"n"].each do |s|
  s.each_char { |ch| comp2.enqueue_input(ch.ord.to_i64) }
  comp2.enqueue_input(10_i64)
end
comp2.run
while !comp2.out.empty?
  dust = comp2.out.pop
end
puts "Part Two: #{dust}"
