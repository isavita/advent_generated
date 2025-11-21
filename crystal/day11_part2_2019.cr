
struct Intcode
  getter halted : Bool
  @memory : Hash(Int64,Int64)
  @ip : Int64
  @base : Int64

  def initialize(code : Array(Int64))
    @memory = Hash(Int64,Int64).new(0_i64)
    code.each_with_index { |v,i| @memory[i.to_i64] = v }
    @ip = 0_i64
    @base = 0_i64
    @halted = false
  end

  def run(&inb : -> Int64) : Array(Int64)
    out = [] of Int64
    while !@halted
      inst = @memory[@ip]
      op   = (inst % 100).to_i
      m1   = (inst // 100 % 10).to_i
      m2   = (inst // 1000 % 10).to_i
      m3   = (inst // 10000 % 10).to_i
      v1 = read_param(m1,1)
      v2 = read_param(m2,2)
      case op
      when 1
        write_param(m3,3, v1 + v2); @ip += 4
      when 2
        write_param(m3,3, v1 * v2); @ip += 4
      when 3
        write_param(m1,1, inb.call); @ip += 2
      when 4
        out << v1; @ip += 2
      when 5
        @ip = v1 != 0 ? v2 : @ip + 3
      when 6
        @ip = v1 == 0 ? v2 : @ip + 3
      when 7
        write_param(m3,3, v1 < v2 ? 1_i64 : 0_i64); @ip += 4
      when 8
        write_param(m3,3, v1 == v2 ? 1_i64 : 0_i64); @ip += 4
      when 9
        @base += v1; @ip += 2
      when 99
        @halted = true
      else
        raise "bad op #{op}"
      end
    end
    out
  end

  private def read_param(mode : Int, off : Int) : Int64
    raw = @memory[@ip + off]
    case mode
    when 0 then @memory[raw]
    when 1 then raw
    when 2 then @memory[@base + raw]
    else        raise "bad rmode"
    end
  end

  private def write_param(mode : Int, off : Int, val : Int64)
    raw = @memory[@ip + off]
    case mode
    when 0 then @memory[raw] = val
    when 2 then @memory[@base + raw] = val
    else        raise "bad wmode"
    end
  end
end

class Robot
  @panels : Hash(Tuple(Int32,Int32),Int32)
  @painted : Set(Tuple(Int32,Int32))
  @dir : Int32
  @pos : Tuple(Int32,Int32)

  def initialize(code : Array(Int64), start : Int32 = 0)
    @ic  = Intcode.new(code)
    @pos = {0,0}
    @dir = 0
    @panels = Hash(Tuple(Int32,Int32),Int32).new(0)
    @painted = Set(Tuple(Int32,Int32)).new
    @panels[@pos] = start
  end

  def run
    while !@ic.halted
      outs = @ic.run { @panels.fetch(@pos,0).to_i64 }
      outs.each_slice(2) do |pair|
        color, turn = pair
        @panels[@pos] = color.to_i32
        @painted << @pos
        @dir = (@dir + (turn == 0 ? -1 : 1)) & 3
        @pos = case @dir
               when 0 then {@pos[0], @pos[1]-1}
               when 1 then {@pos[0]+1, @pos[1]}
               when 2 then {@pos[0], @pos[1]+1}
               when 3 then {@pos[0]-1, @pos[1]}
               else        @pos
               end
      end
    end
  end

  def count
    @painted.size
  end

  def render
    return if @panels.empty?
    xs = @panels.keys.map(&.[0])
    ys = @panels.keys.map(&.[1])
    (ys.min..ys.max).each do |y|
      (xs.min..xs.max).each { |x| print @panels.fetch({x,y},0)==1 ? '#' : ' ' }
      puts
    end
  end
end

prog = File.read("input.txt").split(',').map(&.to_i64)

r = Robot.new(prog,0)
r.run
puts r.count

r = Robot.new(prog,1)
r.run
r.render
