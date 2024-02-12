
struct Claim
  getter id : Int32
  getter x : Int32
  getter y : Int32
  getter width : Int32
  getter height : Int32

  def initialize(@id : Int32, @x : Int32, @y : Int32, @width : Int32, @height : Int32)
  end
end

def read_claims(filename : String) : Array(Claim)
  claims = [] of Claim
  File.open(filename) do |file|
    file.each_line do |line|
      parts = line.split(" ")
      id = parts[0][1..].to_i32
      coords = parts[2][0...-1].split(",")
      x = coords[0].to_i32
      y = coords[1].to_i32
      dims = parts[3].split("x")
      width = dims[0].to_i32
      height = dims[1].to_i32
      claims << Claim.new(id, x, y, width, height)
    end
  end
  claims
end

claims = read_claims("input.txt")

fabric = Array.new(1000) { Array.new(1000, 0) }

claims.each do |claim|
  claim.height.times do |i|
    claim.width.times do |j|
      fabric[claim.y + i][claim.x + j] += 1
    end
  end
end

claims.each do |claim|
  overlap = false
  claim.height.times do |i|
    claim.width.times do |j|
      if fabric[claim.y + i][claim.x + j] > 1
        overlap = true
        break
      end
    end
    break if overlap
  end
  unless overlap
    puts claim.id
    break
  end
end
