class Claim
  property id : Int32
  property left : Int32
  property top : Int32
  property width : Int32
  property height : Int32

  def initialize(id : Int32, left : Int32, top : Int32, width : Int32, height : Int32)
    @id = id
    @left = left
    @top = top
    @width = width
    @height = height
  end
end

def parse_claim(line : String) : Claim
  parts = line.split
  id = parts[0][1..-1].to_i
  left, top = parts[2][0..-2].split(",").map(&.to_i)
  width, height = parts[3].split("x").map(&.to_i)
  Claim.new(id, left, top, width, height)
end

def read_claims(filename : String) : Array(Claim)
  claims = [] of Claim
  File.each_line(filename) do |line|
    claims << parse_claim(line)
  end
  claims
end

def count_overlapping_inches(claims : Array(Claim)) : Int32
  fabric = Hash(String, Int32).new(0)
  claims.each do |claim|
    (claim.left...claim.left + claim.width).each do |i|
      (claim.top...claim.top + claim.height).each do |j|
        fabric["#{i},#{j}"] += 1
      end
    end
  end
  fabric.count { |_, count| count > 1 }
end

claims = read_claims("input.txt")
puts count_overlapping_inches(claims)