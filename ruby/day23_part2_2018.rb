#!/usr/bin/env ruby
def tdiv(x, z)
  x >= 0 ? x / z : -((-x) / z)
end

def dist(a, b)
  (a[0]-b[0]).abs + (a[1]-b[1]).abs + (a[2]-b[2]).abs
end

lines = File.read("input.txt").strip.split("\n")
bots = lines.map { |l| l.scan(/-?\d+/).map(&:to_i) }  # [x,y,z,r]
mx = bots.flat_map { |x,y,z,r| [x.abs,y.abs,z.abs,r] }.max
zoom = 1; zoom *= 2 while zoom < mx

tl = [0,0,0]
br = [0,0,0]
loop do
  zb = bots.map { |x,y,z,r| [tdiv(x, zoom), tdiv(y, zoom), tdiv(z, zoom), r / zoom] }
  best_count = -1
  best = nil
  (tl[0]..br[0]).each do |x|
    (tl[1]..br[1]).each do |y|
      (tl[2]..br[2]).each do |z|
        c = zb.count { |bx,by,bz,brad| (x-bx).abs+(y-by).abs+(z-bz).abs <= brad }
        if c > best_count || (c == best_count && (x.abs+y.abs+z.abs) < (best ? best.map(&:abs).sum : 1.0/0))
          best_count = c
          best = [x,y,z]
        end
      end
    end
  end
  tl = [(best[0]-1)<<1, (best[1]-1)<<1, (best[2]-1)<<1]
  br = [(best[0]+1)<<1, (best[1]+1)<<1, (best[2]+1)<<1]
  zoom /= 2
  if zoom == 0
    puts best.map(&:abs).sum
    break
  end
end