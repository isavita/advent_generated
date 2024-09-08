require 'set'

class Scanner
  attr_reader :beacons, :position

  def initialize(beacons)
    @beacons = beacons
    @position = [0, 0, 0]
  end

  def rotate(rotation)
    @beacons = @beacons.map { |b| rotate_point(b, rotation) }
  end

  def translate(translation)
    @position = @position.zip(translation).map { |a, b| a + b }
    @beacons = @beacons.map { |b| b.zip(translation).map { |a, b| a + b } }
  end

  private

  def rotate_point(point, rotation)
    x, y, z = point
    case rotation
    when 0 then [x, y, z]
    when 1 then [x, -z, y]
    when 2 then [x, -y, -z]
    when 3 then [x, z, -y]
    when 4 then [-x, -y, z]
    when 5 then [-x, -z, -y]
    when 6 then [-x, y, -z]
    when 7 then [-x, z, y]
    when 8 then [y, z, x]
    when 9 then [y, -x, z]
    when 10 then [y, -z, -x]
    when 11 then [y, x, -z]
    when 12 then [-y, -z, x]
    when 13 then [-y, -x, -z]
    when 14 then [-y, z, -x]
    when 15 then [-y, x, z]
    when 16 then [z, x, y]
    when 17 then [z, -y, x]
    when 18 then [z, -x, -y]
    when 19 then [z, y, -x]
    when 20 then [-z, -x, y]
    when 21 then [-z, -y, -x]
    when 22 then [-z, x, -y]
    when 23 then [-z, y, x]
    end
  end
end

def parse_input(input)
  input.split("\n\n").map do |scanner_data|
    beacons = scanner_data.split("\n")[1..-1].map { |line| line.split(',').map(&:to_i) }
    Scanner.new(beacons)
  end
end

def find_overlap(scanner1, scanner2)
  beacon_set = Set.new(scanner1.beacons)

  (0..23).each do |rotation|
    scanner2.rotate(rotation)
    offsets = Hash.new(0)

    scanner1.beacons.each do |b1|
      scanner2.beacons.each do |b2|
        offset = b1.zip(b2).map { |a, b| a - b }
        offsets[offset] += 1
        if offsets[offset] >= 12
          return offset
        end
      end
    end
  end

  nil
end

def align_scanners(scanners)
  aligned = [scanners.shift]
  unaligned = scanners

  until unaligned.empty?
    aligned_scanner = aligned.shift
    still_unaligned = []

    unaligned.each do |scanner|
      offset = find_overlap(aligned_scanner, scanner)
      if offset
        scanner.translate(offset)
        aligned << scanner
      else
        still_unaligned << scanner
      end
    end

    aligned << aligned_scanner
    unaligned = still_unaligned
  end

  aligned
end

def count_beacons(scanners)
  Set.new(scanners.flat_map(&:beacons)).size
end

# Main execution
input = File.read('input.txt')  # Assume the input is in a file named 'input.txt'
scanners = parse_input(input)
aligned_scanners = align_scanners(scanners)
beacon_count = count_beacons(aligned_scanners)
puts "Total number of beacons: #{beacon_count}"
