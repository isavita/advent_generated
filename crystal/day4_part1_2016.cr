
class Room
  property name : String
  property sector_id : Int32
  property checksum : String

  def initialize(@name : String, @sector_id : Int32, @checksum : String)
  end

  def real? : Bool
    # Count the frequency of each letter in the name
    letter_counts = Hash(Char, Int32).new(0)
    @name.each_char do |char|
      letter_counts[char] += 1 if char != '-'
    end

    # Sort letters by frequency (descending) and then alphabetically
    sorted_letters = letter_counts.keys.sort do |a, b|
      if letter_counts[b] == letter_counts[a]
        a <=> b
      else
        letter_counts[b] <=> letter_counts[a]
      end
    end

    # Take the first 5 letters and join them to form the expected checksum
    expected_checksum = sorted_letters[0, 5].join

    # Compare the expected checksum with the actual checksum
    expected_checksum == @checksum
  end
end

def process_rooms(file_path : String) : Int32
  sum_of_sector_ids = 0

  File.open(file_path, "r") do |file|
    file.each_line do |line|
      # Parse the line to extract the room name, sector ID, and checksum
      match = line.match(/([a-z-]+)-(\d+)\[([a-z]+)\]/)
      if match
        name = match[1]
        sector_id = match[2].to_i32
        checksum = match[3]
        room = Room.new(name, sector_id, checksum)

        # If the room is real, add its sector ID to the sum
        sum_of_sector_ids += sector_id if room.real?
      end
    end
  end

  sum_of_sector_ids
end

# Read input from input.txt and print the sum of sector IDs of real rooms
puts process_rooms("input.txt")
