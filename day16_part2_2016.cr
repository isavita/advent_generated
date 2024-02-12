
def read_initial_state(filename : String) : String
  File.read(filename).strip
end

def generate_data(initial_state : String, length : Int) : String
  data = initial_state
  while data.size < length
    b = String.build do |builder|
      (data.size - 1).downto(0) do |i|
        if data[i] == '0'
          builder << '1'
        else
          builder << '0'
        end
      end
    end
    data = "#{data}0#{b}"
  end
  data[0, length]
end

def calculate_checksum(data : String) : String
  while data.size % 2 == 0
    b = String.build do |builder|
      (0...data.size).step(2) do |i|
        if data[i] == data[i + 1]
          builder << '1'
        else
          builder << '0'
        end
      end
    end
    data = b
  end
  data
end

disk_length = 35651584
initial_state = read_initial_state("input.txt")
data = generate_data(initial_state, disk_length)
checksum = calculate_checksum(data)
puts "Checksum: #{checksum}"
