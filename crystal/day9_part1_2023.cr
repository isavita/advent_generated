def parse_input(input : Array(String)) : Array(Array(Int32))
  histories = [] of Array(Int32)
  input.each do |line|
    numbers = line.split(" ").map(&.to_i)
    histories << numbers
  end
  histories
end

def all_zeros(nums : Array(Int32)) : Bool
  nums.all?(&.zero?)
end

def calculate_extrapolation(history : Array(Int32)) : Array(Int32)
  extrapolations = [] of Int32
  (1...history.size).each do |i|
    extrapolation = history[i] - history[i - 1]
    extrapolations << extrapolation
  end
  extrapolations
end

def calculate_extrapolations(history : Array(Int32)) : Array(Array(Int32))
  extrapolations_series = [] of Array(Int32)
  extrapolations_series << history

  (1...history.size).each do |i|
    previous_extrapolations = extrapolations_series[i - 1]
    return extrapolations_series if all_zeros(previous_extrapolations)

    extrapolations = calculate_extrapolation(previous_extrapolations)
    extrapolations_series << extrapolations
  end
  extrapolations_series
end

def solve(input : Array(String)) : Int32
  histories = parse_input(input)
  res = 0

  histories.each do |history|
    extrapolations_series = calculate_extrapolations(history)

    future_prediction = 0
    (extrapolations_series.size - 1).downto(0) do |i|
      future_prediction = extrapolations_series[i].last + future_prediction
    end

    res += future_prediction
  end

  res
end

def read_file(file_name : String) : Array(String)
  file = File.read(file_name)
  file.split("\n").map(&.strip)
end

input = read_file("input.txt")
puts solve(input)