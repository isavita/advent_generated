
def parse_input(input)
  histories = []
  input.each do |line|
    numbers = line.split.map(&:to_i)
    histories << numbers
  end
  histories
end

def all_zeros(nums)
  nums.all? { |num| num == 0 }
end

def calculate_extrapolation(history)
  extrapolations = []
  (1...history.length).each do |i|
    extrapolation = history[i] - history[i-1]
    extrapolations << extrapolation
  end
  extrapolations
end

def calculate_extrapolations(history)
  extrapolations_series = [history]

  (1...history.length).each do |i|
    previous_extrapolations = extrapolations_series[i-1]
    return extrapolations_series if all_zeros(previous_extrapolations)

    extrapolations = calculate_extrapolation(previous_extrapolations)
    extrapolations_series << extrapolations
  end

  extrapolations_series
end

def solve(input)
  histories = parse_input(input)
  res = 0

  histories.each do |history|
    extrapolations_series = calculate_extrapolations(history)

    past_prediction = 0
    (extrapolations_series.length - 1).downto(0) do |i|
      past_prediction = extrapolations_series[i][0] - past_prediction
    end

    res += past_prediction
  end

  res
end

input = File.readlines("input.txt").map(&:chomp)
puts solve(input)
