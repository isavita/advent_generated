def fft(input, phases)
  signal = input.chars.map(&:to_i)
  length = signal.length

  phases.times do
    new_signal = Array.new(length, 0)
    
    # Calculate cumulative sum from the end
    cumulative_sum = 0
    (length - 1).downto(0) do |i|
      cumulative_sum += signal[i]
      new_signal[i] = cumulative_sum.abs % 10
    end

    # Calculate the first half using optimized method
    (0...length/2).each do |i|
      sum = 0
      j = i
      while j < length
        sum += new_signal[j...[j + i + 1].min(length)].sum
        sum -= new_signal[[j + 2 * i + 2, length].min...[j + 3 * i + 3, length].min].sum if j + 2 * i + 2 < length
        j += 4 * i + 4
      end
      new_signal[i] = sum.abs % 10
    end

    signal = new_signal
  end

  signal.join[0, 8]
end

input = File.read('input.txt').strip
puts fft(input, 100)
