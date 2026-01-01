
#!/usr/bin/env ruby
def main
  data = File.read('input.txt').gsub(/\r?\n/, '').strip
  return if data.empty?

  ids = {}

  data.split(',').each do |range_str|
    next if range_str.empty?
    s_str, e_str = range_str.split('-')
    start_num = s_str.to_i
    end_num   = e_str.to_i

    s_len = s_str.length
    e_len = e_str.length

    (s_len..e_len).each do |total_len|
      (1..total_len / 2).each do |k|
        next unless total_len % k == 0

        reps = total_len / k
        pow_k = 10**k
        m = (10**(k * reps) - 1) / (pow_k - 1)          # 111…1 (reps times)

        min_seed = 10**(k - 1)
        max_seed = pow_k - 1

        # ceil(start / m)  == (start + m - 1) / m
        target_min = (start_num + m - 1) / m
        target_max = end_num / m

        seed_start = [target_min, min_seed].max
        seed_end   = [target_max, max_seed].min

        if seed_start <= seed_end
          (seed_start..seed_end).each do |seed|
            ids[seed * m] = true
          end
        end
      end
    end
  end

  sum = ids.keys.inject(0) { |a, b| a + b }
  puts "Sum of invalid IDs: #{sum}"
end

main if __FILE__ == $PROGRAM_NAME
