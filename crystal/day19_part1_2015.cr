File.open("input.txt", "r") do |file|
  replacements = Hash(String, Array(String)).new
  medicine = ""

  file.each_line do |line|
    if line.includes?(" => ")
      from, to = line.split(" => ")
      replacements[from] = [] of String if !replacements.has_key?(from)
      replacements[from] << to
    elsif !line.empty?
      medicine = line.chomp
    end
  end

  molecules = Set(String).new
  medicine.each_char.with_index do |c, i|
    replacements.each do |from, to|
      if medicine[i, from.size] == from
        to.each do |t|
          molecules << medicine[0, i] + t + medicine[i + from.size..-1]
        end
      end
    end
  end

  puts molecules.size
end