expenses = File.read("input.txt").lines.map { |line| line.to_i }

expenses.each_with_index do |expense1, index1|
  expenses[index1+1..].each_with_index do |expense2, index2|
    expenses[index1+index2+2..].each do |expense3|
      if expense1 + expense2 + expense3 == 2020
        puts expense1 * expense2 * expense3
        break
      end
    end
  end
end