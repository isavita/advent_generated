
input = File.readlines('input.txt').map(&:chomp)

programs = {}
above_programs = []

input.each do |line|
  parts = line.split(' -> ')
  program_info = parts[0].split(' ')
  program_name = program_info[0]
  above_programs += parts[1].split(', ') if parts[1]

  programs[program_name] = {
    weight: program_info[1][1..-2].to_i,
    above: parts[1] ? parts[1].split(', ') : []
  }
end

bottom_program = (programs.keys - above_programs).first
puts bottom_program
