
BEGIN {
  getline n < "input.txt"
  for (i = 1; i <= n; i++) {
    elf[i] = 1
    next_elf[i] = i + 1
  }
  next_elf[n] = 1
  current = 1
  before_across = 1
  for (i = 1; i <= int(n/2) - 1; i++) {
    before_across = next_elf[before_across]
  }
  is_odd = n % 2 == 1
  while (next_elf[current] != current) {
    elf[current] += elf[next_elf[before_across]]
    next_elf[before_across] = next_elf[next_elf[before_across]]
    if (is_odd) {
      before_across = next_elf[before_across]
    }
    is_odd = !is_odd
    current = next_elf[current]
  }
  print current
}
