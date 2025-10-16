
import os

fn main() {
  input := os.read_file('input.txt') or { panic(err) }
  mut total_sum := 0
  mut enabled := true
  mut i := 0
  for i < input.len {
    if i + 4 <= input.len && input[i..i + 4] == 'mul(' {
      if enabled {
        i += 4
        mut num1 := 0
        for i < input.len && input[i].is_digit() {
          num1 = num1 * 10 + int(input[i] - `0`)
          i++
        }
        if i >= input.len || input[i] != `,` { continue }
        i++
        mut num2 := 0
        for i < input.len && input[i].is_digit() {
          num2 = num2 * 10 + int(input[i] - `0`)
          i++
        }
        if i < input.len && input[i] == `)` {
          total_sum += num1 * num2
        }
      } else {
        i++
      }
    } else if i + 4 <= input.len && input[i..i + 4] == 'do()' {
      enabled = true
      i += 4
    } else if i + 7 <= input.len && input[i..i + 7] == "don't()" {
      enabled = false
      i += 7
    } else {
      i++
    }
  }
  println(total_sum)
}
