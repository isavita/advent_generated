
import os

fn main() {
  mut count := 0
  for line in os.read_lines('input.txt') or { return } {
    mut in_hypernet := false
    mut abba_outside := false
    mut abba_inside := false
    for i := 0; i < line.len - 3; i++ {
      match line[i] {
        `[` { in_hypernet = true }
        `]` { in_hypernet = false }
        else {
          if line[i] != line[i + 1] && line[i] == line[i + 3] && line[i + 1] == line[i + 2] {
            if in_hypernet {
              abba_inside = true
            } else {
              abba_outside = true
            }
          }
        }
      }
    }
    if abba_outside && !abba_inside {
      count++
    }
  }
  println(count)
}
