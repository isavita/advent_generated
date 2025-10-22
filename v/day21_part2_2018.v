
import os

fn main() {
    mut reg5 := 0
    mut seen := map[int]bool{}
    mut last_unique := 0

    for {
        mut reg3 := reg5 | 65536
        reg5 = 7586220

        for {
            reg1 := reg3 & 255
            reg5 = ((reg5 + reg1) & 16777215 * 65899) & 16777215

            if reg3 < 256 {
                if seen[reg5] {
                    println(last_unique)
                    return
                }
                seen[reg5] = true
                last_unique = reg5
                break
            } else {
                reg3 = reg3 >> 8
            }
        }
    }
}
