
import os
import strconv

struct Gate {
    input1 string
    input2 string
    op     string
    output string
}

fn main() {
    // Read input file
    content := os.read_file('input.txt') or {
        eprintln('Failed to read input.txt')
        return
    }
    
    mut wires := map[string]int{}
    mut gates := []Gate{}
    mut lines := content.split_into_lines()
    
    // Parse initial wire values
    mut i := 0
    for i < lines.len && lines[i].contains(':') {
        parts := lines[i].split(':')
        wire := parts[0].trim_space()
        value := strconv.atoi(parts[1].trim_space()) or { 0 }
        wires[wire] = value
        i++
    }
    
    // Skip empty line
    i++
    
    // Parse gates
    for i < lines.len {
        parts := lines[i].split(' ')
        if parts.len == 5 {
            gates << Gate{
                input1: parts[0]
                op: parts[1]
                input2: parts[2]
                output: parts[4]
            }
        }
        i++
    }
    
    // Simulate gates until all wires are evaluated
    mut changed := true
    for changed {
        changed = false
        for gate in gates {
            if gate.output !in wires {
                if gate.input1 in wires && gate.input2 in wires {
                    val1 := wires[gate.input1]
                    val2 := wires[gate.input2]
                    result := match gate.op {
                        'AND' { val1 & val2 }
                        'OR' { val1 | val2 }
                        'XOR' { val1 ^ val2 }
                        else { 0 }
                    }
                    wires[gate.output] = result
                    changed = true
                }
            }
        }
    }
    
    // Collect z wires and sort them
    mut z_wires := []string{}
    for wire in wires.keys() {
        if wire.starts_with('z') {
            z_wires << wire
        }
    }
    z_wires.sort()
    
    // Build binary number
    mut result := u64(0)
    for z_wire in z_wires {
        bit := wires[z_wire]
        if bit == 1 {
            // Find the bit position from the wire name
            pos_str := z_wire[1..]
            pos := strconv.atoi(pos_str) or { 0 }
            result |= u64(1) << pos
        }
    }
    
    println(result)
}
