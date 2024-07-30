import os

fn main() {
    mut severity := 0
    layers := read_input('input.txt')
    
    for depth in 0 .. layers.len {
        if layers[depth] > 0 && depth % (2 * layers[depth] - 2) == 0 {
            severity += depth * layers[depth]
        }
    }
    
    println(severity)
}

fn read_input(filename string) []int {
    mut layers := []int{len: 0, init: 0}
    lines := os.read_lines(filename) or { return layers }
    
    for line in lines {
        parts := line.split(': ')
        depth := parts[0].int()
        range := parts[1].int()
        if depth >= layers.len {
            layers << []int{len: depth - layers.len + 1, init: 0}
        }
        layers[depth] = range
    }
    return layers
}