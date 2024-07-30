import os

struct Layer {
    depth int
    range int
}

fn read_input(file string) []Layer {
    mut layers := []Layer{}
    lines := os.read_lines(file) or { panic(err) }
    for line in lines {
        parts := line.split(': ')
        layers << Layer{depth: parts[0].int(), range: parts[1].int()}
    }
    return layers
}

fn severity(layers []Layer) int {
    mut total_severity := 0
    for picosecond in 0 .. layers.len {
        for layer in layers {
            if layer.depth == picosecond {
                if picosecond % (2 * (layer.range - 1)) == 0 {
                    total_severity += layer.depth * layer.range
                }
            }
        }
    }
    return total_severity
}

fn min_delay(layers []Layer) int {
    mut delay := 0
    for {
        mut caught := false
        for layer in layers {
            if (layer.depth + delay) % (2 * (layer.range - 1)) == 0 {
                caught = true
                break
            }
        }
        if !caught {
            return delay
        }
        delay++
    }
    return delay // Added return statement
}

fn main() {
    layers := read_input('input.txt')
    severity_value := severity(layers)
    println('Severity of trip: $severity_value')
    delay_value := min_delay(layers)
    println('Minimum delay to pass through safely: $delay_value')
}