import os

struct Component {
    port1 int
    port2 int
}

fn read_components(filename string) []Component {
    mut components := []Component{}
    lines := os.read_lines(filename) or { return components }
    for line in lines {
        ports := line.split('/')
        if ports.len == 2 {
            components << Component{port1: ports[0].int(), port2: ports[1].int()}
        }
    }
    return components
}

fn find_strongest_longest_bridge(components []Component) (int, int) {
    return dfs(components, 0, 0, 0)
}

fn dfs(components []Component, port int, strength int, length int) (int, int) {
    mut max_strength := strength
    mut max_length := length
    for i, comp in components {
        if comp.port1 == port || comp.port2 == port {
            next_port := if comp.port1 == port { comp.port2 } else { comp.port1 }
            mut remaining := components.clone()
            remaining.delete(i)
            s, l := dfs(remaining, next_port, strength + comp.port1 + comp.port2, length + 1)
            if l > max_length || (l == max_length && s > max_strength) {
                max_strength = s
                max_length = l
            }
        }
    }
    return max_strength, max_length
}

fn main() {
    components := read_components('input.txt')
    strongest, _ := find_strongest_longest_bridge(components)
    println(strongest)
}