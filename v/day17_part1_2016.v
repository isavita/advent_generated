
import crypto.md5
import os

const max_path = 100
const queue_cap = 32768

struct State {
    x     int
    y     int
    path  string
}

fn main() {
    passcode := os.read_file('input.txt') or { panic('missing input.txt') }.trim_space()
    mut queue := []State{cap: queue_cap}
    queue << State{0, 0, ''}

    dirs := 'UDLR'
    dx   := [0, 0, -1, 1]
    dy   := [-1, 1, 0, 0]

    for queue.len > 0 {
        cur := queue[0]
        queue.delete(0)

        if cur.x == 3 && cur.y == 3 {
            println(cur.path)
            return
        }

        digest := md5.sum((passcode + cur.path).bytes())
        for i in 0 .. 4 {
            nibble := if i & 1 == 0 { digest[i/2] >> 4 } else { digest[i/2] & 0x0F }
            if nibble > 10 {
                nx, ny := cur.x + dx[i], cur.y + dy[i]
                if nx >= 0 && nx < 4 && ny >= 0 && ny < 4 {
                    queue << State{nx, ny, cur.path + dirs[i].ascii_str()}
                }
            }
        }
    }
    exit(1)
}
