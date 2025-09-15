use std::collections::{HashMap, HashSet, VecDeque};
use std::fs;

type Int = i64;

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
enum EmuStatus {
    Halted,
    Output,
    WaitingForInput,
}

struct Emulator {
    mem: HashMap<Int, Int>,
    ip: Int,
    rb: Int,
    input: VecDeque<Int>,
}
impl Emulator {
    fn new(program: &[Int]) -> Self {
        let mut mem = HashMap::new();
        for (i, &v) in program.iter().enumerate() {
            mem.insert(i as Int, v);
        }
        Self { mem, ip: 0, rb: 0, input: VecDeque::new() }
    }
    fn mget(&self, a: Int) -> Int { *self.mem.get(&a).unwrap_or(&0) }
    fn mset(&mut self, a: Int, v: Int) { self.mem.insert(a, v); }
    fn write_str(&mut self, s: &str) { for b in s.bytes() { self.input.push_back(b as Int); } }
    fn rd(&self, idx: Int, m: Int) -> Int {
        let v = self.mget(self.ip + idx);
        match m { 0 => self.mget(v), 1 => v, 2 => self.mget(self.rb + v), _ => panic!("bad mode") }
    }
    fn wa(&self, idx: Int, m: Int) -> Int {
        let v = self.mget(self.ip + idx);
        match m { 0 => v, 2 => self.rb + v, _ => panic!("bad write mode") }
    }
    fn emulate(&mut self) -> (Option<Int>, EmuStatus) {
        loop {
            let op = self.mget(self.ip);
            let oc = op % 100;
            let m1 = (op / 100) % 10;
            let m2 = (op / 1000) % 10;
            let m3 = (op / 10000) % 10;
            match oc {
                1 => { let a=self.rd(1,m1); let b=self.rd(2,m2); let c=self.wa(3,m3); self.mset(c,a+b); self.ip+=4; }
                2 => { let a=self.rd(1,m1); let b=self.rd(2,m2); let c=self.wa(3,m3); self.mset(c,a*b); self.ip+=4; }
                3 => {
                    let a = self.wa(1, m1);
                    if let Some(v) = self.input.pop_front() { self.mset(a, v); self.ip += 2; }
                    else { return (None, EmuStatus::WaitingForInput); }
                }
                4 => { let a=self.rd(1,m1); self.ip+=2; return (Some(a), EmuStatus::Output); }
                5 => { let a=self.rd(1,m1); let b=self.rd(2,m2); self.ip = if a!=0 { b } else { self.ip+3 }; }
                6 => { let a=self.rd(1,m1); let b=self.rd(2,m2); self.ip = if a==0 { b } else { self.ip+3 }; }
                7 => { let a=self.rd(1,m1); let b=self.rd(2,m2); let c=self.wa(3,m3); self.mset(c, if a<b {1} else {0}); self.ip+=4; }
                8 => { let a=self.rd(1,m1); let b=self.rd(2,m2); let c=self.wa(3,m3); self.mset(c, if a==b {1} else {0}); self.ip+=4; }
                9 => { let a=self.rd(1,m1); self.rb += a; self.ip += 2; }
                99 => return (None, EmuStatus::Halted),
                _ => panic!("bad opcode {}", oc),
            }
        }
    }
}

#[derive(Clone)]
struct Room {
    name: String,
    // direction -> Some(room_idx) once known; present key with None means known door but target unknown yet
    connections: HashMap<String, Option<usize>>,
}
impl Room {
    fn new(name: String) -> Self {
        Self { name, connections: HashMap::new() }
    }
}

#[derive(Clone, Copy, PartialEq, Eq)]
enum Mode { Explore, Navigate, Test }

fn read_file(p: &str) -> String {
    fs::read_to_string(p).expect("read input.txt").trim().to_string()
}

fn is_bad_item(item: &str) -> bool {
    matches!(item, "photons" | "escape pod" | "molten lava" | "infinite loop" | "giant electromagnet")
}
fn opposite(dir: &str) -> &'static str {
    match dir { "north" => "south", "south" => "north", "west" => "east", "east" => "west", _ => "" }
}
fn find_path(from_idx: usize, to_idx: usize, world: &[Room]) -> Option<Vec<usize>> {
    let mut q = VecDeque::new();
    let mut seen = HashSet::new();
    let mut prev: HashMap<usize, usize> = HashMap::new();
    q.push_back(from_idx);
    seen.insert(from_idx);
    while let Some(u) = q.pop_front() {
        if u == to_idx { break; }
        for (_d, optv) in &world[u].connections {
            if let Some(v) = optv {
                if seen.insert(*v) {
                    prev.insert(*v, u);
                    q.push_back(*v);
                }
            }
        }
    }
    if !seen.contains(&to_idx) { return None; }
    let mut path = vec![to_idx];
    let mut cur = to_idx;
    while cur != from_idx {
        cur = prev[&cur];
        path.push(cur);
    }
    path.reverse();
    Some(path)
}

fn parse_result_code(text: &str) -> Option<String> {
    if let Some(idx) = text.find("You should be able to get in by typing ") {
        let t = &text[idx + "You should be able to get in by typing ".len()..];
        let mut digits = String::new();
        for ch in t.chars() {
            if ch.is_ascii_digit() { digits.push(ch); } else { break; }
        }
        if !digits.is_empty() { return Some(digits); }
    }
    None
}

fn main() {
    let program: Vec<Int> = read_file("input.txt").split(',').map(|s| s.parse::<Int>().unwrap()).collect();
    let mut emu = Emulator::new(&program);

    // World and state
    let mut world: Vec<Room> = Vec::new();
    let mut by_name: HashMap<String, usize> = HashMap::new();
    let mut current_idx: Option<usize> = None;

    let mut inventory: HashMap<String, bool> = HashMap::new(); // item -> have?
    let mut mode = Mode::Explore;
    let mut path_stack: Vec<usize> = Vec::new(); // DFS stack of rooms
    let mut checkpoint_idx: Option<usize> = None;
    let mut test_dir: String = String::new(); // direction from checkpoint to floor

    // “last” tracking (exactly like the Ruby)
    let mut last_room_idx: Option<usize> = None;
    let mut last_items: Vec<String> = Vec::new();
    let mut last_dir: String = String::new();

    // TEST-mode
    let mut available_items: Vec<String> = Vec::new();
    let mut item_mask: usize = 0;

    let mut out_buf: Vec<u8> = Vec::new();
    let send_cmd = |emu: &mut Emulator, s: &str| { emu.write_str(s); };

    loop {
        let (val, st) = emu.emulate();
        match st {
            EmuStatus::Output => {
                if let Some(v) = val {
                    if let Some(c) = char::from_u32(v as u32) { out_buf.push(c as u8); }
                }
            }
            EmuStatus::Halted => {
                // Some interpreters can halt if fed EOF; try to extract code if present
                let text = String::from_utf8_lossy(&out_buf);
                if let Some(code) = parse_result_code(&text) { println!("{}", code); }
                break;
            }
            EmuStatus::WaitingForInput => {
                let text = String::from_utf8_lossy(&out_buf).to_string();
                out_buf.clear();

                // --- Parse the output block (like Ruby) ---
                let mut items_here: Vec<String> = Vec::new();
                let lines: Vec<&str> = text.lines().map(|l| l.trim_end()).collect();
                let mut i = 0usize;
                while i < lines.len() {
                    let line = lines[i].trim();

                    if line.is_empty() || line == "Command?" { i += 1; continue; }

                    // Room header "== Name =="
                    if line.starts_with("== ") && line.ends_with(" ==") {
                        let name = line.trim_matches('=').trim().to_string();
                        // Skip description lines until empty line
                        i += 1;
                        while i < lines.len() && !lines[i].trim().is_empty() { i += 1; }
                        let idx = if let Some(&idx) = by_name.get(&name) {
                            idx
                        } else {
                            let idx = world.len();
                            world.push(Room::new(name.clone()));
                            by_name.insert(name, idx);
                            idx
                        };
                        current_idx = Some(idx);
                        items_here.clear();
                        continue;
                    }

                    // Doors here lead:
                    if line == "Doors here lead:" {
                        i += 1;
                        while i < lines.len() {
                            let l = lines[i].trim();
                            if l.starts_with("- ") {
                                let dir = l[2..].trim().to_string();
                                if let Some(ci) = current_idx {
                                    world[ci].connections.entry(dir).or_insert(None);
                                }
                                i += 1;
                            } else { break; }
                        }
                        continue;
                    }

                    // Items here:
                    if line == "Items here:" {
                        i += 1;
                        while i < lines.len() {
                            let l = lines[i].trim();
                            if l.starts_with("- ") {
                                items_here.push(l[2..].trim().to_string());
                                i += 1;
                            } else { break; }
                        }
                        continue;
                    }

                    // You take/drop the X.
                    if let Some(rest) = line.strip_prefix("You take the ") {
                        if let Some(item) = rest.strip_suffix('.') {
                            inventory.insert(item.to_string(), true);
                            if last_room_idx.is_some() && !last_items.is_empty() {
                                last_items.retain(|it| it != item);
                            }
                        }
                        i += 1;
                        continue;
                    }
                    if let Some(rest) = line.strip_prefix("You drop the ") {
                        if let Some(item) = rest.strip_suffix('.') {
                            inventory.insert(item.to_string(), false);
                            if last_room_idx.is_some() {
                                last_items.push(item.to_string());
                            }
                        }
                        i += 1;
                        continue;
                    }

                    // ALERT: learn checkpoint + test_dir (mirror Ruby)
                    if line.starts_with("A loud, robotic voice says \"Alert!") {
                        if mode == Mode::Explore {
                            if !path_stack.is_empty() { path_stack.pop(); } // mirror Ruby's path.pop
                            if let (Some(cp), Some(cur), ld) = (last_room_idx, current_idx, last_dir.as_str()) {
                                checkpoint_idx = Some(cp);
                                // Link cp --ld--> cur and reverse
                                world[cp].connections.insert(ld.to_string(), Some(cur));
                                world[cur].connections.insert(opposite(ld).to_string(), Some(cp));
                                test_dir = ld.to_string();
                            }
                        }
                        last_room_idx = None;
                        last_items.clear();
                        last_dir.clear();
                        i += 1;
                        continue;
                    }

                    i += 1;
                }

                // Wire the last movement edge if we just moved
                if let (Some(prev_idx), Some(cur_idx)) = (last_room_idx, current_idx) {
                    if !last_dir.is_empty() {
                        let need = world[prev_idx].connections.get(&last_dir).map(|o| o.is_none()).unwrap_or(true);
                        if need {
                            world[prev_idx].connections.insert(last_dir.clone(), Some(cur_idx));
                            let rev = opposite(&last_dir).to_string();
                            world[cur_idx].connections.insert(rev, Some(prev_idx));
                        }
                    }
                }

                // Update "last"
                last_room_idx = current_idx;
                last_items = items_here.clone();
                last_dir.clear();

                // Success detection anywhere in this block
                if let Some(code) = parse_result_code(&text) {
                    println!("{}", code);
                    return;
                }

                // --- Decide next command (exactly one) ---
                match mode {
                    Mode::Explore => {
                        // Take a safe item if present
                        if let Some(item) = items_here.iter().find(|it| !is_bad_item(it)) {
                            send_cmd(&mut emu, &format!("take {}\n", item));
                            continue;
                        }

                        // Unexplored door? go there and push current on path
                        if let Some(ci) = current_idx {
                            if let Some((dir, _)) = world[ci].connections.iter().find(|(_d, to)| to.is_none()) {
                                path_stack.push(ci);
                                last_dir = dir.clone();
                                send_cmd(&mut emu, &format!("{}\n", dir));
                                continue;
                            }
                        }

                        // Backtrack if path stack non-empty
                        if let Some(ci) = current_idx {
                            if let Some(last_room) = path_stack.pop() {
                                // find direction to last_room
                                if let Some((dir, _)) = world[ci].connections.iter().find(|(_d, to)| to == &&Some(last_room)) {
                                    last_dir = dir.clone();
                                    send_cmd(&mut emu, &format!("{}\n", dir));
                                    continue;
                                }
                            }
                        }

                        // If we know checkpoint + test_dir, navigate there
                        if let (Some(ci), Some(cp)) = (current_idx, checkpoint_idx) {
                            if ci != cp {
                                if let Some(p) = find_path(ci, cp, &world) {
                                    // Convert to a sequence of room indices to visit (skip current)
                                    let mut p2: Vec<usize> = p.into_iter().skip(1).collect();
                                    path_stack = p2.drain(..).collect();
                                    mode = Mode::Navigate;
                                    continue;
                                }
                            } else {
                                // already at checkpoint, switch to TEST
                                mode = Mode::Test;
                                // initialize available items from inventory
                                available_items = inventory.iter().filter_map(|(k, v)| if *v { Some(k.clone()) } else { None }).collect();
                                available_items.sort();
                                item_mask = 0;
                            }
                        }
                    }

                    Mode::Navigate => {
                        if let Some(ci) = current_idx {
                            if let Some(next_room) = path_stack.first().cloned() {
                                // find direction to next_room
                                if let Some((dir, _)) = world[ci].connections.iter().find(|(_d, to)| to == &&Some(next_room)) {
                                    last_dir = dir.clone();
                                    send_cmd(&mut emu, &format!("{}\n", dir));
                                    // consume this step after we actually move (next WAIT)
                                    path_stack.remove(0);
                                    continue;
                                } else {
                                    // if linkage missing, drop back to Explore
                                    mode = Mode::Explore;
                                }
                            } else {
                                // arrived
                                mode = Mode::Test;
                                available_items = inventory.iter().filter_map(|(k, v)| if *v { Some(k.clone()) } else { None }).collect();
                                available_items.sort();
                                item_mask = 0;
                            }
                        }
                    }

                    Mode::Test => {
                        // Align inventory to the desired subset state for current mask
                        let mut changed = false;
                        for (idx, item) in available_items.iter().enumerate() {
                            let want = ((item_mask >> idx) & 1) == 1;
                            let have = *inventory.get(item).unwrap_or(&false);
                            if want != have {
                                let action = if want { "take" } else { "drop" };
                                send_cmd(&mut emu, &format!("{action} {item}\n"));
                                changed = true;
                                break;
                            }
                        }
                        if changed { continue; }

                        // Inventory matches mask -> attempt crossing
                        if !test_dir.is_empty() {
                            send_cmd(&mut emu, &format!("{}\n", test_dir));
                            item_mask += 1;
                            continue;
                        } else {
                            // Shouldn't happen; stay idle
                            return;
                        }
                    }
                }
            }
        }
    }
}

