
BEGIN {
    num_materials = 0
    for (f = 0; (getline line < "input.txt") > 0; f++) {
        gsub(/[,.-]/, " ", line)
        n = split(line, words)
        for (i = 1; i <= n; i++) {
            if (words[i] == "generator") {
                id = get_mat_id(words[i-1])
                floors[f] = or(floors[f], lshift(1, id))
            } else if (words[i] == "microchip") {
                id = get_mat_id(words[i-2])
                floors[f] = or(floors[f], lshift(1, id + 10))
            }
        }
    }
    close("input.txt")
    
    id = get_mat_id("elerium")
    floors[0] = or(floors[0], lshift(1, id))
    floors[0] = or(floors[0], lshift(1, id + 10))
    id = get_mat_id("dilithium")
    floors[0] = or(floors[0], lshift(1, id))
    floors[0] = or(floors[0], lshift(1, id + 10))
    
    elevator = 0
    steps = 0
    final_mask = 0
    for (i = 0; i < num_materials; i++) {
        final_mask = or(final_mask, or(lshift(1, i), lshift(1, i + 10)))
    }
    
    queue[q_rear++] = pack_state()
    visited[get_key()] = 1
    
    while (q_front < q_rear) {
        unpack_state(queue[q_front++])
        
        if (floors[3] == final_mask) {
            print steps
            exit
        }
        
        n_items = 0
        for (i = 0; i < 20; i++) {
            if (and(floors[elevator], lshift(1, i))) items[n_items++] = i
        }
        
        for (d = -1; d <= 1; d += 2) {
            next_elev = elevator + d
            if (next_elev < 0 || next_elev >= 4) continue
            
            for (i = 0; i < n_items; i++) {
                move1 = lshift(1, items[i])
                
                new_floors[elevator] = and(floors[elevator], compl(move1))
                new_floors[next_elev] = or(floors[next_elev], move1)
                new_floors[1-elevator] = floors[1-elevator]
                new_floors[3] = floors[3]
                
                if (is_floor_valid(new_floors[elevator]) && is_floor_valid(new_floors[next_elev])) {
                    old_elevator = elevator
                    elevator = next_elev
                    floors[elevator] = new_floors[elevator]
                    floors[old_elevator] = new_floors[old_elevator]
                    key = get_key()
                    if (!visited[key]) {
                        visited[key] = 1
                        queue[q_rear++] = pack_state()
                    }
                    floors[elevator] = new_floors[next_elev]
                    floors[old_elevator] = new_floors[elevator]
                    elevator = old_elevator
                }
                
                for (j = i + 1; j < n_items; j++) {
                    move2 = or(move1, lshift(1, items[j]))
                    
                    new_floors[elevator] = and(floors[elevator], compl(move2))
                    new_floors[next_elev] = or(floors[next_elev], move2)
                    new_floors[1-elevator] = floors[1-elevator]
                    new_floors[3] = floors[3]
                    
                    if (is_floor_valid(new_floors[elevator]) && is_floor_valid(new_floors[next_elev])) {
                        old_elevator = elevator
                        elevator = next_elev
                        floors[elevator] = new_floors[elevator]
                        floors[old_elevator] = new_floors[old_elevator]
                        key = get_key()
                        if (!visited[key]) {
                            visited[key] = 1
                            queue[q_rear++] = pack_state()
                        }
                        floors[elevator] = new_floors[next_elev]
                        floors[old_elevator] = new_floors[elevator]
                        elevator = old_elevator
                    }
                }
            }
        }
    }
}

function get_mat_id(name) {
    for (i = 0; i < num_materials; i++) {
        if (materials[i] == name) return i
    }
    materials[num_materials] = name
    return num_materials++
}

function is_floor_valid(floor) {
    gens = and(floor, 1023)
    chips = rshift(floor, 10)
    return gens == 0 || and(chips, compl(gens)) == 0
}

function get_key() {
    key = elevator
    for (i = 0; i < num_materials; i++) {
        for (f = 0; f < 4; f++) {
            if (and(floors[f], lshift(1, i))) pairs[i]["g"] = f
            if (and(floors[f], lshift(1, i + 10))) pairs[i]["c"] = f
        }
    }
    for (i = 0; i < num_materials - 1; i++) {
        for (j = i + 1; j < num_materials; j++) {
            if (pairs[i]["g"] > pairs[j]["g"] || 
                (pairs[i]["g"] == pairs[j]["g"] && pairs[i]["c"] > pairs[j]["c"])) {
                tg = pairs[i]["g"]
                tc = pairs[i]["c"]
                pairs[i]["g"] = pairs[j]["g"]
                pairs[i]["c"] = pairs[j]["c"]
                pairs[j]["g"] = tg
                pairs[j]["c"] = tc
            }
        }
    }
    for (i = 0; i < num_materials; i++) {
        key = or(lshift(key, 4), or(lshift(pairs[i]["g"], 2), pairs[i]["c"]))
    }
    return key
}

function pack_state() {
    packed = elevator
    for (f = 0; f < 4; f++) {
        packed = or(lshift(packed, 20), floors[f])
    }
    packed = or(lshift(packed, 16), steps)
    return packed
}

function unpack_state(packed) {
    steps = and(packed, 65535)
    packed = rshift(packed, 16)
    for (f = 3; f >= 0; f--) {
        floors[f] = and(packed, 1048575)
        packed = rshift(packed, 20)
    }
    elevator = packed
}
