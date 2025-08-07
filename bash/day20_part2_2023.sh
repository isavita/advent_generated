
#!/usr/bin/env bash
#  Advent of Code 2023 – Day 20, Part 2
#  Reads the circuit description from input.txt and prints the
#  least‑common‑multiple of the loop lengths feeding the
#  “rx” module.

awk '
BEGIN {
    FS = " -> ";
    press_count = 0;
    loops_found = 0;
}

# ---------- Parse the circuit ------------------------------------
{
    if (NF == 0) next;

    name_part = $1;
    outputs_part = $2;

    # Determine module type
    if (name_part == "broadcaster") {
        type = "BROADCASTER";
        name = "broadcaster";
    } else if (substr(name_part, 1, 1) == "%") {
        type = "FLIP_FLOP";
        name = substr(name_part, 2);
    } else if (substr(name_part, 1, 1) == "&") {
        type = "CONJUNCTION";
        name = substr(name_part, 2);
    } else {
        next;                     # ignore malformed lines
    }

    idx[name] = ++num_modules;
    mod_type[idx[name]] = type;
    mod_name[idx[name]] = name;

    # Split outputs (comma‑ or space‑separated)
    gsub(/^[ \t]+|[ \t]+$/, "", outputs_part);
    split(outputs_part, outs, /[ ,]+/);
    for (i in outs) {
        if (outs[i] == "") continue;
        out_names[idx[name], ++out_count[idx[name]]] = outs[i];
    }
}

# ---------- Resolve connections --------------------------------
END {
    # Convert output names to indices
    for (i = 1; i <= num_modules; i++) {
        for (j = 1; j <= out_count[i]; j++) {
            out_name = out_names[i, j];
            out_idx = idx[out_name];
            if (out_idx == 0) continue;
            out_indices[i, j] = out_idx;

            # If the target is a conjunction, register this module as an input
            if (mod_type[out_idx] == "CONJUNCTION") {
                conj_in_idx[out_idx, ++conj_in_count[out_idx]] = i;
                conj_in_last[out_idx, conj_in_count[out_idx]] = 0;
            }
        }
    }

    # Find the module that feeds into “rx”
    rx_feeder = -1;
    for (i = 1; i <= num_modules; i++) {
        for (j = 1; j <= out_count[i]; j++) {
            if (out_names[i, j] == "rx") {
                rx_feeder = i;
                break;
            }
        }
        if (rx_feeder != -1) break;
    }

    if (rx_feeder == -1 || mod_type[rx_feeder] != "CONJUNCTION") {
        print 0;
        exit;
    }

    num_inputs = conj_in_count[rx_feeder];
    for (i = 1; i <= num_inputs; i++) loop_len[i] = 0;

    # ---------- Simulation ------------------------------------
    press_count = 0;
    while (loops_found < num_inputs && press_count < 1000000) {
        press_count++;

        # Initialise queue for this button press
        q_head = 1; q_tail = 1;
        enqueue(-1, idx["broadcaster"], 0);

        while (q_head < q_tail) {
            from = q_from[q_head];
            to   = q_to[q_head];
            pulse = q_pulse[q_head];
            q_head++;

            if (to == -1) continue;

            type = mod_type[to];

            if (type == "FLIP_FLOP") {
                if (!pulse) {
                    ff_state[to] = !ff_state[to];
                    pulse_to_send = ff_state[to];
                    for (k = 1; k <= out_count[to]; k++)
                        enqueue(to, out_indices[to, k], pulse_to_send);
                }
            } else if (type == "CONJUNCTION") {
                # Update memory for the input that sent the pulse
                for (k = 1; k <= conj_in_count[to]; k++) {
                    if (conj_in_idx[to, k] == from)
                        conj_in_last[to, k] = pulse;
                }
                all_high = 1;
                for (k = 1; k <= conj_in_count[to]; k++)
                    if (!conj_in_last[to, k]) { all_high = 0; break; }

                pulse_to_send = !all_high;
                for (k = 1; k <= out_count[to]; k++)
                    enqueue(to, out_indices[to, k], pulse_to_send);
            } else {   # BROADCASTER
                pulse_to_send = pulse;
                for (k = 1; k <= out_count[to]; k++)
                    enqueue(to, out_indices[to, k], pulse_to_send);
            }

            # Detect high pulse arriving at rx feeder
            if (to == rx_feeder && pulse) {
                for (k = 1; k <= num_inputs; k++) {
                    if (conj_in_idx[rx_feeder, k] == from && loop_len[k] == 0) {
                        loop_len[k] = press_count;
                        loops_found++;
                    }
                }
            }
        }
    }

    # ---------- Compute LCM --------------------------------------
    lcm_val = 1;
    for (i = 1; i <= num_inputs; i++)
        lcm_val = lcm(lcm_val, loop_len[i]);

    print lcm_val;
}

# ---------- Helper functions --------------------------------
function enqueue(f, t, p,    idx) {
    q_from[q_tail] = f;
    q_to[q_tail]   = t;
    q_pulse[q_tail] = p;
    q_tail++;
}

function gcd(a, b,    t) {
    while (b != 0) {
        t = a % b;
        a = b;
        b = t;
    }
    return a;
}

function lcm(a, b,    g) {
    g = gcd(a, b);
    return (a / g) * b;
}
' input.txt
