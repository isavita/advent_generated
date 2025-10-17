
import os

const max_bots = 256
const max_outputs = 256

struct Bot {
mut:
    low_to  int
    high_to int
    chips   []int
}

struct Env {
mut:
    bots    [max_bots]Bot
    outputs [max_outputs]int
}

fn give_chip(mut env Env, target int, value int, is_bot bool) {
    if is_bot {
        mut bot := &env.bots[target]
        if bot.chips.len < 2 {
            bot.chips << value
        }
    } else {
        env.outputs[target] = value
    }
}

fn main() {
    data := os.read_file('input.txt') or { panic(err) }
    mut env := Env{}
    for line in data.split_into_lines() {
        if line.starts_with('value') {
            parts := line.split(' ')
            value := parts[1].int()
            bot_id := parts[5].int()
            give_chip(mut env, bot_id, value, true)
        } else if line.starts_with('bot') {
            parts := line.split(' ')
            bot_id := parts[1].int()
            low_type := parts[5]
            low_to := parts[6].int()
            high_type := parts[10]
            high_to := parts[11].int()
            mut bot := &env.bots[bot_id]
            bot.low_to = if low_type == 'bot' { low_to } else { -1 - low_to }
            bot.high_to = if high_type == 'bot' { high_to } else { -1 - high_to }
        }
    }
    for {
        mut action := false
        for i := 0; i < max_bots; i++ {
            mut bot := &env.bots[i]
            if bot.chips.len == 2 {
                action = true
                low := if bot.chips[0] < bot.chips[1] { bot.chips[0] } else { bot.chips[1] }
                high := if bot.chips[0] > bot.chips[1] { bot.chips[0] } else { bot.chips[1] }
                give_chip(mut env, if bot.low_to < 0 { -1 - bot.low_to } else { bot.low_to },
                    low, bot.low_to >= 0)
                give_chip(mut env, if bot.high_to < 0 { -1 - bot.high_to } else { bot.high_to },
                    high, bot.high_to >= 0)
                bot.chips.clear()
            }
        }
        if !action {
            break
        }
    }
    println(env.outputs[0] * env.outputs[1] * env.outputs[2])
}
