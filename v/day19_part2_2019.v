
import os

const mem_size = 2048
const input_size = 16
const output_size = 16

struct Vm {
mut:
    code []i64
    ip i64
    rb i64
    input []i64
    in_h int
    in_t int
    output []i64
    out_h int
    out_t int
}

fn new_vm() Vm {
    return Vm{
        code: []i64{len: mem_size}
        ip: 0
        rb: 0
        input: []i64{len: input_size}
        in_h: 0
        in_t: 0
        output: []i64{len: output_size}
        out_h: 0
        out_t: 0
    }
}

fn load(mut vm Vm) {
    data := os.read_file('input.txt') or { panic('missing input.txt') }
    for i, s in data.trim_space().split(',') {
        vm.code[i] = s.i64()
    }
}

fn has_in(vm Vm) bool { return vm.in_h != vm.in_t }

fn push_in(mut vm Vm, v i64) {
    vm.input[vm.in_t] = v
    vm.in_t = (vm.in_t + 1) % input_size
    if vm.in_t == vm.in_h { vm.in_h = (vm.in_h + 1) % input_size }
}

fn has_out(vm Vm) bool { return vm.out_h != vm.out_t }

fn pop_out(mut vm Vm) i64 {
    v := vm.output[vm.out_h]
    vm.out_h = (vm.out_h + 1) % output_size
    return v
}

fn add_out(mut vm Vm, v i64) {
    vm.output[vm.out_t] = v
    vm.out_t = (vm.out_t + 1) % output_size
    if vm.out_t == vm.out_h { vm.out_h = (vm.out_h + 1) % output_size }
}

fn get(mut vm Vm, pos i64, mode int) i64 {
    match mode {
        0 { return vm.code[vm.code[pos]] }
        1 { return vm.code[pos] }
        2 { return vm.code[vm.code[pos] + vm.rb] }
        else { panic('bad mode') }
    }
}

fn addr(mut vm Vm, pos i64, mode int) int {
    return match mode {
        0 { int(vm.code[pos]) }
        2 { int(vm.code[pos] + vm.rb) }
        else { panic('bad addr mode') }
    }
}

fn run(mut vm Vm) {
    for {
        op := vm.code[vm.ip] % 100
        m1 := int((vm.code[vm.ip] / 100) % 10)
        m2 := int((vm.code[vm.ip] / 1000) % 10)
        m3 := int((vm.code[vm.ip] / 10000) % 10)
        match op {
            1 {
                a := get(mut vm, vm.ip + 1, m1)
                b := get(mut vm, vm.ip + 2, m2)
                vm.code[addr(mut vm, vm.ip + 3, m3)] = a + b
                vm.ip += 4
            }
            2 {
                a := get(mut vm, vm.ip + 1, m1)
                b := get(mut vm, vm.ip + 2, m2)
                vm.code[addr(mut vm, vm.ip + 3, m3)] = a * b
                vm.ip += 4
            }
            3 {
                if !has_in(vm) { return }
                vm.code[addr(mut vm, vm.ip + 1, m1)] = vm.input[vm.in_h]
                vm.in_h = (vm.in_h + 1) % input_size
                vm.ip += 2
            }
            4 {
                add_out(mut vm, get(mut vm, vm.ip + 1, m1))
                vm.ip += 2
            }
            5 {
                a := get(mut vm, vm.ip + 1, m1)
                b := get(mut vm, vm.ip + 2, m2)
                vm.ip = if a != 0 { b } else { vm.ip + 3 }
            }
            6 {
                a := get(mut vm, vm.ip + 1, m1)
                b := get(mut vm, vm.ip + 2, m2)
                vm.ip = if a == 0 { b } else { vm.ip + 3 }
            }
            7 {
                a := get(mut vm, vm.ip + 1, m1)
                b := get(mut vm, vm.ip + 2, m2)
                vm.code[addr(mut vm, vm.ip + 3, m3)] = if a < b { 1 } else { 0 }
                vm.ip += 4
            }
            8 {
                a := get(mut vm, vm.ip + 1, m1)
                b := get(mut vm, vm.ip + 2, m2)
                vm.code[addr(mut vm, vm.ip + 3, m3)] = if a == b { 1 } else { 0 }
                vm.ip += 4
            }
            9 {
                vm.rb += get(mut vm, vm.ip + 1, m1)
                vm.ip += 2
            }
            99 { return }
            else { panic('bad op $op') }
        }
    }
}

fn beam(x int, y int) bool {
    mut vm := new_vm()
    load(mut vm)
    push_in(mut vm, x)
    push_in(mut vm, y)
    run(mut vm)
    return has_out(vm) && pop_out(mut vm) == 1
}

fn main() {
    mut y := 20
    mut x := 0
    for {
        if !beam(x, y) {
            x++
            continue
        }
        if !beam(x + 99, y) {
            y++
            continue
        }
        if !beam(x, y + 99) {
                       x++
            continue
        }
        println(x * 10000 + y)
        return
    }
}
