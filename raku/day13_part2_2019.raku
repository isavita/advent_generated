
sub run-intcode(@program, $input = 0) {
    my @mem = @program;
    my int $pc = 0;
    my int $rb = 0;
    my @out;

    sub addr(int $off) {
        my int $mode = (@mem[$pc] div 10**(1+$off)) % 10;
        my int $a = $pc + $off;
        my int $res = $mode == 2 ?? (@mem[$a] // 0) + $rb !! ($mode ?? $a !! @mem[$a] // 0);
        die "Bad addr" if $mode > 2;
        $res
    }
    sub val(int $off) { @mem[addr($off)] // 0 }

    loop {
        my int $op = @mem[$pc] % 100;
        given $op {
            when 1 { @mem[addr(3)] = val(1) + val(2); $pc += 4 }
            when 2 { @mem[addr(3)] = val(1) * val(2); $pc += 4 }
            when 3 { @mem[addr(1)] = $input; $pc += 2 }
            when 4 { @out.push: val(1); $pc += 2 }
            when 5 { $pc = val(1) ?? val(2) !! $pc + 3 }
            when 6 { $pc = val(1) == 0 ?? val(2) !! $pc + 3 }
            when 7 { @mem[addr(3)] = val(1) < val(2) ?? 1 !! 0; $pc += 4 }
            when 8 { @mem[addr(3)] = val(1) == val(2) ?? 1 !! 0; $pc += 4 }
            when 9 { $rb += val(1); $pc += 2 }
            when 99 { last }
            default { die "Bad op" }
        }
    }
    @out
}

sub MAIN {
    my @prog = 'input.txt'.IO.slurp.comb(/\-?\d+/).map: +*;

    # Part 1
    my @out = run-intcode(@prog);
    say 'Part 1: ' ~ @out[2,5...*].grep(2).elems;

    # Part 2
    @prog[0] = 2;
    my int $pc = 0;
    my int $rb = 0;
    my int $score = 0;
    my int $bx = 0;
    my int $px = 0;
    my @mem = @prog;
    my @buf;

    sub addr(int $off) {
        my int $mode = (@mem[$pc] div 10**(1+$off)) % 10;
        my int $a = $pc + $off;
        my int $res = $mode == 2 ?? (@mem[$a] // 0) + $rb !! ($mode ?? $a !! @mem[$a] // 0);
        die "Bad addr" if $mode > 2;
        $res
    }
    sub val(int $off) { @mem[addr($off)] // 0 }

    loop {
        my int $op = @mem[$pc] % 100;
        given $op {
            when 1 { @mem[addr(3)] = val(1) + val(2); $pc += 4 }
            when 2 { @mem[addr(3)] = val(1) * val(2); $pc += 4 }
            when 3 { @mem[addr(1)] = $bx <=> $px; $pc += 2 }
            when 4 { @buf.push: val(1); $pc += 2 }
            when 5 { $pc = val(1) ?? val(2) !! $pc + 3 }
            when 6 { $pc = val(1) == 0 ?? val(2) !! $pc + 3 }
            when 7 { @mem[addr(3)] = val(1) < val(2) ?? 1 !! 0; $pc += 4 }
            when 8 { @mem[addr(3)] = val(1) == val(2) ?? 1 !! 0; $pc += 4 }
            when 9 { $rb += val(1); $pc += 2 }
            when 99 { last }
            default { die "Bad op" }
        }

        while @buf >= 3 {
            my int $x = @buf.shift;
            my int $y = @buf.shift;
            my int $t = @buf.shift;
            if $x == -1 && $y == 0 { $score = $t }
            elsif $t == 3 { $px = $x }
            elsif $t == 4 { $bx = $x }
        }
    }
    say 'Part 2: ' ~ $score
}
