
sub run-intcode(@program, @inputs) {
    my @mem;
    @mem[$_] = @program[$_] for ^@program;
    my int $ip = 0;
    my int $rb = 0;
    my @out;
    sub val(int $m, int $p) {
        given $m {
            when 0 { @mem[$p] // 0 }
            when 1 { $p }
            when 2 { @mem[$rb + $p] // 0 }
        }
    }
    sub set(int $m, int $p, int $v) {
        given $m {
            when 0 { @mem[$p] = $v }
            when 2 { @mem[$rb + $p] = $v }
        }
    }
    while @mem[$ip] != 99 {
        my int $op = @mem[$ip] % 100;
        my int $m1 = (@mem[$ip] div 100) % 10;
        my int $m2 = (@mem[$ip] div 1000) % 10;
        my int $m3 = (@mem[$ip] div 10000) % 10;
        given $op {
            when 1 { set($m3, @mem[$ip + 3], val($m1, @mem[$ip + 1]) + val($m2, @mem[$ip + 2])); $ip += 4 }
            when 2 { set($m3, @mem[$ip + 3], val($m1, @mem[$ip + 1]) * val($m2, @mem[$ip + 2])); $ip += 4 }
            when 3 { set($m1, @mem[$ip + 1], @inputs.shift); $ip += 2 }
            when 4 { @out.push(val($m1, @mem[$ip + 1])); $ip += 2 }
            when 5 { $ip = val($m1, @mem[$ip + 1]) ?? val($m2, @mem[$ip + 2]) !! $ip + 3 }
            when 6 { $ip = val($m1, @mem[$ip + 1]) ?? $ip + 3 !! val($m2, @mem[$ip + 2]) }
            when 7 { set($m3, @mem[$ip + 3], val($m1, @mem[$ip + 1]) < val($m2, @mem[$ip + 2]) ?? 1 !! 0); $ip += 4 }
            when 8 { set($m3, @mem[$ip + 3], val($m1, @mem[$ip + 1]) == val($m2, @mem[$ip + 2]) ?? 1 !! 0); $ip += 4 }
            when 9 { $rb += val($m1, @mem[$ip + 1]); $ip += 2 }
        }
    }
    @out
}

sub affected-points(@program, int $size) {
    my int $a = 0;
    loop (my int $y = 0; $y < $size; $y = $y + 1) {
        loop (my int $x = 0; $x < $size; $x = $x + 1) {
            $a++ if run-intcode(@program, [$x, $y])[0] == 1
        }
    }
    $a
}

sub MAIN {
    my @program = slurp('input.txt').split(',').map: *.Int;
    say affected-points(@program, 50)
}
