
sub run-intcode(@program, @input) {
    my @memory = @program;
    my int $i = 0;
    my int $rb = 0;
    my @out;

    sub p(int $m, int $o) {
        my $v = @memory[$i + $o];
        $m == 0 ?? @memory[$v] // 0
        !! $m == 1 ?? $v
        !! @memory[$v + $rb] // 0
    }

    sub s(int $m, int $o, int $val) {
        my $a = @memory[$i + $o];
        $a += $rb if $m == 2;
        @memory[$a] = $val
    }

    loop {
        my $ins = @memory[$i].fmt('%05d');
        my int $op = $ins.substr(*-2, 2).Int;
        my int $m1 = $ins.substr(*-3, 1).Int;
        my int $m2 = $ins.substr(*-4, 1).Int;
        my int $m3 = $ins.substr(*-5, 1).Int;

        given $op {
            when 1 { s($m3, 3, p($m1,1) + p($m2,2)); $i += 4 }
            when 2 { s($m3, 3, p($m1,1) * p($m2,2)); $i += 4 }
            when 3 { s($m1, 1, @input.shift); $i += 2 }
            when 4 { @out.push: p($m1,1); $i += 2 }
            when 5 { $i = p($m1,1) ?? p($m2,2) !! $i + 3 }
            when 6 { $i = p($m1,1) ?? $i + 3 !! p($m2,2) }
            when 7 { s($m3, 3, p($m1,1) < p($m2,2) ?? 1 !! 0); $i += 4 }
            when 8 { s($m3, 3, p($m1,1) == p($m2,2) ?? 1 !! 0); $i += 4 }
            when 9 { $rb += p($m1,1); $i += 2 }
            when 99 { return @out }
        }
    }
}

sub MAIN {
    my @program = slurp('input.txt').split(',').map: *.Int;
    my @input = "NOT A J\nNOT B T\nOR T J\nNOT C T\nOR T J\nAND D J\nWALK\n".ords;
    my @out = run-intcode(@program, @input);
    say @out[*-1] > 255 ?? @out[*-1] !! @out.map(*.chr).join
}
