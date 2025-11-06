
sub MAIN {
    my @input = slurp('input.txt').trim.split(',').map: *.Int;
    say part1(@input);
    say part2(@input);
}

sub intcode(@program, @input) {
    my @mem = @program;
    my $in  = 0;
    my $out = 0;
    my $rb  = 0;
    my $ip  = 0;

    sub m($a) { @mem[$a] // 0 }

    sub addr($i, $mode) {
        my $v = m($ip+$i);
        $mode == 2 ?? $v + $rb !! $mode ?? $ip+$i !! $v
    }

    loop {
        my $op  = m($ip) % 100;
        my $m1  = (m($ip) div 100) % 10;
        my $m2  = (m($ip) div 1000) % 10;
        my $m3  = (m($ip) div 10000) % 10;
        if $op == 1 {
            @mem[addr(3,$m3)] = m(addr(1,$m1)) + m(addr(2,$m2));
            $ip += 4;
        }
        elsif $op == 2 {
            @mem[addr(3,$m3)] = m(addr(1,$m1)) * m(addr(2,$m2));
            $ip += 4;
        }
        elsif $op == 3 {
            @mem[addr(1,$m1)] = @input[$in++];
            $ip += 2;
        }
        elsif $op == 4 {
            $out = m(addr(1,$m1));
            $ip += 2;
        }
        elsif $op == 5 {
            $ip = m(addr(1,$m1)) ?? m(addr(2,$m2)) !! $ip+3;
        }
        elsif $op == 6 {
            $ip = m(addr(1,$m1)) ?? $ip+3 !! m(addr(2,$m2));
        }
        elsif $op == 7 {
            @mem[addr(3,$m3)] = m(addr(1,$m1)) < m(addr(2,$m2)) ?? 1 !! 0;
            $ip += 4;
        }
        elsif $op == 8 {
            @mem[addr(3,$m3)] = m(addr(1,$m1)) == m(addr(2,$m2)) ?? 1 !! 0;
            $ip += 4;
        }
        elsif $op == 9 {
            $rb += m(addr(1,$m1));
            $ip += 2;
        }
        elsif $op == 99 {
            return $out;
        }
    }
}

sub part1(@prog) {
    (sum (0..49).map: -> $y {
        (0..49).map: -> $x { intcode(@prog, [$x,$y]) }
    }).flat
}

sub part2(@prog) {
    my $x = 0;
    my $y = 99;
    loop {
        $x++ while intcode(@prog, [$x,$y]) == 0;
        return $x * 10000 + $y - 99 if intcode(@prog, [$x+99,$y-99]) == 1;
        $y++;
    }
}
