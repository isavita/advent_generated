
my @program = slurp('input.txt').trim.split(',')».Int;

my %grid;
my ($x, $y) = 0, 0;
my $dir = 0;                       # 0 up, 1 right, 2 down, 3 left
my @dx = 0, 1, 0, -1;
my @dy = -1, 0, 1, 0;

my @mem = @program;
my $ip = 0;
my @in;
my @out;
my $halted = False;

sub run {
    @out = ();
    while !$halted {
        my $op = @mem[$ip] % 100;
        if $op == 1|2|7|8 {
            my @p = get-params(3);
            my $a = read-mem(@p[0]);
            my $b = read-mem(@p[1]);
            my $res = $op == 1 ?? $a + $b !!
                    $op == 2 ?? $a * $b !!
                    $op == 7 ?? ($a < $b ?? 1 !! 0) !!
                               ($a == $b ?? 1 !! 0);
            write-mem(@p[2], $res);
            $ip += 4;
        }
        elsif $op == 3|4 {
            my @p = get-params(1);
            if $op == 3 {
                return if !@in;
                write-mem(@p[0], @in.shift);
            } else {
                @out.push: read-mem(@p[0]);
            }
            $ip += 2;
        }
        elsif $op == 5|6 {
            my @p = get-params(2);
            my $a = read-mem(@p[0]);
            my $b = read-mem(@p[1]);
            $ip = ($op == 5 && $a != 0) || ($op == 6 && $a == 0) ?? $b !! $ip + 3;
        }
        elsif $op == 99 {
            $halted = True;
        }
    }
}

sub read-mem($a)  { ensure-mem($a); @mem[$a] }
sub write-mem($a,$v) { ensure-mem($a); @mem[$a] = $v }

sub ensure-mem($a) { @mem[$a] = 0 if $a >= @mem }

sub get-params($n) {
    my $modes = (@mem[$ip] / 100).Int;
    (0..^$n).map: {
        my $m = $modes % 10;
        $modes = ($modes / 10).Int;
        $m == 1 ?? $ip + $_ + 1 !! @mem[$ip + $_ + 1]
    }
}

while !$halted {
    my $color = %grid{"$x,$y"} // 0;
    @in.push: $color;
    run;
    if @out == 2 {
        %grid{"$x,$y"} = @out[0];
        $dir = @out[1] == 0 ?? ($dir + 3) % 4 !! ($dir + 1) % 4;
        $x += @dx[$dir];
        $y += @dy[$dir];
    }
}

say %grid.elems;
