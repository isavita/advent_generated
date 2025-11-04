
use v6.d;

my @program = slurp('input.txt').trim.split(/','/).map: *.Int;

my @data = flat @program, 0 xx 10000;
my int $ip     = 0;
my int $relbase = 0;
my int $blocks  = 0;
my int $out-count = 0;
my int $x;
my int $y;
my int $tile;

while 1 {
    my int $op = @data[$ip] % 100;
    my int $m0 = @data[$ip] div 100 % 10;
    my int $m1 = @data[$ip] div 1000 % 10;
    my int $m2 = @data[$ip] div 10000 % 10;

    sub val(int $p, int $m) {
        my int $v = @data[$ip + $p];
        $m == 1 ?? $v !! @data[$v + ($m == 2 ?? $relbase !! 0)];
    }

    sub set(int $p, int $m, int $v) {
        my int $a = @data[$ip + $p];
        @data[$a + ($m == 2 ?? $relbase !! 0)] = $v;
    }

    given $op {
        when 1  { set(3, $m2, val(1,$m0) + val(2,$m1)); $ip += 4 }
        when 2  { set(3, $m2, val(1,$m0) * val(2,$m1)); $ip += 4 }
        when 3  { set(1, $m0, 0); $ip += 2 }
        when 4  {
            my int $o = val(1,$m0);
            if $out-count % 3 == 0 { $x = $o }
            elsif $out-count % 3 == 1 { $y = $o }
            else { $tile = $o; ++$blocks if $tile == 2 }
            ++$out-count;
            $ip += 2
        }
        when 5  { $ip = val(1,$m0) ?? val(2,$m1) !! $ip + 3 }
        when 6  { $ip = val(1,$m0) == 0 ?? val(2,$m1) !! $ip + 3 }
        when 7  { set(3, $m2, val(1,$m0) < val(2,$m1) ?? 1 !! 0); $ip += 4 }
        when 8  { set(3, $m2, val(1,$m0) == val(2,$m1) ?? 1 !! 0); $ip += 4 }
        when 9  { $relbase += val(1,$m0); $ip += 2 }
        when 99 { last }
        default { die "bad op $op" }
    }
}

put $blocks;
