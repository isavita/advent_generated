
use v6;
my @code = 'input.txt'.IO.slurp.split(',').map(*.Int);
my $ip     = 0;
my $rel-b = 0;
my @in;
my @out;

sub vm-get-addr(Int $pos, Int $mode) {
    given $mode {
        when 0 { @code[$pos] }
        when 1 { $pos }
        when 2 { $rel-b + @code[$pos] }
    }
}

sub vm-get(Int $pos, Int $mode) { @code[vm-get-addr($pos, $mode)] }

sub run {
    loop {
        my Int $inst = @code[$ip];
        my Int $op   = $inst % 100;
        my Int $m1   = ($inst div 100)  % 10;
        my Int $m2   = ($inst div 1000) % 10;
        my Int $m3   = ($inst div 10000) % 10;
        given $op {
            when 1 {
                my \a = vm-get-addr($ip+3, $m3);
                @code[a] = vm-get($ip+1,$m1) + vm-get($ip+2,$m2);
                $ip += 4;
            }
            when 2 {
                my \a = vm-get-addr($ip+3, $m3);
                @code[a] = vm-get($ip+1,$m1) * vm-get($ip+2,$m2);
                $ip += 4;
            }
            when 3 {
                return if !@in;
                my \a = vm-get-addr($ip+1, $m1);
                @code[a] = @in.shift;
                $ip += 2;
            }
            when 4 {
                @out.push: vm-get($ip+1,$m1);
                $ip += 2;
            }
            when 5 {
                vm-get($ip+1,$m1) ?? ($ip = vm-get($ip+2,$m2)) !! ($ip += 3);
            }
            when 6 {
                vm-get($ip+1,$m1) ?? ($ip += 3) !! ($ip = vm-get($ip+2,$m2));
            }
            when 7 {
                my \a = vm-get-addr($ip+3, $m3);
                @code[a] = vm-get($ip+1,$m1) < vm-get($ip+2,$m2) ?? 1 !! 0;
                $ip += 4;
            }
            when 8 {
                my \a = vm-get-addr($ip+3, $m3);
                @code[a] = vm-get($ip+1,$m1) == vm-get($ip+2,$m2) ?? 1 !! 0;
                $ip += 4;
            }
            when 9 {
                $rel-b += vm-get($ip+1,$m1);
                $ip += 2;
            }
            when 99 { last }
        }
    }
}

sub send-str(Str $s) {
    @in.append: $s.comb».ord;
    @in.push: 10;
}

send-str($_) for
    'NOT A J',
    'NOT B T',
    'OR T J',
    'NOT C T',
    'OR T J',
    'AND D J',
    'NOT A T',
    'AND A T',
    'OR E T',
    'OR H T',
    'AND T J',
    'RUN';

run;
put @out[*-1];
