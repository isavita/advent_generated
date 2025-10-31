
#!/usr/bin/env raku

sub run-amplifier(@prog, $phase, $input) {
    my @code = @prog;
    my $ip   = 0;
    my @in  = ($phase, $input);
    my $out;

    loop {
        my $instr   = @code[$ip];
        my $opcode  = $instr % 100;
        my $mode1   = ($instr div 100)   % 10;
        my $mode2   = ($instr div 1000)  % 10;

        given $opcode {
            when 1 {
                my $a = $mode1 ?? @code[$ip+1] !! @code[@code[$ip+1]];
                my $b = $mode2 ?? @code[$ip+2] !! @code[@code[$ip+2]];
                @code[@code[$ip+3]] = $a + $b;
                $ip += 4;
            }
            when 2 {
                my $a = $mode1 ?? @code[$ip+1] !! @code[@code[$ip+1]];
                my $b = $mode2 ?? @code[$ip+2] !! @code[@code[$ip+2]];
                @code[@code[$ip+3]] = $a * $b;
                $ip += 4;
            }
            when 3 {
                @code[@code[$ip+1]] = shift @in;
                $ip += 2;
            }
            when 4 {
                $out = $mode1 ?? @code[$ip+1] !! @code[@code[$ip+1]];
                $ip += 2;
            }
            when 5 {
                my $a = $mode1 ?? @code[$ip+1] !! @code[@code[$ip+1]];
                my $b = $mode2 ?? @code[$ip+2] !! @code[@code[$ip+2]];
                $ip = $a != 0 ?? $b !! $ip + 3;
            }
            when 6 {
                my $a = $mode1 ?? @code[$ip+1] !! @code[@code[$ip+1]];
                my $b = $mode2 ?? @code[$ip+2] !! @code[@code[$ip+2]];
                $ip = $a == 0 ?? $b !! $ip + 3;
            }
            when 7 {
                my $a = $mode1 ?? @code[$ip+1] !! @code[@code[$ip+1]];
                my $b = $mode2 ?? @code[$ip+2] !! @code[@code[$ip+2]];
                @code[@code[$ip+3]] = $a < $b ?? 1 !! 0;
                $ip += 4;
            }
            when 8 {
                my $a = $mode1 ?? @code[$ip+1] !! @code[@code[$ip+1]];
                my $b = $mode2 ?? @code[$ip+2] !! @code[@code[$ip+2]];
                @code[@code[$ip+3]] = $a == $b ?? 1 !! 0;
                $ip += 4;
            }
            when 99 { last }
            default { die "unknown opcode $opcode at $ip" }
        }
    }
    $out;
}

my $line = slurp "input.txt";
my @code = $line.split(',').map(*.Int);

my $max = 0;
for (0..4).permutations -> @phase {
    my $signal = 0;
    for @phase -> $p {
        $signal = run-amplifier(@code, $p, $signal);
    }
    $max = $signal if $signal > $max;
}
say $max;
