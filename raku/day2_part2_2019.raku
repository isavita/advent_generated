
sub run_intcode($noun, $verb, @program) {
    @program[1] = $noun;
    @program[2] = $verb;
    my $i = 0;
    while @program[$i] != 99 {
        my $op = @program[$i];
        if $op == 1 {
            @program[@program[$i + 3]] = @program[@program[$i + 1]] + @program[@program[$i + 2]];
        } elsif $op == 2 {
            @program[@program[$i + 3]] = @program[@program[$i + 1]] * @program[@program[$i + 2]];
        }
        $i += 4;
    }
    return @program[0];
}

sub MAIN {
    my $data = 'input.txt'.IO.slurp;
    my @program = $data.split(',').map(*.Int);

    say run_intcode(12, 2, @program.clone);

    for 0..99 -> $noun {
        for 0..99 -> $verb {
            if run_intcode($noun, $verb, @program.clone) == 19690720 {
                say 100 * $noun + $verb;
                return;
            }
        }
    }
}
