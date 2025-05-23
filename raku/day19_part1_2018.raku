
sub main {
    my @lines = 'input.txt'.IO.lines.grep(*.chars);
    my $ip-register = @lines[0].split(' ')[1].Int;
    my @instructions = @lines[1..*];

    my @registers = 0 xx 6;

    my %ops = (
        addr => -> @r, $a, $b, $c { @r[$c] = @r[$a] + @r[$b] },
        addi => -> @r, $a, $b, $c { @r[$c] = @r[$a] + $b },
        mulr => -> @r, $a, $b, $c { @r[$c] = @r[$a] * @r[$b] },
        muli => -> @r, $a, $b, $c { @r[$c] = @r[$a] * $b },
        banr => -> @r, $a, $b, $c { @r[$c] = @r[$a] +& @r[$b] },
        bani => -> @r, $a, $b, $c { @r[$c] = @r[$a] +& $b },
        borr => -> @r, $a, $b, $c { @r[$c] = @r[$a] +| @r[$b] },
        bori => -> @r, $a, $b, $c { @r[$c] = @r[$a] +| $b },
        setr => -> @r, $a, $b, $c { @r[$c] = @r[$a] },
        seti => -> @r, $a, $b, $c { @r[$c] = $a },
        gtir => -> @r, $a, $b, $c { @r[$c] = $a > @r[$b] ?? 1 !! 0 },
        gtri => -> @r, $a, $b, $c { @r[$c] = @r[$a] > $b ?? 1 !! 0 },
        gtrr => -> @r, $a, $b, $c { @r[$c] = @r[$a] > @r[$b] ?? 1 !! 0 },
        eqir => -> @r, $a, $b, $c { @r[$c] = $a == @r[$b] ?? 1 !! 0 },
        eqri => -> @r, $a, $b, $c { @r[$c] = @r[$a] == $b ?? 1 !! 0 },
        eqrr => -> @r, $a, $b, $c { @r[$c] = @r[$a] == @r[$b] ?? 1 !! 0 },
    );

    while 0 <= @registers[$ip-register] < @instructions.elems {
        my $ip = @registers[$ip-register];

        last unless $ip < @instructions.elems;

        my @parts = @instructions[$ip].split(' ');
        my $opcode = @parts[0];
        my $A = @parts[1].Int;
        my $B = @parts[2].Int;
        my $C = @parts[3].Int;

        %ops{$opcode}(@registers, $A, $B, $C);

        @registers[$ip-register] += 1;
    }

    say @registers[0];
}

main;
