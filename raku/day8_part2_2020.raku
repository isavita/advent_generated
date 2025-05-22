
sub MAIN {
    my @instructions = "input.txt".IO.lines;

    my @parsed-instructions = @instructions.map: {
        my ($op, $arg) = .split(' ');
        ($op, $arg.Int);
    }

    for 0 ..^ @parsed-instructions.elems -> $i {
        my ($op, $arg) = @parsed-instructions[$i];
        next if $op eq "acc";

        my @modified-program = @parsed-instructions.clone;
        if $op eq "jmp" {
            @modified-program[$i] = ("nop", $arg);
        } else {
            @modified-program[$i] = ("jmp", $arg);
        }

        my ($accumulator, $terminated) = executeBootCode(@modified-program);
        if $terminated {
            say $accumulator;
            last;
        }
    }
}

sub executeBootCode(@program) {
    my $accumulator = 0;
    my $pc = 0;
    my %visited;

    while $pc < @program.elems {
        if %visited{$pc}:exists {
            return ($accumulator, False);
        }
        %visited{$pc} = True;

        my ($op, $arg) = @program[$pc];

        given $op {
            when "acc" {
                $accumulator += $arg;
                $pc++;
            }
            when "jmp" {
                $pc += $arg;
            }
            when "nop" {
                $pc++;
            }
        }
    }

    return ($accumulator, True);
}
