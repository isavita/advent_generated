
sub MAIN() {
    my @instructions = 'input.txt'.IO.lines;
    my ($accumulator, $is-loop) = executeBootCode(@instructions);
    say $accumulator;
}

sub executeBootCode(@instructions) {
    my $accumulator = 0;
    my %visited;
    my $current-instruction = 0;

    while $current-instruction < @instructions.elems {
        if %visited{$current-instruction}:exists {
            return $accumulator, True;
        }

        %visited{$current-instruction} = True;

        my ($op, $arg) = @instructions[$current-instruction].words;
        $arg = $arg.Int;

        given $op {
            when 'acc' {
                $accumulator += $arg;
                $current-instruction++;
            }
            when 'jmp' {
                $current-instruction += $arg;
            }
            when 'nop' {
                $current-instruction++;
            }
        }
    }

    return $accumulator, False;
}
