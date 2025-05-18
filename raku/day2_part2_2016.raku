
sub get_code(@instructions, @keypad) {
    my ($x, $y) = (1, 1);
    my $code = "";

    for @instructions -> $instruction {
        for $instruction.comb -> $move {
            my ($nx, $ny) = ($x, $y);
            if $move eq 'U' { $nx-- }
            elsif $move eq 'D' { $nx++ }
            elsif $move eq 'L' { $ny-- }
            elsif $move eq 'R' { $ny++ }

            if $nx >= 0 && $nx < @keypad.elems && $ny >= 0 && $ny < @keypad[0].elems {
                if @keypad[$nx][$ny] != 0 {
                    $x = $nx;
                    $y = $ny;
                }
            }
        }
        $code ~= @keypad[$x][$y];
    }
    return $code;
}

sub main {
    my @instructions = 'input.txt'.IO.slurp.lines;

    my @keypad1 = [
        [1, 2, 3],
        [4, 5, 6],
        [7, 8, 9]
    ];

    my @keypad2 = [
        [0, 0, 1, 0, 0],
        [0, 2, 3, 4, 0],
        [5, 6, 7, 8, 9],
        [0, 'A', 'B', 'C', 0],
        [0, 0, 'D', 0, 0]
    ];

    say get_code(@instructions, @keypad1);
    say get_code(@instructions, @keypad2);
}

main;
