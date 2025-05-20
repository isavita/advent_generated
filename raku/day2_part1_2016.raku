
sub MAIN {
    my @keypad = [
        <1 2 3>,
        <4 5 6>,
        <7 8 9>
    ];
    my ($x, $y) = 1, 1;
    my $code = '';

    for 'input.txt'.IO.lines -> $line {
        for $line.comb -> $char {
            given $char {
                when 'U' { if $y > 0 { $y-- } }
                when 'D' { if $y < 2 { $y++ } }
                when 'L' { if $x > 0 { $x-- } }
                when 'R' { if $x < 2 { $x++ } }
            }
        }
        $code ~= @keypad[$y][$x];
    }

    say $code;
}
