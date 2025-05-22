
sub MAIN() {
    my @instructions = 'input.txt'.IO.slurp.split(', ');

    my ($x, $y) = (0, 0);
    my $dir = 0;
    my @dirs = ((0,1), (1,0), (0,-1), (-1,0));

    for @instructions -> $instr {
        my $turn = $instr.substr(0,1);
        my $blocks = $instr.substr(1).Int;

        if $turn eq 'R' {
            $dir = ($dir + 1) % 4;
        } else {
            $dir = ($dir - 1) mod 4;
        }

        $x += @dirs[$dir][0] * $blocks;
        $y += @dirs[$dir][1] * $blocks;
    }

    say $x.abs + $y.abs;
}
