
sub MAIN() {
    my $line = "input.txt".IO.slurp.trim;
    my @instructions = $line.comb(/<[RL]>\d+/);
    my @dirs = (0,1), (1,0), (0,-1), (-1,0);
    my $dir = 0;
    my ($x,$y) = 0,0;
    my %visited = "0,0" => True;

    for @instructions -> $instr {
        my $turn = $instr.substr(0,1);
        my $blocks = $instr.substr(1).Int;
        $dir = ($dir + ($turn eq 'R' ?? 1 !! -1)) % 4;

        for ^$blocks {
            $x += @dirs[$dir][0];
            $y += @dirs[$dir][1];
            my $k = "$x,$y";
            if %visited{$k} {
                say $x.abs + $y.abs;
                exit;
            }
            %visited{$k} = True;
        }
    }

    say -1;
}
