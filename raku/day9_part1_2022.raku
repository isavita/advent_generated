
sub MAIN {
    my ($hx, $hy) = (0, 0);
    my ($tx, $ty) = (0, 0);
    my %visited;

    %visited{ $tx => $ty } = True;

    for 'input.txt'.IO.lines -> $line {
        my ($dir, $steps) = $line.split(' ');
        my $num-steps = $steps.Int;

        for 1 .. $num-steps {
            given $dir {
                when "R" { $hx++ }
                when "L" { $hx-- }
                when "U" { $hy++ }
                when "D" { $hy-- }
            }

            if abs($hx - $tx) > 1 || abs($hy - $ty) > 1 {
                $tx += ($hx - $tx).sign;
                $ty += ($hy - $ty).sign;
            }
            %visited{ $tx => $ty } = True;
        }
    }
    say %visited.keys.elems;
}
