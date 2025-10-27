
sub MAIN {
    my $file = 'input.txt'.IO or die "Error opening file";
    my @directions = $file.slurp.comb;

    my %seen;
    my ($x, $y) = 0, 0;
    %seen{"$x,$y"} = 1;

    for @directions -> $d {
        given $d {
            when '^' { $y++ }
            when 'v' { $y-- }
            when '>' { $x++ }
            when '<' { $x-- }
        }
        %seen{"$x,$y"} = 1;
    }

    say %seen.keys.elems;
}
