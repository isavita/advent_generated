
sub MAIN() {
    my %seen;
    my ($sx,$sy,$rx,$ry) = 0,0,0,0;
    %seen{"$sx,$sy"} = 1;

    for slurp('input.txt').comb.kv -> $i, $c {
        my ($x,$y) = $i %% 2 ?? ($sx,$sy) !! ($rx,$ry);
        given $c {
            when '^' { ++$y }
            when 'v' { --$y }
            when '>' { ++$x }
            when '<' { --$x }
        }
        %seen{"$x,$y"} = 1;
        if $i %% 2 { ($sx,$sy) = $x,$y } else { ($rx,$ry) = $x,$y }
    }
    say %seen.elems
}
