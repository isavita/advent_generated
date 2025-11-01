
sub MAIN {
    my %rules;
    for 'input.txt'.IO.lines {
        my ($lhs, $rhs) = .split(' => ');
        for 0,1 {
            for ^4 {
                %rules{$lhs} = $rhs;
                $lhs = rotate($lhs);
            }
            $lhs = flip($lhs);
        }
    }

    my @grid = '.#.', '..#', '###';

    for ^18 {
        my $n = @grid.elems;
        my ($k, $m) = $n %% 2 ?? (2,3) !! (3,4);
        my $b = $n div $k;
        my @new = '' xx ($b*$m);

        for ^$b X ^$b -> ($y, $x) {
            my @block = (^$k).map: { @grid[$y*$k+$_].substr($x*$k,$k) };
            my $key = @block.join('/');
            my @enh = %rules{$key}.split('/');
            for ^$m {
                @new[$y*$m+$_] ~= @enh[$_];
            }
        }
        @grid = @new;
    }

    say @grid.join.comb('#').elems;
}

sub rotate(Str $s) {
    my @a = $s.split('/');
    my $n = @a.elems;
    join '/', (^$n).map: -> $y {
        (^$n).map(-> $x { @a[$n-1-$x].substr($y,1) }).join
    }
}

sub flip(Str $s) {
    join '/', $s.split('/').map: *.flip;
}
