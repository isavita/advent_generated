
sub MAIN {
    my @moons = 'input.txt'.IO.lines.map: { .comb(/\-?\d+/)».Int.Array }
    my @v = [0,0,0] xx 4;

    for ^1000 {
        for ^4 -> $i {
            for ^4 -> $j {
                next if $i == $j;
                for ^3 -> $axis {
                    @v[$i][$axis] += (@moons[$j][$axis] <=> @moons[$i][$axis]);
                }
            }
        }
        for ^4 -> $i {
            for ^3 -> $axis {
                @moons[$i][$axis] += @v[$i][$axis];
            }
        }
    }

    my $total = 0;
    for ^4 -> $i {
        $total += ([+] @moons[$i].map: *.abs) * ([+] @v[$i].map: *.abs);
    }
    say $total;
}
