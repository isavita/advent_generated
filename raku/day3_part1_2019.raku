
sub MAIN() {
    my @wires = 'input.txt'.IO.lines;
    my @w1 = @wires[0].split(',');
    my @w2 = @wires[1].split(',');

    sub get-points(@wire) {
        my Int $x = 0;
        my Int $y = 0;
        my %points is SetHash;
        for @wire -> $move {
            my $dir = $move.substr(0,1);
            my $dist = +$move.substr(1);
            for ^$dist {
                given $dir {
                    when 'R' { $x += 1 }
                    when 'L' { $x -= 1 }
                    when 'U' { $y += 1 }
                    when 'D' { $y -= 1 }
                }
                %points{"$x,$y"} = True;
            }
        }
        %points
    }

    my %w1-points = get-points(@w1);
    my %w2-points = get-points(@w2);
    my $intersections = (%w1-points (&) %w2-points).keys.grep: { $_ ne '0,0' };
    say $intersections.map(-> $p { [+] $p.split(',').map({ abs($_) }) }).min;
}
