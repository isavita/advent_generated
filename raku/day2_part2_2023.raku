
sub MAIN {
    my %max = red => 12, green => 13, blue => 14;

    my @games = 'input.txt'.IO.lines.map: -> $line {
        my ($id, $sets) = $line.split(': ');
        my @cube-sets = $sets.split('; ').map: -> $set {
            my %cubes;
            $set.split(', ').map: -> $cube {
                my ($n, $color) = $cube.split: ' ';
                %cubes{$color} += $n;
            }
            %cubes
        }
        %( id => $id.split(' ')[1].Int, cube-sets => @cube-sets )
    }

    my $valid = @games.grep(-> $g {
        $g<cube-sets>.all.{ (.<red>  // 0) <= %max<red>  &&
                            (.<green> // 0) <= %max<green> &&
                            (.<blue>  // 0) <= %max<blue> }
    }).map(*<id>).sum;
    say $valid;

    my $power = @games.map(-> $g {
        my %min = red => 0, green => 0, blue => 0;
        $g<cube-sets>.map: -> $s {
            %min<red>   max= $s<red>   // 0;
            %min<green> max= $s<green> // 0;
            %min<blue>  max= $s<blue>  // 0;
        }
        [*] %min.values
    }).sum;
    say $power
}
