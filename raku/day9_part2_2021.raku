
sub is_low_point(@heightmap, $x, $y) {
    my $height = @heightmap[$y][$x];
    my $rows = @heightmap.elems;
    my $cols = @heightmap[0].elems;

    if $x > 0 and @heightmap[$y][$x-1] <= $height { return False }
    if $x < $cols - 1 and @heightmap[$y][$x+1] <= $height { return False }
    if $y > 0 and @heightmap[$y-1][$x] <= $height { return False }
    if $y < $rows - 1 and @heightmap[$y+1][$x] <= $height { return False }

    return True;
}

sub explore_basin(@heightmap, $x, $y, %visited) {
    my $rows = @heightmap.elems;
    my $cols = @heightmap[0].elems;

    if $x < 0 || $x >= $cols || $y < 0 || $y >= $rows { return 0 }
    if %visited{"$x,$y"} { return 0 }
    if @heightmap[$y][$x] == 9 { return 0 }

    %visited{"$x,$y"} = True;
    my $size = 1;

    my @directions = ( (0, -1), (-1, 0), (0, 1), (1, 0) );
    for @directions -> ($dx, $dy) {
        $size += explore_basin(@heightmap, $x + $dx, $y + $dy, %visited);
    }

    return $size;
}

sub MAIN {
    my @heightmap;
    for "input.txt".IO.lines -> $line {
        @heightmap.push: $line.comb.map(*.Int);
    }

    my @basin_sizes;
    my %visited;

    for 0 .. @heightmap.end -> $y {
        for 0 .. @heightmap[$y].end -> $x {
            if is_low_point(@heightmap, $x, $y) {
                %visited = (); # Reset visited for each new basin exploration
                my $size = explore_basin(@heightmap, $x, $y, %visited);
                @basin_sizes.push: $size;
            }
        }
    }

    @basin_sizes .= sort({ $^b <=> $^a });

    my $result = @basin_sizes[0] * @basin_sizes[1] * @basin_sizes[2];
    say $result;
}
