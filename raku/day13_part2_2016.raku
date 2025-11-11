
my $favorite-number = "input.txt".IO.slurp.Int;

sub is-wall(Int $x, Int $y --> Bool) {
    return True if $x < 0 or $y < 0;
    my $num = $x*$x + 3*$x + 2*$x*$y + $y + $y*$y + $favorite-number;
    my $bits = 0;
    while $num {
        $bits += $num +& 1;
        $num +>= 1;
    }
    return $bits % 2 != 0;
}

sub count-reachable(Int $start-x, Int $start-y, Int $max-steps --> Int) {
    my @queue = ($start-x, $start-y, 0);
    my $front = 0;
    my SetHash $visited .= new;
    $visited.set("$start-x,$start-y");
    
    while $front < @queue.elems {
        my ($x, $y, $steps) = @queue[$front..$front+2];
        $front += 3;
        last if $steps >= $max-steps;
        for (1,0), (-1,0), (0,1), (0,-1) -> ($dx, $dy) {
            my $nx = $x + $dx;
            my $ny = $y + $dy;
            my $key = "$nx,$ny";
            next if $visited{$key}:exists or is-wall($nx, $ny);
            $visited.set($key);
            @queue.append: $nx, $ny, $steps + 1;
        }
    }
    return $visited.elems;
}

sub MAIN() {
    say count-reachable(1, 1, 50);
}
