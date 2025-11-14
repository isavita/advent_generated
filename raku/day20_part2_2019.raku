
class Point { has Int $.x; has Int $.y }
class State { has Point $.p; has Int $.l; has Int $.s }

my @maze = 'input.txt'.IO.lines».comb;
my $h = +@maze;
my $w = +@maze[0];

my %portals;
for ^$h -> $y {
    for ^$w -> $x {
        next unless @maze[$y][$x] ~~ /<upper>/;
        if $x+1 < $w && @maze[$y][$x+1] ~~ /<upper>/ {
            my $label = @maze[$y][$x] ~ @maze[$y][$x+1];
            my $P = ($x+2 < $w && @maze[$y][$x+2] eq '.') ?? Point.new(:x($x+2), :y($y)) !! Point.new(:x($x-1), :y($y));
            %portals{$label}.push: $P;
        }
        elsif $y+1 < $h && @maze[$y+1][$x] ~~ /<upper>/ {
            my $label = @maze[$y][$x] ~ @maze[$y+1][$x];
            my $P = ($y+2 < $h && @maze[$y+2][$x] eq '.') ?? Point.new(:x($x), :y($y+2)) !! Point.new(:x($x), :y($y-1));
            %portals{$label}.push: $P;
        }
    }
}

my $start = %portals<AA>[0];
my $end   = %portals<ZZ>[0];

sub solve(Bool $part2) {
    my @q; @q.push: State.new(:p($start), :l(0), :s(0));
    my $v = SetHash.new;
    $v.set: ($start.x, $start.y, 0).join(',');
    my @dir = (Point.new(:x(0), :y(1)), Point.new(:x(0), :y(-1)),
               Point.new(:x(1), :y(0)), Point.new(:x(-1), :y(0)));

    while @q {
        my $c = @q.shift;
        return $c.s if $c.p.x == $end.x && $c.p.y == $end.y && $c.l == 0;

        for @dir -> $d {
            my $nx = $c.p.x + $d.x;
            my $ny = $c.p.y + $d.y;
            next unless 0 <= $nx < $w && 0 <= $ny < $h && @maze[$ny][$nx] eq '.';
            my $key = ($nx, $ny, $c.l).join(',');
            next if $v{$key};
            $v.set: $key;
            @q.push: State.new(:p(Point.new(:x($nx), :y($ny))), :l($c.l), :s($c.s+1));
        }

        for %portals.kv -> $label, $ps {
            next if $label eq 'AA' | 'ZZ';
            next unless $c.p.x == $ps[0].x && $c.p.y == $ps[0].y
                       || $c.p.x == $ps[1].x && $c.p.y == $ps[1].y;
            my $other = ($c.p.x == $ps[0].x && $c.p.y == $ps[0].y) ?? $ps[1] !! $ps[0];
            my $lvl = $c.l;
            if $part2 {
                $lvl += ($c.p.x == 2 || $c.p.x == $w-3 || $c.p.y == 2 || $c.p.y == $h-3) ?? -1 !! 1;
                next if $lvl < 0;
            }
            my $key = ($other.x, $other.y, $lvl).join(',');
            next if $v{$key};
            $v.set: $key;
            @q.push: State.new(:p($other), :l($lvl), :s($c.s+1));
        }
    }
    -1
}

say solve(False);
say solve(True);
