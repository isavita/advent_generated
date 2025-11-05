
class Point {
    has Int $.x;
    has Int $.y;
    method new(Int $x, Int $y) { self.bless(:$x, :$y) }
    method Str() { "$!x,$!y" }
    method gist() { self.Str }
}

sub buildMap(Str $regex) {
    my %doorMap;
    my @stack;
    my $cp = Point.new(0,0);

    for $regex.comb -> $c {
        given $c {
            when '(' { @stack.push($cp) }
            when '|' { $cp = @stack[*-1] }
            when ')' { $cp = @stack.pop }
            when 'N'|'S'|'E'|'W' {
                my $np = move($cp, $c);
                %doorMap{$cp.Str}{$np.Str} = True;
                $cp = $np;
            }
        }
    }
    %doorMap
}

sub move(Point $p, Str $dir) {
    given $dir {
        when 'N' { Point.new($p.x,   $p.y-1) }
        when 'S' { Point.new($p.x,   $p.y+1) }
        when 'E' { Point.new($p.x+1, $p.y)   }
        when 'W' { Point.new($p.x-1, $p.y)   }
    }
}

sub findFurthestRoom(%doorMap) {
    my %visited;
    my @queue = Point.new(0,0);
    %visited{@queue[0].Str} = 0;
    my Int $max = 0;

    while @queue {
        my $p = @queue.shift;
        my $key = $p.Str;
        if %doorMap{$key} -> $nei {
            for $nei.keys -> $npStr {
                unless %visited{$npStr}:exists {
                    %visited{$npStr} = %visited{$key} + 1;
                    $max max= %visited{$npStr};
                    my ($x,$y) = $npStr.split(',').map: *.Int;
                    @queue.push: Point.new($x,$y);
                }
            }
        }
    }
    $max
}

sub MAIN() {
    my $regex = slurp('input.txt').trim.substr(1,*-1);
    say findFurthestRoom buildMap($regex);
}
