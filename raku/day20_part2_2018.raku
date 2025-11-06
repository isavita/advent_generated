
class Point {
    has Int $.x;
    has Int $.y;
    method Str { "$!x,$!y" }
}

sub MAIN {
    my $regex = 'input.txt'.IO.slurp.trim.substr(1,*-1);
    my %doors;
    my @stack;
    my $pos = Point.new(x=>0,y=>0);

    for $regex.comb -> $c {
        if $c eq '(' {
            @stack.push: $pos;
        } elsif $c eq '|' {
            $pos = @stack[*-1];
        } elsif $c eq ')' {
            $pos = @stack.pop;
        } else {
            my $next = move($pos,$c);
            %doors{$pos.Str}{$next.Str} = True;
            $pos = $next;
        }
    }

    my %seen;
    my @q = Point.new(x=>0,y=>0);
    my $count = 0;
    while @q {
        my $p = @q.shift;
        for %doors{$p.Str}.keys -> $s {
            unless %seen{$s}:exists {
                %seen{$s} = (%seen{$p.Str} // 0) + 1;
                $count++ if %seen{$s} >= 1000;
                @q.push: str-to-point($s);
            }
        }
    }
    say $count;
}

sub move(Point $p, Str $d) {
    given $d {
        when 'N' { Point.new(x=>$p.x,   y=>$p.y-1) }
        when 'S' { Point.new(x=>$p.x,   y=>$p.y+1) }
        when 'E' { Point.new(x=>$p.x+1, y=>$p.y)   }
        when 'W' { Point.new(x=>$p.x-1, y=>$p.y)   }
    }
}

sub str-to-point(Str $s) {
    my ($x,$y) = $s.split(',').map: *.Int;
    Point.new(x=>$x,y=>$y);
}
