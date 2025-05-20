
use v6;

class Coord {
    has Int ($.x, $.y, $.z) is rw;
}

class Brick {
    has Coord $.mini is rw;
    has Coord $.maxi is rw;
    has Brick @.basedOn is rw;
    has Brick @.support is rw;
}

sub parseInput(@lines) {
    my @bricks;
    for @lines -> $line {
        my ($p1-str, $p2-str) = $line.split('~');
        my @p1 = $p1-str.split(',').map(*.Int);
        my @p2 = $p2-str.split(',').map(*.Int);
        my $mini = Coord.new(x => @p1[0], y => @p1[1], z => @p1[2]);
        my $maxi = Coord.new(x => @p2[0], y => @p2[1], z => @p2[2]);
        @bricks.push(Brick.new(mini => $mini, maxi => $maxi));
    }
    return @bricks;
}

sub settle(@bricks) {
    @bricks = @bricks.sort({ $^a.maxi.z <=> $^b.maxi.z });

    for ^@bricks.elems -> $i {
        my $brick = @bricks[$i];
        my $supportZ = 0;
        my @basedBricks;

        for ^$i -> $j {
            my $other = @bricks[$j];

            my $intersectX = max($brick.mini.x, $other.mini.x) <= min($brick.maxi.x, $other.maxi.x);
            my $intersectY = max($brick.mini.y, $other.mini.y) <= min($brick.maxi.y, $other.maxi.y);
            my $intersect = $intersectX && $intersectY;

            if $intersect {
                if $other.maxi.z == $supportZ {
                    @basedBricks.push($other);
                } elsif $other.maxi.z > $supportZ {
                    $supportZ = $other.maxi.z;
                    @basedBricks = ($other,);
                }
            }
        }

        $brick.basedOn = @basedBricks;
        for @basedBricks -> $basedBrick {
            $basedBrick.support.push($brick);
        }

        my $deltaZ = $brick.maxi.z - $brick.mini.z;
        $brick.mini.z = $supportZ + 1;
        $brick.maxi.z = $brick.mini.z + $deltaZ;
    }
    return @bricks;
}

sub solve(@lines) {
    my @bricks = parseInput(@lines);
    @bricks = settle(@bricks);

    my %critical-supporters;

    for @bricks -> $brick {
        if $brick.basedOn.elems == 1 {
            %critical-supporters{$brick.basedOn[0]} = True;
        }
    }

    return @bricks.elems - %critical-supporters.keys.elems;
}

sub MAIN {
    my $file = 'input.txt';
    my @lines = $file.IO.lines;
    say solve(@lines);
}
