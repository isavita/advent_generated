
class Sensor {
    has Int $.sx;
    has Int $.sy;
    has Int $.bx;
    has Int $.by;
    has Int $.dist;
}

sub manhattan($p1x, $p1y, $p2x, $p2y) {
    ($p1x - $p2x).abs + ($p1y - $p2y).abs;
}

sub distress(@sensors, $max_coord) {
    for 0..$max_coord -> $x {
        my $y = 0;
        while $y <= $max_coord {
            my $detected = False;
            my $next_y_candidate = $y + 1;

            for @sensors -> $s {
                if ($s.sx - $x).abs + ($s.sy - $y).abs <= $s.dist {
                    $detected = True;
                    my $remaining_dist_at_x = $s.dist - ($s.sx - $x).abs;
                    my $max_covered_y_for_s = $s.sy + $remaining_dist_at_x;
                    $next_y_candidate = max($next_y_candidate, $max_covered_y_for_s + 1);
                }
            }

            if not $detected {
                return $x * 4000000 + $y;
            } else {
                $y = $next_y_candidate;
            }
        }
    }
    return -1;
}

sub MAIN() {
    my $input_text = slurp 'input.txt';
    my @sensors;
    for $input_text.lines -> $line {
        my @parts = $line ~~ m:g/\-?\d+/;
        my ($sx, $sy, $bx, $by) = @parts».Int;
        push @sensors, Sensor.new(sx => $sx, sy => $sy, bx => $bx, by => $by, dist => manhattan($sx, $sy, $bx, $by));
    }
    my $max_coord = 4000000;
    say distress(@sensors, $max_coord);
}
