
sub MAIN {
    my ($x, $y) = 0, 0;
    my ($waypoint_x, $waypoint_y) = 10, 1;

    for 'input.txt'.IO.lines -> $line {
        my $action = $line.substr(0, 1);
        my $value  = $line.substr(1).Int;

        given $action {
            when 'N' { $waypoint_y += $value; }
            when 'S' { $waypoint_y -= $value; }
            when 'E' { $waypoint_x += $value; }
            when 'W' { $waypoint_x -= $value; }
            when 'L' {
                my $turns = $value div 90;
                given $turns % 4 {
                    when 1 { ($waypoint_x, $waypoint_y) = (-$waypoint_y, $waypoint_x); }
                    when 2 { ($waypoint_x, $waypoint_y) = (-$waypoint_x, -$waypoint_y); }
                    when 3 { ($waypoint_x, $waypoint_y) = ($waypoint_y, -$waypoint_x); }
                }
            }
            when 'R' {
                my $turns = $value div 90;
                given $turns % 4 {
                    when 1 { ($waypoint_x, $waypoint_y) = ($waypoint_y, -$waypoint_x); }
                    when 2 { ($waypoint_x, $waypoint_y) = (-$waypoint_x, -$waypoint_y); }
                    when 3 { ($waypoint_x, $waypoint_y) = (-$waypoint_y, $waypoint_x); }
                }
            }
            when 'F' {
                $x += $waypoint_x * $value;
                $y += $waypoint_y * $value;
            }
        }
    }

    say ($x.abs + $y.abs);
}
