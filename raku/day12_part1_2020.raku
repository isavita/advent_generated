
sub MAIN {
    my ($x,$y,$facing) = 0,0,0;
    for 'input.txt'.IO.lines {
        my ($action,$val) = .substr(0,1), .substr(1).Int;
        given $action {
            when 'N' { $y += $val }
            when 'S' { $y -= $val }
            when 'E' { $x += $val }
            when 'W' { $x -= $val }
            when 'L' { $facing = ($facing - $val) % 360 }
            when 'R' { $facing = ($facing + $val) % 360 }
            when 'F' {
                given $facing {
                    when 0   { $x += $val }
                    when 90  { $y -= $val }
                    when 180 { $x -= $val }
                    when 270 { $y += $val }
                }
            }
        }
    }
    say $x.abs + $y.abs
}
