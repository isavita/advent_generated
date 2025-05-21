
sub MAIN {
    my ($h, $d) = (0, 0);
    for 'input.txt'.IO.lines -> $line {
        my ($action, $value) = $line.words;
        given $action {
            when 'forward' { $h += $value.Int }
            when 'down'    { $d += $value.Int }
            when 'up'      { $d -= $value.Int }
        }
    }
    ($h * $d).say;
}
