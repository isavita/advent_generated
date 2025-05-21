
sub MAIN() {
    my ($h, $d, $a) = 0, 0, 0;

    for "input.txt".IO.lines -> $line {
        my ($command, $value) = $line.split(' ', 2);
        $value = $value.Int;

        given $command {
            when 'forward' {
                $h += $value;
                $d += $a * $value;
            }
            when 'down' {
                $a += $value;
            }
            when 'up' {
                $a -= $value;
            }
        }
    }

    ($h * $d).say;
}
