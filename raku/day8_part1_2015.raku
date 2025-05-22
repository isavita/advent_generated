
sub calculateMemoryLength(Str $s) returns Int {
    my $length = 0;
    my $in-escape = False;
    my $hex-count = 0;

    for 1 .. $s.chars - 2 -> $i {
        my $char = $s.substr($i, 1);
        given $char {
            when $hex-count > 0 {
                $hex-count--;
            }
            when $in-escape {
                if $_ eq 'x' {
                    $hex-count = 2;
                }
                $in-escape = False;
                $length++;
            }
            when '\\' {
                $in-escape = True;
            }
            default {
                $length++;
            }
        }
    }
    return $length;
}

sub MAIN {
    my $totalDiff = 0;
    for "input.txt".IO.lines -> $line {
        my $codeLength = $line.chars;
        my $memoryLength = calculateMemoryLength($line);
        $totalDiff += $codeLength - $memoryLength;
    }
    say $totalDiff;
}
