
sub decompressed_length_v1($data) {
    my $len = 0;
    my $i = 0;
    my $chars = $data.chars;
    while $i < $chars {
        if $data.substr($i, 1) eq '(' {
            my $marker_end = $data.index(')', $i);
            my $marker_str = $data.substr($i + 1, $marker_end - ($i + 1));
            my ($num_chars, $repeat) = $marker_str.split('x').map(*.Int);
            $len += $num_chars * $repeat;
            $i = $marker_end + $num_chars + 1;
        } else {
            $len++;
            $i++;
        }
    }
    return $len;
}

sub decompressed_length_v2($data) {
    my $len = 0;
    my $i = 0;
    my $chars = $data.chars;
    while $i < $chars {
        if $data.substr($i, 1) eq '(' {
            my $marker_end = $data.index(')', $i);
            my $marker_str = $data.substr($i + 1, $marker_end - ($i + 1));
            my ($num_chars, $repeat) = $marker_str.split('x').map(*.Int);
            $i = $marker_end + 1;
            $len += decompressed_length_v2($data.substr($i, $num_chars)) * $repeat;
            $i += $num_chars;
        } else {
            $len++;
            $i++;
        }
    }
    return $len;
}

sub MAIN {
    my $data = slurp('input.txt').chomp;
    say decompressed_length_v1($data);
    say decompressed_length_v2($data);
}

