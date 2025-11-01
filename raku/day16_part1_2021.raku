
sub hex-to-bin(Str $hex) {
    $hex.comb.map({ sprintf "%04b", :16($_) }).join
}

sub parse-packet(Str $bin is copy, Int $pos is copy) {
    my $ver   = :2($bin.substr($pos, 3));
    my $type  = :2($bin.substr($pos + 3, 3));
    $pos += 6;

    if $type == 4 {
        while $bin.substr($pos, 1) eq '1' { $pos += 5 }
        $pos += 5;
        return $ver, $pos;
    }

    my $len-id = $bin.substr($pos++, 1);
    my $sum = $ver;

    my ($limit, $count);
    if $len-id eq '0' {
        $limit = :2($bin.substr($pos, 15));
        $pos += 15;
        my $end = $pos + $limit;
        while $pos < $end {
            my ($v, $new) = parse-packet($bin, $pos);
            $sum += $v;
            $pos = $new;
        }
    } else {
        $count = :2($bin.substr($pos, 11));
        $pos += 11;
        for ^$count {
            my ($v, $new) = parse-packet($bin, $pos);
            $sum += $v;
            $pos = $new;
        }
    }
    return $sum, $pos;
}

sub MAIN {
    my $hex = 'input.txt'.IO.slurp.trim;
    my $bin = hex-to-bin($hex);
    say parse-packet($bin, 0)[0];
}
