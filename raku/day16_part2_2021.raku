
my %hex-map = (
    '0' => '0000', '1' => '0001', '2' => '0010', '3' => '0011',
    '4' => '0100', '5' => '0101', '6' => '0110', '7' => '0111',
    '8' => '1000', '9' => '1001', 'A' => '1010', 'B' => '1011',
    'C' => '1100', 'D' => '1101', 'E' => '1110', 'F' => '1111',
);

sub hex-to-bin(Str $hex) {
    $hex.comb.map({ %hex-map{$_} }).join
}

sub parse-packet(Str $bin-str, Int :$idx is copy) {
    my $version = $bin-str.substr($idx, 3).parse-base(2);
    my $type-id = $bin-str.substr($idx + 3, 3).parse-base(2);
    $idx += 6;

    if $type-id == 4 {
        my $value = 0;
        loop {
            my $group = $bin-str.substr($idx, 5);
            $value = $value +< 4 + $group.substr(1, 4).parse-base(2);
            $idx += 5;
            last if $group.substr(0, 1) eq '0';
        }
        return $version, $idx, $value;
    }

    my $length-type-id = $bin-str.substr($idx, 1).parse-base(2);
    $idx++;

    my @values;
    if $length-type-id == 0 {
        my $sub-packet-length = $bin-str.substr($idx, 15).parse-base(2);
        $idx += 15;
        my $target-idx = $idx + $sub-packet-length;
        while $idx < $target-idx {
            my ($sub-version, $new-idx, $sub-value) = parse-packet($bin-str, :idx($idx));
            @values.push($sub-value);
            $idx = $new-idx;
        }
    } else {
        my $num-sub-packets = $bin-str.substr($idx, 11).parse-base(2);
        $idx += 11;
        for 1 .. $num-sub-packets {
            my ($sub-version, $new-idx, $sub-value) = parse-packet($bin-str, :idx($idx));
            @values.push($sub-value);
            $idx = $new-idx;
        }
    }

    my $result;
    given $type-id {
        when 0 { $result = [+] @values }
        when 1 { $result = [*] @values }
        when 2 { $result = @values.min }
        when 3 { $result = @values.max }
        when 5 { $result = @values[0] > @values[1] ?? 1 !! 0 }
        when 6 { $result = @values[0] < @values[1] ?? 1 !! 0 }
        when 7 { $result = @values[0] == @values[1] ?? 1 !! 0 }
        default { die "Unknown typeID: $type-id" }
    }

    return $version, $idx, $result;
}

sub MAIN {
    my $hex-str = slurp 'input.txt'.trim;
    my $bin-str = hex-to-bin($hex-str);
    my ($, $, $value) = parse-packet($bin-str, :idx(0));
    say $value;
}
