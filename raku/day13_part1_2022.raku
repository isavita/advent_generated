
use JSON::Fast;

multi sub compare(Int $a, Int $b) {
    ($a cmp $b).Int
}

multi sub compare(Int $a, @b) {
    compare([$a], @b)
}

multi sub compare(@a, Int $b) {
    compare(@a, [$b])
}

multi sub compare(@a, @b) {
    for ^min(@a.elems, @b.elems) -> $i {
        my $c = compare(@a[$i], @b[$i]);
        if $c != 0 {
            return $c;
        }
    }
    (@a.elems cmp @b.elems).Int
}

sub MAIN() {
    my $s = slurp "input.txt";
    my $sum = 0;
    my @all-packets;

    for $s.split("\n\n").kv -> $i, $pair {
        my ($first-str, $second-str) = $pair.split("\n");
        my $first = from-json($first-str);
        my $second = from-json($second-str);

        @all-packets.push($first, $second);

        if compare($first, $second) == -1 {
            $sum += $i + 1;
        }
    }
    say $sum;
}
