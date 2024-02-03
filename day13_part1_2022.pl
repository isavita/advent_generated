
use JSON;

my $s = do {
    local $/;
    open my $fh, '<', 'input.txt' or die $!;
    <$fh>;
};

my @packets;
my $sum = 0;
my @pairs = split /\n\n/, $s;
for my $i (0..$#pairs) {
    my ($first, $second) = map { decode_json($_) } split /\n/, $pairs[$i];
    push @packets, $first, $second;
    if (compare($first, $second) == -1) {
        $sum += $i + 1;
    }
}

print "$sum\n";

sub compare {
    my ($a, $b) = @_;
    my $anum = ref $a eq 'ARRAY' ? 0 : 1;
    my $bnum = ref $b eq 'ARRAY' ? 0 : 1;

    if ($anum && $bnum) {
        return sign(int($a) - int($b));
    } elsif ($anum) {
        return compare([$a], $b);
    } elsif ($bnum) {
        return compare($a, [$b]);
    } else {
        my @aa = @$a;
        my @bb = @$b;
        for my $i (0..$#aa) {
            if ($i < @aa && $i < @bb) {
                my $c = compare($aa[$i], $bb[$i]);
                return $c if $c != 0;
            }
        }
        return sign(scalar @aa - scalar @bb);
    }
}

sub sign {
    my ($n) = @_;
    return 0 if $n == 0;
    return -1 if $n < 0;
    return 1;
}
