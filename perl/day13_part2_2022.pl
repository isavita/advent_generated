
use JSON;
use List::Util qw(first);

my $s = do {
    local $/;
    open my $fh, '<', 'input.txt' or die $!;
    <$fh>;
};

my @packets;
for my $pair (split /\n\n/, $s) {
    my ($first, $second) = map { decode_json($_) } split /\n/, $pair;
    push @packets, $first, $second;
}

my $divider1 = decode_json('[[2]]');
my $divider2 = decode_json('[[6]]');
push @packets, $divider1, $divider2;

@packets = sort { compare($a, $b) } @packets;
my $divider1Pos = first { compare($packets[$_], $divider1) >= 0 } 0..$#packets;
my $divider2Pos = first { compare($packets[$_], $divider2) >= 0 } 0..$#packets;
print (($divider1Pos + 1) * ($divider2Pos + 1) . "\n");

sub compare {
    my ($a, $b) = @_;
    my $anum = ref $a ? 0 : 1;
    my $bnum = ref $b ? 0 : 1;

    if ($anum && $bnum) {
        return sign($a - $b);
    } elsif ($anum) {
        return compare([$a], $b);
    } elsif ($bnum) {
        return compare($a, [$b]);
    } else {
        my @aa = @$a;
        my @bb = @$b;
        for my $i (0..$#aa) {
            return compare($aa[$i], $bb[$i]) if $i < @aa && $i < @bb;
        }
        return sign(@aa - @bb);
    }
}

sub sign {
    my ($n) = @_;
    return 0 if $n == 0;
    return -1 if $n < 0;
    return 1;
}
