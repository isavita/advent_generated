
use strict;
use warnings;

sub firewall {
    my $input = shift;
    my @allBlockedRanges;
    foreach my $line (split("\n", $input)) {
        my @r = ();
        if ($line =~ /(\d+)-(\d+)/) {
            push @r, $1, $2;
        }
        push @allBlockedRanges, \@r;
    }

    @allBlockedRanges = sort { $a->[0] <=> $b->[0] || $a->[1] <=> $b->[1] } @allBlockedRanges;

    my @merged = ( [0, 0] );
    foreach my $r (@allBlockedRanges) {
        my $endOfLastRange = $merged[-1][1];
        if ($endOfLastRange >= $r->[0]-1) {
            $merged[-1][1] = maxInt($endOfLastRange, $r->[1]);
        } else {
            push @merged, $r;
        }
    }

    if ($merged[-1][1] != 4294967295) {
        push @merged, [4294967295, 0];
    }

    my $totalAllowed = 0;
    for (my $i = 1; $i < scalar @merged; $i++) {
        $totalAllowed += $merged[$i][0] - $merged[$i-1][1] - 1;
    }

    return $totalAllowed;
}

sub maxInt {
    my ($a, $b) = @_;
    return $a > $b ? $a : $b;
}

my $filename = 'input.txt';
open(my $fh, '<', $filename) or die "Cannot open file: $!";
my $input = do { local $/; <$fh> };
close($fh);

my $ans = firewall($input);
print "$ans\n";
