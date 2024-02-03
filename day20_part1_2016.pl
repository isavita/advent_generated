
use strict;
use warnings;
use List::Util qw( min );

my @ipRanges = readIPRanges("input.txt");
@ipRanges = sort { $a->{start} <=> $b->{start} } @ipRanges;

my $unblockedIP = findUnblockedIP(\@ipRanges);
print "$unblockedIP\n";

sub readIPRanges {
    my ($filename) = @_;
    open(my $fh, '<', $filename) or die "Cannot open file: $!";
    my @ranges;
    while (my $line = <$fh>) {
        chomp $line;
        my @parts = split("-", $line);
        my $start = int($parts[0]);
        my $end = int($parts[1]);
        push @ranges, { start => $start, end => $end };
    }
    close $fh;
    return @ranges;
}

sub findUnblockedIP {
    my ($ranges) = @_;
    my $currentIP = 0;
    foreach my $r (@$ranges) {
        if ($r->{start} > $currentIP) {
            return $currentIP;
        }
        if ($r->{end} >= $currentIP) {
            $currentIP = $r->{end} + 1;
        }
    }
    return $currentIP;
}
