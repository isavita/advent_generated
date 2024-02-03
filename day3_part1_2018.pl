
use strict;
use warnings;

sub parse_claim {
    my $line = shift;
    my ($id, $left, $top, $width, $height) = $line =~ /#(\d+) @ (\d+),(\d+): (\d+)x(\d+)/;
    return { id => $id, left => $left, top => $top, width => $width, height => $height };
}

sub read_claims {
    my $filename = shift;
    open(my $fh, '<', $filename) or die "Cannot open file: $!";
    my @claims;
    while (my $line = <$fh>) {
        chomp $line;
        my $claim = parse_claim($line);
        push @claims, $claim;
    }
    close $fh;
    return @claims;
}

sub count_overlapping_inches {
    my @claims = @_;
    my %fabric;
    foreach my $claim (@claims) {
        for (my $i = $claim->{left}; $i < $claim->{left} + $claim->{width}; $i++) {
            for (my $j = $claim->{top}; $j < $claim->{top} + $claim->{height}; $j++) {
                my $coord = "$i,$j";
                $fabric{$coord}++;
            }
        }
    }

    my $overlapping = 0;
    foreach my $count (values %fabric) {
        if ($count > 1) {
            $overlapping++;
        }
    }
    return $overlapping;
}

my @claims = read_claims("input.txt");
my $overlapping = count_overlapping_inches(@claims);
print "$overlapping\n";
