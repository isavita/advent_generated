
use strict;
use warnings;

open my $fh, '<', 'input.txt' or die $!;

my @seed_ranges;
my @maps;
my @current_ranges;

while (<$fh>) {
    chomp;
    if (/map:/) {
        push @maps, [@current_ranges] if @current_ranges;
        @current_ranges = ();
    } elsif (/^seeds:/) {
        my @seeds = split ' ', substr($_, 7);
        while (@seeds) {
            my $start = shift @seeds;
            my $length = shift @seeds;
            push @seed_ranges, [$start, $length];
        }
    } elsif (/(\d+)\s+(\d+)\s+(\d+)/) {
        push @current_ranges, [$2, $1, $3];
    }
}
push @maps, [@current_ranges] if @current_ranges;

sub reverse_convert_number {
    my ($number, $ranges) = @_;
    for my $r (reverse @$ranges) {
        if ($number >= $r->[1] && $number < $r->[1] + $r->[2]) {
            return $r->[0] + ($number - $r->[1]);
        }
    }
    return $number;
}

sub is_in_seed_ranges {
    my ($number, $ranges) = @_;
    for my $r (@$ranges) {
        if ($number >= $r->[0] && $number < $r->[0] + $r->[1]) {
            return 1;
        }
    }
    return 0;
}

my $location = 0;
while (1) {
    my $seed = $location;
    for my $i (reverse 0 .. $#maps) {
        $seed = reverse_convert_number($seed, $maps[$i]);
    }
    if (is_in_seed_ranges($seed, \@seed_ranges)) {
        print $location;
        last;
    }
    $location++;
}

close $fh;
