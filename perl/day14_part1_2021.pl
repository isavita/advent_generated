
use strict;
use warnings;

open(my $fh, '<', 'input.txt') or die $!;

my $polymer = <$fh>;
chomp $polymer;

my %rules;
while (my $line = <$fh>) {
    chomp $line;
    next if $line eq "";
    my ($key, $value) = split / -> /, $line;
    $rules{$key} = $value;
}

for (my $step = 0; $step < 10; $step++) {
    $polymer = applyInsertion($polymer, \%rules);
}

my %counts = countElements($polymer);
my ($min, $max) = minMax(\%counts);

print $max - $min;

sub applyInsertion {
    my ($polymer, $rules) = @_;
    my $newPolymer = "";
    for (my $i = 0; $i < length($polymer) - 1; $i++) {
        $newPolymer .= substr($polymer, $i, 1);
        if (exists $rules->{substr($polymer, $i, 2)}) {
            $newPolymer .= $rules->{substr($polymer, $i, 2)};
        }
    }
    $newPolymer .= substr($polymer, -1);
    return $newPolymer;
}

sub countElements {
    my ($polymer) = @_;
    my %counts;
    foreach my $c (split //, $polymer) {
        $counts{$c}++;
    }
    return %counts;
}

sub minMax {
    my ($counts) = @_;
    my $min = ~0 >> 1;
    my $max = 0;
    foreach my $count (values %$counts) {
        $min = $count if $count < $min;
        $max = $count if $count > $max;
    }
    return ($min, $max);
}
