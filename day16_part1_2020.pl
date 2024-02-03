
use strict;
use warnings;

my @rules;
my $scanningRules = 1;
my $errorRate = 0;

sub isValid {
    my ($value, $ranges) = @_;
    foreach my $range (@$ranges) {
        if ($value >= $range->[0] && $value <= $range->[1]) {
            return 1;
        }
    }
    return 0;
}

sub toInt {
    my ($s) = @_;
    return int($s);
}

sub isValidForAnyRule {
    my ($value, $rules) = @_;
    foreach my $rule (@$rules) {
        if (isValid($value, $rule->{ranges})) {
            return 1;
        }
    }
    return 0;
}

open(my $fh, '<', 'input.txt') or die "Error opening file: $!";
while (my $line = <$fh>) {
    chomp($line);
    if ($line eq "") {
        next;
    }
    if ($line eq "your ticket:" || $line eq "nearby tickets:") {
        $scanningRules = 0;
        next;
    }
    if ($scanningRules) {
        if ($line =~ /^([^:]+): (\d+)-(\d+) or (\d+)-(\d+)$/) {
            my $name = $1;
            my $range1 = [$2, $3];
            my $range2 = [$4, $5];
            push @rules, {name => $name, ranges => [$range1, $range2]};
        }
    } else {
        foreach my $value (split(",", $line)) {
            my $val = toInt($value);
            if (!isValidForAnyRule($val, \@rules)) {
                $errorRate += $val;
            }
        }
    }
}

print "$errorRate\n";
