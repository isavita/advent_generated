
use strict;
use warnings;

# Read input from file
open(my $fh, '<', 'input.txt') or die "Could not open file: $!";
my @rules;
my @updates;
my $section = 0; # 0: rules, 1: updates

while (<$fh>) {
    chomp;
    if ($_ eq "") {
        $section++;
        next;
    }
    if ($section == 0) {
        push @rules, $_;
    } else {
        push @updates, [split /,/];
    }
}
close $fh;


# Process rules
my %rule_hash;
foreach my $rule (@rules) {
    my ($a, $b) = split /\|/, $rule;
    $rule_hash{$a}{$b} = 1;
}

my $sum_of_middle_pages = 0;
foreach my $update (@updates) {
    my $valid = 1;
    foreach my $i (0..$#$update) {
        foreach my $j ($i+1..$#$update) {
            if (exists $rule_hash{$update->[$j]}{$update->[$i]}) {
                $valid = 0;
                last;
            }
        }
        last unless $valid;
    }
    if ($valid) {
        my $middle_index = int(@$update / 2);
        $sum_of_middle_pages += $update->[$middle_index];
    }
}

print $sum_of_middle_pages . "\n";
