
#!/usr/bin/perl
use strict;
use warnings;

open my $fh, '<', 'input.txt' or die "Cannot open input.txt: $!";
my @ranges;
while (my $line = <$fh>) {
    $line =~ /^\s*$/ and last;
    my ($s, $e) = split '-', $line;
    push @ranges, [ $s, $e ];
}
my @ids = <$fh>;
close $fh;
@ranges = sort { $a->[0] <=> $b->[0] } @ranges;

my @merged;
for my $r (@ranges) {
    if (@merged && $r->[0] <= $merged[-1][1] + 1) {
        $merged[-1][1] = $r->[1] if $r->[1] > $merged[-1][1];
    } else {
        push @merged, $r;
    }
}

my $fresh = 0;
ID: for my $id (@ids) {
    chomp $id;
    for my $r (@merged) {
        if ($id >= $r->[0] && $id <= $r->[1]) {
            $fresh++;
            next ID;
        }
        last if $id < $r->[0];
    }
}
print "$fresh\n";
