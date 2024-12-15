
#!/usr/bin/perl
use strict;
use warnings;

my $width = 101;
my $height = 103;
my @robots;

open my $fh, "<", "input.txt" or die "Could not open input.txt: $!";
while (my $line = <$fh>) {
    chomp $line;
    my ($p, $v) = split " ", $line;
    $p =~ s/p=//;
    $v =~ s/v=//;
    my ($px, $py) = split ",", $p;
    my ($vx, $vy) = split ",", $v;
    push @robots, [$px, $py, $vx, $vy];
}
close $fh;

for (my $i = 0; $i < 100; $i++) {
    for my $r (@robots) {
        $r->[0] = ($r->[0] + $r->[2]) % $width;
        $r->[1] = ($r->[1] + $r->[3]) % $height;
        $r->[0] += $width if $r->[0] < 0;
        $r->[1] += $height if $r->[1] < 0;
    }
}

my ($q1, $q2, $q3, $q4) = (0, 0, 0, 0);
for my $r (@robots) {
    my ($x, $y) = @{$r}[0,1];
    next if $x == 50 || $y == 51;
    $q1++ if $x < 50 && $y < 51;
    $q2++ if $x > 50 && $y < 51;
    $q3++ if $x < 50 && $y > 51;
    $q4++ if $x > 50 && $y > 51;
}

print $q1 * $q2 * $q3 * $q4, "\n";
