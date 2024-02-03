
use strict;
use warnings;

open(my $fh, '<', 'input.txt') or die $!;

my @particles;
while (my $line = <$fh>) {
    chomp $line;
    my @parts = split(", ", $line);

    my %p;
    for my $i (0..$#parts) {
        my @coords = split(",", substr($parts[$i], 3, -1));
        for my $j (0..$#coords) {
            my $num = int($coords[$j]);
            if ($i == 0) {
                $p{p}[$j] = $num;
            } elsif ($i == 1) {
                $p{v}[$j] = $num;
            } else {
                $p{a}[$j] = $num;
            }
        }
    }
    push @particles, \%p;
}

for my $tick (0..999) {
    my %positions;
    for my $i (0..$#particles) {
        my $particle = $particles[$i];
        for my $j (0..2) {
            $particle->{v}[$j] += $particle->{a}[$j];
            $particle->{p}[$j] += $particle->{v}[$j];
        }
        $particles[$i] = $particle;
        my $posStr = join(",", @{$particle->{p}});
        $positions{$posStr}++;
    }

    my @newParticles;
    for my $particle (@particles) {
        my $posStr = join(",", @{$particle->{p}});
        if ($positions{$posStr} == 1) {
            push @newParticles, $particle;
        }
    }
    @particles = @newParticles;
}

print scalar @particles;
