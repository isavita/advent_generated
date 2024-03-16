use strict;
use warnings;

my $gridSize = 1000;
my @grid = ();
for (my $i = 0; $i < $gridSize; $i++) {
    for (my $j = 0; $j < $gridSize; $j++) {
        $grid[$i][$j] = 0;
    }
}

open(my $fh, '<', 'input.txt') or die "Error opening file: $!";
while (my $instruction = <$fh>) {
    chomp $instruction;
    processInstruction($instruction, \@grid);
}
close($fh);

print totalBrightness(\@grid) . "\n";

sub processInstruction {
    my ($instruction, $grid) = @_;
    my @parts = split(' ', $instruction);
    my ($startX, $startY) = $parts[-3] =~ /(\d+),(\d+)/;
    my ($endX, $endY) = $parts[-1] =~ /(\d+),(\d+)/;

    for my $x ($startX..$endX) {
        for my $y ($startY..$endY) {
            if ($instruction =~ /^turn on/) {
                $grid->[$x][$y]++;
            } elsif ($instruction =~ /^turn off/) {
                $grid->[$x][$y]-- if $grid->[$x][$y] > 0;
            } elsif ($instruction =~ /^toggle/) {
                $grid->[$x][$y] += 2;
            }
        }
    }
}

sub totalBrightness {
    my ($grid) = @_;
    my $brightness = 0;
    for my $row (@$grid) {
        for my $light (@$row) {
            $brightness += $light;
        }
    }
    return $brightness;
}