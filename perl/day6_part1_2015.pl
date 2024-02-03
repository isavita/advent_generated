
use strict;
use warnings;

my $gridSize = 1000;
my @grid;

open(my $fh, '<', 'input.txt') or die "Error opening file: $!";
while (my $instruction = <$fh>) {
    processInstruction($instruction, \@grid);
}

sub processInstruction {
    my ($instruction, $grid) = @_;
    my @parts = split(' ', $instruction);
    my ($startX, $startY) = $parts[-3] =~ /(\d+),(\d+)/;
    my ($endX, $endY) = $parts[-1] =~ /(\d+),(\d+)/;

    for my $x ($startX..$endX) {
        for my $y ($startY..$endY) {
            if ($instruction =~ /^turn on/) {
                $grid->[$x][$y] = 1;
            }
            elsif ($instruction =~ /^turn off/) {
                $grid->[$x][$y] = 0;
            }
            elsif ($instruction =~ /^toggle/) {
                $grid->[$x][$y] = $grid->[$x][$y] ? 0 : 1;
            }
        }
    }
}

sub countLights {
    my $grid = shift;
    my $count = 0;
    for my $row (@$grid) {
        for my $light (@$row) {
            $count++ if $light;
        }
    }
    return $count;
}

print countLights(\@grid) . "\n";
