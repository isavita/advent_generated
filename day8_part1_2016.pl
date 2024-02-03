
use strict;
use warnings;

my $screenWidth = 50;
my $screenHeight = 6;

my @screen;
for (my $i = 0; $i < $screenHeight; $i++) {
    $screen[$i] = [(0) x $screenWidth];
}

open(my $fh, '<', 'input.txt') or die $!;
while (my $line = <$fh>) {
    chomp $line;
    processInstruction($line, \@screen);
}

print countLitPixels(\@screen) . "\n";

sub processInstruction {
    my ($instruction, $screen) = @_;
    my $rectRegex = qr/rect (\d+)x(\d+)/;
    my $rotateRowRegex = qr/rotate row y=(\d+) by (\d+)/;
    my $rotateColumnRegex = qr/rotate column x=(\d+) by (\d+)/;

    if ($instruction =~ /$rectRegex/) {
        my ($a, $b) = ($1, $2);
        rect($screen, $a, $b);
    }
    elsif ($instruction =~ /$rotateRowRegex/) {
        my ($a, $b) = ($1, $2);
        rotateRow($screen, $a, $b);
    }
    elsif ($instruction =~ /$rotateColumnRegex/) {
        my ($a, $b) = ($1, $2);
        rotateColumn($screen, $a, $b);
    }
}

sub rect {
    my ($screen, $a, $b) = @_;
    for (my $y = 0; $y < $b; $y++) {
        for (my $x = 0; $x < $a; $x++) {
            $screen->[$y][$x] = 1;
        }
    }
}

sub rotateRow {
    my ($screen, $row, $shift) = @_;
    my @temp;
    for (my $i = 0; $i < $screenWidth; $i++) {
        $temp[($i + $shift) % $screenWidth] = $screen->[$row][$i];
    }
    $screen->[$row] = \@temp;
}

sub rotateColumn {
    my ($screen, $col, $shift) = @_;
    my @temp;
    for (my $i = 0; $i < $screenHeight; $i++) {
        $temp[($i + $shift) % $screenHeight] = $screen->[$i][$col];
    }
    for (my $i = 0; $i < $screenHeight; $i++) {
        $screen->[$i][$col] = $temp[$i];
    }
}

sub countLitPixels {
    my ($screen) = @_;
    my $count = 0;
    for my $row (@$screen) {
        for my $pixel (@$row) {
            $count++ if $pixel;
        }
    }
    return $count;
}
