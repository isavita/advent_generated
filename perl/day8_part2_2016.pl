use strict;
use warnings;

my $screenWidth = 50;
my $screenHeight = 6;
my @screen;

open(my $fh, '<', 'input.txt') or die "Cannot open file: $!";

while (my $line = <$fh>) {
    chomp($line);
    processInstruction($line, \@screen);
}

displayScreen(\@screen);

sub displayScreen {
    my $screen = shift;
    foreach my $row (@$screen) {
        foreach my $pixel (@$row) {
            if ($pixel) {
                print "#";
            } else {
                print ".";
            }
        }
        print "\n";
    }
}

sub processInstruction {
    my ($instruction, $screen) = @_;

    if ($instruction =~ /rect (\d+)x(\d+)/) {
        my $a = $1;
        my $b = $2;
        rect($screen, $a, $b);
    } elsif ($instruction =~ /rotate row y=(\d+) by (\d+)/) {
        my $row = $1;
        my $shift = $2;
        rotateRow($screen, $row, $shift);
    } elsif ($instruction =~ /rotate column x=(\d+) by (\d+)/) {
        my $col = $1;
        my $shift = $2;
        rotateColumn($screen, $col, $shift);
    }
}

sub rect {
    my ($screen, $a, $b) = @_;
    for my $y (0..$b-1) {
        for my $x (0..$a-1) {
            $screen->[$y][$x] = 1;
        }
    }
}

sub rotateRow {
    my ($screen, $row, $shift) = @_;
    my @temp;
    foreach my $i (0..$screenWidth-1) {
        $temp[($i+$shift)%$screenWidth] = $screen->[$row][$i];
    }
    $screen->[$row] = \@temp;
}

sub rotateColumn {
    my ($screen, $col, $shift) = @_;
    my @temp;
    foreach my $i (0..$screenHeight-1) {
        $temp[($i+$shift)%$screenHeight] = $screen->[$i][$col];
    }
    foreach my $i (0..$screenHeight-1) {
        $screen->[$i][$col] = $temp[$i];
    }
}

close($fh);