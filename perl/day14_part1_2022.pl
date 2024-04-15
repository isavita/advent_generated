use strict;
use warnings;

my %grid;
my $floor;

open(my $fh, '<', 'input.txt') or die "Could not open file 'input.txt': $!";
while (my $line = <$fh>) {
    chomp $line;
    my @pts = map { [split /,/] } split / -> /, $line;
    for my $i (0 .. $#pts - 1) {
        if ($pts[$i][0] == $pts[$i+1][0]) {
            for my $y (min($pts[$i][1], $pts[$i+1][1]) .. max($pts[$i][1], $pts[$i+1][1])) {
                $grid{$pts[$i][0]}{$y} = 1;
            }
        } else {
            for my $x (min($pts[$i][0], $pts[$i+1][0]) .. max($pts[$i][0], $pts[$i+1][0])) {
                $grid{$x}{$pts[$i][1]} = 1;
            }
        }
    }
}
close $fh;

$floor = (sort { $b <=> $a } map { (keys %{$grid{$_}})[0] } keys %grid)[0] + 2;

my ($sands, $firstFloorTouch) = (0, 0);
while (!exists $grid{500}{0}) {
    my ($x, $y) = (500, 0);
    while (1) {
        if ($y == $floor - 1) {
            $firstFloorTouch = $sands if $firstFloorTouch == 0;
            $grid{$x}{$y} = 1;
            last;
        }
        if (!exists $grid{$x}{$y+1}) {
            $y++;
        } elsif (!exists $grid{$x-1}{$y+1}) {
            $x--; $y++;
        } elsif (!exists $grid{$x+1}{$y+1}) {
            $x++; $y++;
        } else {
            $grid{$x}{$y} = 1;
            last;
        }
    }
    $sands++;
}

print "$firstFloorTouch\n";

sub min { $_[0] < $_[1] ? $_[0] : $_[1] }
sub max { $_[0] > $_[1] ? $_[0] : $_[1] }