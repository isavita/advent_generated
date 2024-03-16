use strict;
use warnings;

my @neighbors4 = ([0, 1], [0, -1], [1, 0], [-1, 0]);
my %grid;
my %visible;

open(my $fh, '<', 'input.txt') or die $!;

my $y = 0;
while (my $line = <$fh>) {
    chomp $line;
    my @chars = split('', $line);
    my $x = 0;
    foreach my $char (@chars) {
        $grid{"$x,$y"} = ord($char) - ord('0');
        $x++;
    }
    $y++;
}

close($fh);

foreach my $point (keys %grid) {
    foreach my $neighbor (@neighbors4) {
        my ($dx, $dy) = @$neighbor;
        my ($px, $py) = split(',', $point);
        my $nx = $px + $dx;
        my $ny = $py + $dy;
        while (exists $grid{"$nx,$ny"}) {
            if ($grid{"$nx,$ny"} >= $grid{$point}) {
                last;
            }
            $nx += $dx;
            $ny += $dy;
        }
        $visible{$point} = 1 unless exists $grid{"$nx,$ny"};
    }
}

print scalar keys %visible;