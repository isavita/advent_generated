open(my $fh, '<', 'input.txt') or die "Cannot open file: $!";
my $line = <$fh>;
close($fh);

my @parts = split /, /, $line;
my @xRange = split /\.\./, substr($parts[0], 15);
my @yRange = split /\.\./, substr($parts[1], 2);
my ($xMin, $xMax, $yMin, $yMax) = (int($xRange[0]), int($xRange[1]), int($yRange[0]), int($yRange[1]));

my %velocities;
for my $xVel (-1000..1000) {
    for my $yVel (-1000..1000) {
        my ($xPos, $yPos) = (0, 0);
        my ($curXVel, $curYVel) = ($xVel, $yVel);
        my $inTargetArea = 0;
        while (1) {
            $xPos += $curXVel;
            $yPos += $curYVel;

            if ($xPos >= $xMin && $xPos <= $xMax && $yPos >= $yMin && $yPos <= $yMax) {
                $inTargetArea = 1;
                last;
            }

            if (isMovingAway($xPos, $yPos, $curXVel, $curYVel, $xMin, $xMax, $yMin, $yMax)) {
                last;
            }

            if ($curXVel > 0) {
                $curXVel--;
            } elsif ($curXVel < 0) {
                $curXVel++;
            }

            $curYVel--;
        }

        if ($inTargetArea) {
            my $velocityKey = "$xVel,$yVel";
            $velocities{$velocityKey} = 1;
        }
    }
}

print scalar(keys %velocities);

sub isMovingAway {
    my ($xPos, $yPos, $xVel, $yVel, $xMin, $xMax, $yMin, $yMax) = @_;
    return 1 if $xPos < $xMin && $xVel < 0;
    return 1 if $xPos > $xMax && $xVel > 0;
    return 1 if $yPos < $yMin && $yVel < 0;
    return 0;
}