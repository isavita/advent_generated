
sub mod(Int $a, Int $b) { (($a % $b) + $b) % $b }

sub parse-line(Str $line) {
    $line ~~ /p\= (\-?\d+) ',' (\-?\d+) \s+ v\= (\-?\d+) ',' (\-?\d+)/;
    { x => $0.Int, y => $1.Int, vx => $2.Int, vy => $3.Int }
}

sub move-robots(@robots, Int $sizeX, Int $sizeY) {
    for @robots -> $r {
        $r<x> = mod($r<x> + $r<vx>, $sizeX);
        $r<y> = mod($r<y> + $r<vy>, $sizeY);
    }
}

sub count-quadrants(@robots, Int $sizeX, Int $sizeY) {
    my @counts = 0 xx 4;
    my $centerX = $sizeX div 2;
    my $centerY = $sizeY div 2;
    for @robots {
        my $x = $_<x>;
        my $y = $_<y>;
        if $x < $centerX {
            if $y < $centerY { @counts[0]++ }
            elsif $y > $centerY { @counts[1]++ }
        } elsif $x > $centerX {
            if $y < $centerY { @counts[2]++ }
            elsif $y > $centerY { @counts[3]++ }
        }
    }
    @counts
}

sub has-no-overlaps(@robots) {
    my %seen;
    for @robots {
        my $key = "$_<x>,$_<y>";
        return False if %seen{$key}:exists;
        %seen{$key} = 1;
    }
    True
}

sub MAIN {
    my ($sizeX, $sizeY) = (101, 103);
    my @lines = 'input.txt'.IO.lines.grep(*.trim ne '');
    my @robots = @lines.map(&parse-line);

    my @part1 = @robots.deepmap(*.clone);
    move-robots(@part1, $sizeX, $sizeY) for ^100;
    my $safety = [*] count-quadrants(@part1, $sizeX, $sizeY);
    say $safety;

    my @part2 = @robots.deepmap(*.clone);
    my $sec = 0;
    while !has-no-overlaps(@part2) {
        move-robots(@part2, $sizeX, $sizeY);
        $sec++;
        die "Too many iterations" if $sec > 1_000_000;
    }
    say $sec;
}
