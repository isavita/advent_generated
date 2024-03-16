open(my $fh, '<', 'input.txt') or die "Cannot open file: $!";
my $target = <$fh>;
close($fh);

chomp($target);
my %grid = ();
$grid{join(',', 0, 0)} = 1;

my ($x, $y) = (0, 0);
my ($dx, $dy) = (0, -1);

while (1) {
    if ($x == $y || ($x < 0 && $x == -$y) || ($x > 0 && $x == 1-$y)) {
        ($dx, $dy) = (-$dy, $dx);
    }

    $x += $dx;
    $y += $dy;

    my $value = 0;
    for my $dx (-1..1) {
        for my $dy (-1..1) {
            $value += $grid{join(',', $x + $dx, $y + $dy)};
        }
    }
    $grid{join(',', $x, $y)} = $value;

    if ($value > $target) {
        print "$value\n";
        last;
    }
}