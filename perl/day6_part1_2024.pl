
open my $f, '<', 'input.txt' or die $!;
my @grid = map { [split //] } <$f>;
close $f;

my ($h, $w) = (scalar @grid, scalar @{$grid[0]});
my ($x, $y, $dirX, $dirY, $dirIdx);
my @dirs = ([0, -1], [1, 0], [0, 1], [-1, 0]);

for my $i (0 .. $h - 1) {
  for my $j (0 .. $w - 1) {
    if ($grid[$i][$j] =~ /[\^><v]/) {
      ($x, $y) = ($j, $i);
      $dirIdx = index('^>v<', $grid[$i][$j]);
      ($dirX, $dirY) = @{$dirs[$dirIdx]};
      last;
    }
  }
}

my %visited;
$visited{"$x,$y"} = 1;

while (1) {
  my ($nx, $ny) = ($x + $dirX, $y + $dirY);
  last if $nx < 0 || $nx >= $w || $ny < 0 || $ny >= $h;
  if ($grid[$ny][$nx] eq '#') {
    $dirIdx = ($dirIdx + 1) % 4;
    ($dirX, $dirY) = @{$dirs[$dirIdx]};
    next;
  }
  ($x, $y) = ($nx, $ny);
  $visited{"$x,$y"} = 1;
}

print scalar keys %visited;
