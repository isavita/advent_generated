
unit sub MAIN($file = 'input.txt');

my $offset = 10_000_000_000_000;

sub parse-coord($s) { + $s.comb(/[\-\+]?\d+/).head }

sub parse-point($s) {
    my $i = $s.index(',');
    (parse-coord($s.substr(0,$i)), parse-coord($s.substr($i+1)))
}

my @machines;
for $file.IO.slurp.split("\n\n") -> $chunk {
    my @lines = $chunk.lines;
    my ($ax,$ay) = parse-point(@lines[0]);
    my ($bx,$by) = parse-point(@lines[1]);
    my ($px,$py) = parse-point(@lines[2]);
    push @machines, [ $ax, $ay, $bx, $by, $px + $offset, $py + $offset ];
}

my $total = 0;
for @machines -> [$ax,$ay,$bx,$by,$px,$py] {
    my $D = $ax*$by - $ay*$bx;
    next unless $D;
    my $numA = $px*$by - $py*$bx;
    my $numB = -$px*$ay + $py*$ax;
    next unless $numA %% $D && $numB %% $D;
    my $a = $numA div $D;
    my $b = $numB div $D;
    next if $a < 0 || $b < 0;
    $total += 3*$a + $b;
}
put $total;
