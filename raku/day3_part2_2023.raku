
sub MAIN {
    my @grid = 'input.txt'.IO.lines;
    my ($h,$w) = +@grid, @grid[0].chars;
    my ($sum-parts,$sum-gears) = 0,0;

    my @schematic = @grid.map: *.comb;
    my @used = [False xx $w] xx $h;

    for ^$h X ^$w -> ($r,$c) {
        next unless @schematic[$r][$c] eq '*';

        my Int @nums;
        for (-1..1) X (-1..1) -> ($dr,$dc) {
            my ($nr,$nc) = ($r+$dr,$c+$dc);
            next unless 0 <= $nr < $h && 0 <= $nc < $w;
            next unless @schematic[$nr][$nc] ~~ /\d/ && !@used[$nr][$nc];

            # walk to start of number
            my $left = $nc;
            $left-- while $left > 0 && @schematic[$nr][$left-1] ~~ /\d/;
            # mark digits used
            my $right = $left;
            $right++ while $right < $w && @schematic[$nr][$right] ~~ /\d/;
            @used[$nr][$left..^$right] = True xx ($right-$left);

            push @nums, +@schematic[$nr].join.substr($left,$right-$left);
        }
        $sum-gears += [*] @nums if @nums == 2;
    }

    @used = [False xx $w] xx $h;
    for ^$h X ^$w -> ($r,$c) {
        next unless @schematic[$r][$c] ~~ /\d/ && !@used[$r][$c];

        my $left  = $c;
        my $right = $c;
        $left--  while $left  > 0      && @schematic[$r][$left-1]  ~~ /\d/;
        $right++ while $right < $w-1   && @schematic[$r][$right+1] ~~ /\d/;

        my $part = False;
        for ($left-1..$right+1) X ($r-1..$r+1) -> ($x,$y) {
            next unless 0 <= $y < $h && 0 <= $x < $w;
            $part ||= @schematic[$y][$x] !~~ /\d|\.|\s/;
        }

        if $part {
            my $n = +@schematic[$r].join.substr($left,$right-$left+1);
            $sum-parts += $n;
            @used[$r][$left..$right] = True xx ($right-$left+1);
        }
    }

    put $sum-parts;
    put $sum-gears;
}
