
sub swap-positions(@pw, $x, $y) { @pw[$x, $y] = @pw[$y, $x] }

sub swap-letters(@pw, $x, $y) {
    my $xi = @pw.first($x, :k);
    my $yi = @pw.first($y, :k);
    swap-positions(@pw, $xi, $yi);
}

sub rotate(@pw, $steps is copy) {
    my $len = +@pw;
    $steps %= $len;
    $steps += $len if $steps < 0;
    @pw = |@pw[ *-$steps .. *-1 ], |@pw[ 0 .. *-$steps-1 ];
}

sub rotate-letter(@pw, $x) {
    my $i = @pw.first($x, :k);
    $i += 1 if $i >= 4;
    rotate(@pw, $i + 1);
}

sub derotate-letter(@pw, $x) {
    my $i = @pw.first($x, :k);
    my $rot = $i % 2 ?? -($i + 1) div 2
                    !! $i ?? (6 - $i) div 2 !! -1;
    rotate(@pw, $rot);
}

sub reverse-sub(@pw, $x, $y) { @pw[$x..$y] = @pw[$x..$y].reverse }

sub move(@pw, $x, $y) { @pw.splice($y, 0, @pw.splice($x, 1)[0]) }

sub scramble(@pw is copy, @instr, $dir) {
    @instr = @instr.reverse if $dir < 0;
    for @instr {
        when /swap/ {
            my @t = .words;
            if @t[1] eq 'position' {
                swap-positions(@pw, +@t[2], +@t[*-1]);
            } else {
                swap-letters(@pw, @t[2].substr(0, 1), @t[*-1].substr(0, 1));
            }
        }
        when /rotate/ {
            my @t = .words;
            if @t[1] eq 'based' {
                my $c = @t[*-1].substr(0, 1);
                $dir > 0 ?? rotate-letter(@pw, $c) !! derotate-letter(@pw, $c);
            } else {
                my $s = +@t[2];
                $s = -$s if @t[1] eq 'left';
                $s = -$s if $dir < 0;
                rotate(@pw, $s);
            }
        }
        when /reverse/ {
            my @t = .words;
            reverse-sub(@pw, +@t[2], +@t[*-1]);
        }
        when /move/ {
            my @t = .words;
            my ($x, $y) = +@t[2], +@t[*-1];
            ($x, $y) = ($y, $x) if $dir < 0;
            move(@pw, $x, $y);
        }
    }
    @pw
}

sub MAIN {
    my @instr = 'input.txt'.IO.lines;
    my @pw = 'fbgdceah'.comb;
    put scramble(@pw, @instr, -1).join;
}
