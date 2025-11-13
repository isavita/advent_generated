
enum Direction <N E S W>;
my @dir-points = 3,0,1,2;
my @dx = -1,0,1,0;
my @dy = 0,1,0,-1;

sub cross-border($x,$y,$dir,$size) {
    my $s = $size;
    given ($x,$y,$dir) {
        when $x == -1 && $y < 2*$s { return ($y+2*$s,$x+1),E }
        when $x == -1 && $y >= 2*$s { return ($x+4*$s,$y-2*$s),N }
        when $x == $s && $dir == S { return ($y-$s,$x+$s-1),W }
        when $x == 2*$s-1 && $dir == N { return ($y+$s,$x-$s+1),E }
        when $x == 3*$s && $dir == S { return ($y+2*$s,$x-2*$s-1),W }
        when $x == 4*$s { return ($x-4*$s,$y+2*$s),S }
        when $y == -1 && $x < 3*$s { return (3*$s-1-$x,$y+$s+1),E }
        when $y == -1 && $x >= 3*$s { return ($y+1,$x-2*$s),S }
        when $y == $s-1 && $x < $s { return (3*$s-1-$x,$y-$s+1),E }
        when $y == $s-1 && $x >= $s && $dir == W { return ($y+$s+1,$x-$s),S }
        when $y == $s && $dir == E { return ($y+2*$s-1,$x-2*$s),N }
        when $y == 2*$s && $x < 2*$s && $dir == E { return ($y-$s-1,$x+$s),N }
        when $y == 2*$s && $x >= 2*$s { return (3*$s-1-$x,$y+$s-1),W }
        when $y == 3*$s { return (3*$s-1-$x,$y-$s-1),W }
        default { die "Not a border crossing" }
    }
}

sub parse-path($s) {
    my @m;
    my $acc = 0;
    for $s.comb -> $c {
        if $c eq 'R'|'L' {
            @m.push: $acc if $acc;
            $acc = 0;
            @m.push: $c;
        } else { $acc = $acc*10 + $c }
    }
    @m.push: $acc if $acc;
    @m
}

sub MAIN {
    my @lines = 'input.txt'.IO.lines;
    my $size = @lines[0].chars / 3;
    my %map;
    my $r = 0;
    for @lines -> $line {
        last if $line eq '';
        for $line.comb.kv -> $c,$char {
            given $char {
                when ' ' { }
                when '#' { %map{"$r,$c"} = True }
                when '.' { %map{"$r,$c"} = False }
            }
        }
        $r++;
    }
    my @moves = parse-path(@lines[*-1]);
    my ($x,$y) = 0,$size;
    my $dir = E;
    for @moves -> $m {
        if $m eq 'R' { $dir = ($dir+1)%4 }
        elsif $m eq 'L' { $dir = ($dir-1+4)%4 }
        else {
            for ^$m {
                my $nx = $x + @dx[$dir];
                my $ny = $y + @dy[$dir];
                my $nd = $dir;
                unless %map{"$nx,$ny"}:exists {
                    my $pair;
                    ($pair,$nd) = cross-border($nx,$ny,$dir,$size);
                    ($nx,$ny) = $pair;
                }
                last if %map{"$nx,$ny"}:exists && %map{"$nx,$ny"};
                ($x,$y,$dir) = ($nx,$ny,$nd);
            }
        }
    }
    say 1000*($x+1) + 4*($y+1) + @dir-points[$dir];
}
