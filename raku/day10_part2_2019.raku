
use v6;

sub read-asteroids($file) {
    my @grid = $file.IO.lines>>.comb;
    my @coords;
    for @grid.kv -> $y, $row {
        for $row.kv -> $x, $cell {
            @coords.push: [$x, $y] if $cell eq '#';
        }
    }
    @coords
}

sub angle-dist($sx, $sy, $tx, $ty) {
    my $dx = $tx - $sx;
    my $dy = $ty - $sy;
    my $angle = atan2($dy, $dx);
    $angle += 2 * π if $angle < -π/2;
    my $dist = ($dx * $dx + $dy * $dy).sqrt;
    ($angle, $dist)
}

sub find-best(@coords) {
    my ($bx, $by, $bc) = 0, 0, 0;
    for @coords -> @s {
        my ($sx, $sy) = @s;
        my %seen;
        for @coords -> @t {
            next if @s eqv @t;
            my ($dx, $dy) = @t[0] - $sx, @t[1] - $sy;
            my $g = $dx gcd $dy;
            %seen{"{$dx/$g},{$dy/$g}"}++;
        }
        my $c = %seen.elems;
        if $c > $bc { ($bx, $by, $bc) = $sx, $sy, $c }
    }
    ($bx, $by, $bc)
}

sub vaporize(@coords, $sx, $sy) {
    my @targets = gather for @coords -> @t {
        next if @t[0] == $sx && @t[1] == $sy;
        my ($a, $d) = angle-dist($sx, $sy, |@t);
        take [flat @t, $a, $d];
    }
    my @out;
    while @targets {
        my @next;
        my $last = -Inf;
        for @targets.sort(*.[2]) -> @t {
            if @t[2] != $last {
                @out.push: @t;
                $last = @t[2];
            } else {
                @next.push: @t;
            }
        }
        @targets = @next;
    }
    @out
}

sub MAIN {
    my @coords = read-asteroids('input.txt');
    my ($bx, $by) = find-best(@coords);
    my @v = vaporize(@coords, $bx, $by);
    say @v[199][0] * 100 + @v[199][1] if @v.elems >= 200;
}
