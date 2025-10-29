#!/usr/bin/env raku
my @grid = "input.txt".IO.lines.map({ .comb });
sub count-adjacent(@g, $i, $j, $t) {
    my $c = 0;
    for -1..1 -> $dx {
        for -1..1 -> $dy {
            next if $dx == 0 && $dy == 0;
            my $x = $i + $dx;
            my $y = $j + $dy;
            $c++ if $x ~~ 0..@g.end && $y ~~ 0..@g[0].end && @g[$x][$y] eq $t;
        }
    }
    $c;
}
sub next-state(@g, $i, $j) {
    given @g[$i][$j] {
        when '.' { count-adjacent(@g,$i,$j,'|') >= 3 ?? '|' !! '.' }
        when '|' { count-adjacent(@g,$i,$j,'#') >= 3 ?? '#' !! '|' }
        when '#' {
            my $l = count-adjacent(@g,$i,$j,'#');
            my $t = count-adjacent(@g,$i,$j,'|');
            ($l >= 1 && $t >= 1) ?? '#' !! '.';
        }
    }
}
for ^10 {
    my @new = @grid.map({ Array.new(@grid[0].elems) });
    for ^@grid.elems -> $i {
        for ^@grid[0].elems -> $j {
            @new[$i][$j] = next-state(@grid,$i,$j);
        }
    }
    @grid = @new;
}
my $wood = 0;
my $lumber = 0;
for @grid -> @row {
    for @row -> $c {
        $wood++   if $c eq '|';
        $lumber++ if $c eq '#';
    }
}
say $wood * $lumber;