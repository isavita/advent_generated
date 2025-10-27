
sub MAIN() {
    my @grid = 'input.txt'.IO.lines;
    my \h = +@grid;
    my \w = @grid[0].chars;
    my @cache;
    my @dir = ((1,0),(-1,0),(0,1),(0,-1));
    sub idx(\dx,\dy) { ((dx,dy) eqv (1,0))??0!!((dx,dy) eqv (-1,0))??1!!((dx,dy) eqv (0,1))??2!!3 }
    sub sim(\sx,\sy,\dx,\dy) {
        my @en;
        my @vis;
        my @q;
        @q.push: (sx,sy,dx,dy);
        while @q {
            my ($x,$y,$dx,$dy) = @q.shift;
            my ($nx,$ny) = ($x+$dx,$y+$dy);
            next if $nx < 0 || $nx >= w || $ny < 0 || $ny >= h;
            my $d = idx($dx,$dy);
            next if @vis[$ny][$nx][$d];
            @vis[$ny][$nx][$d] = True;
            @en[$ny][$nx] = True;
            given @grid[$ny].substr($nx,1) {
                when '.' { @q.push: ($nx,$ny,$dx,$dy) }
                when '/' { my ($ndx,$ndy) = (-$dy,-$dx); @q.push: ($nx,$ny,$ndx,$ndy) }
                when '\\' { my ($ndx,$ndy) = ($dy,$dx); @q.push: ($nx,$ny,$ndx,$ndy) }
                when '|' { if $dx != 0 { @q.push: ($nx,$ny,0,1); @q.push: ($nx,$ny,0,-1) } else { @q.push: ($nx,$ny,$dx,$dy) } }
                when '-' { if $dy != 0 { @q.push: ($nx,$ny,1,0); @q.push: ($nx,$ny,-1,0) } else { @q.push: ($nx,$ny,$dx,$dy) } }
            }
        }
        return +@en.map({ |$_ }).grep: *.so
    }
    my $max = 0;
    for ^w -> \x { $max max= sim(x,-1,0,1); $max max= sim(x,h,0,-1) }
    for ^h -> \y { $max max= sim(-1,y,1,0); $max max= sim(w,y,-1,0) }
    say $max
}
