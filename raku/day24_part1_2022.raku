
my @grid = 'input.txt'.IO.lines».comb;
my \rows = +@grid;
my \cols = +@grid[0];
my @bliz = ['^','>','v','<'];
my @dx   = [0,0,1,-1,0];
my @dy   = [1,-1,0,0,0];

sub steps($sx,$sy,$ex,$ey,$init) {
    my %seen;
    my @q = [($sx,$sy,$init)],;
    while @q {
        my ($x,$y,$step) = @q.shift;
        return $step if $x == $ex && $y == $ey;
        for 0..4 -> $i {
            my ($nx,$ny) = ($x + @dx[$i], $y + @dy[$i]);
            my $ns = $step + 1;
            next if $nx < 0 || $nx >= cols || $ny < 0 || $ny >= rows;
            next if @grid[$ny][$nx] eq '#';
            my $key = "$nx,$ny,{$ns % (rows*cols)}";
            next if %seen{$key}++;
            if 0 < $ny < rows-1 {
                my $ok = True;
                for @bliz.kv -> $j,$b {
                    my ($px,$py);
                    if $b eq '^' {
                        $px = $nx;
                        $py = ($ny + $ns) % (rows-2);
                        $py = rows-2 if $py == 0;
                    } elsif $b eq '>' {
                        $px = ($nx - $ns) % (cols-2);
                        $px += cols-2 if $px < 0;
                        $px = cols-2 if $px == 0;
                        $py = $ny;
                    } elsif $b eq 'v' {
                        $px = $nx;
                        $py = ($ny - $ns) % (rows-2);
                        $py += rows-2 if $py < 0;
                        $py = rows-2 if $py == 0;
                    } else {
                        $px = ($nx + $ns) % (cols-2);
                        $px = cols-2 if $px == 0;
                        $py = $ny;
                    }
                    if @grid[$py][$px] eq $b { $ok = False; last }
                }
                next unless $ok;
            }
            @q.push: ($nx,$ny,$ns);
        }
    }
    -1
}

say steps(1,0,cols-2,rows-1,0);
