
my constant \MAX = 50;
my constant \CYCLES = 6;

my @grid;
for 'input.txt'.IO.lines.kv -> $y,$line {
    for $line.comb.kv -> $x,$c {
        @grid[$x+MAX/2][$y+MAX/2][0+MAX/2] = 1 if $c eq '#';
    }
}

for ^CYCLES {
    my @next;
    my int ($minx,$maxx,$miny,$maxy,$minz,$maxz) = MAX,0,MAX,0,MAX,0;
    for ^(2*MAX) -> $x {
        for ^(2*MAX) -> $y {
            for ^(2*MAX) -> $z {
                next unless @grid[$x][$y][$z];
                $minx min= $x; $maxx max= $x;
                $miny min= $y; $maxy max= $y;
                $minz min= $z; $maxz max= $z;
            }
        }
    }
    for max(0,$minx-1)..min(2*MAX-1,$maxx+1) -> $x {
        for max(0,$miny-1)..min(2*MAX-1,$maxy+1) -> $y {
            for max(0,$minz-1)..min(2*MAX-1,$maxz+1) -> $z {
                my $n = 0;
                for -1..1 -> $dx {
                    for -1..1 -> $dy {
                        for -1..1 -> $dz {
                            next if $dx==0&&$dy==0&&$dz==0;
                            $n += @grid[$x+$dx][$y+$dy][$z+$dz] // 0;
                        }
                    }
                }
                @next[$x][$y][$z] = 1 if $n == 3 || ($n == 2 && @grid[$x][$y][$z]);
            }
        }
    }
    @grid = @next;
}

say [+] @grid.map({|$_}).map({|$_}).grep: so *;
