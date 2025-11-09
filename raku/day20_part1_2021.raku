
unit sub MAIN($file = 'input.txt');

my @lines = $file.IO.lines;
my $algo = @lines.shift;
@lines.shift; # blank line

my @img = @lines».comb;

sub enhance(@img, $flip) {
    my @out;
    my $R = @img.elems + 2;
    my $C = @img[0].elems + 2;
    for ^$R -> $i {
        my @row;
        for ^$C -> $j {
            my $idx = 0;
            for -1..1 -> $di {
                for -1..1 -> $dj {
                    $idx +<= 1;
                    my $ii = $i + $di - 1;
                    my $jj = $j + $dj - 1;
                    $idx += 1 if
                      ($ii < 0 || $ii >= @img.elems || $jj < 0 || $jj >= @img[0].elems)
                        ?? $flip
                        !! @img[$ii][$jj] eq '#';
                }
            }
            @row.push: $algo.substr($idx, 1);
        }
        @out.push: @row;
    }
    @out
}

my $flip = 0;
$flip = 1 if $algo.substr(0,1) eq '#' && $algo.substr(*-1,1) eq '.';
@img = enhance(@img, 0);
@img = enhance(@img, $flip);

say @img.map(*.grep('#').elems).sum;
