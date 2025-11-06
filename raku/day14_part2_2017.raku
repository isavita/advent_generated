
use v6;

sub reverse-section(@arr is copy, Int $start, Int $length) {
    my $n = @arr.elems;
    loop (my $i = $start, my $j = $start + $length - 1; $i < $j; $i++, $j--) {
        @arr[$i % $n, $j % $n] = @arr[$j % $n, $i % $n];
    }
    @arr
}

sub knot-hash(Str $input) {
    my @lengths = $input.ords;
    @lengths.append: 17, 31, 73, 47, 23;
    my @list = 0..255;
    my $position = 0;
    my $skip = 0;
    for ^64 {
        for @lengths -> $length {
            @list = reverse-section(@list, $position, $length);
            $position += $length + $skip++;
        }
    }
    my @dense;
    for ^16 -> $i {
        my $xor = 0;
        $xor +^= @list[$i * 16 + $_] for ^16;
        @dense.push: $xor;
    }
    @dense.map({ .base(16).fmt('%02s') }).join
}

sub hex-to-binary(Str $hex) {
    $hex.comb.map({ .parse-base(16).base(2).fmt('%04s') }).join
}

sub dfs(Int $x, Int $y, @grid) {
    return if $x < 0 || $x >= 128 || $y < 0 || $y >= 128 || @grid[$x][$y] != 1;
    @grid[$x][$y] = 0;
    dfs($x - 1, $y, @grid);
    dfs($x + 1, $y, @grid);
    dfs($x, $y - 1, @grid);
    dfs($x, $y + 1, @grid);
}

sub MAIN {
    my $data = 'input.txt'.IO.slurp.trim;
    my @grid = [0 xx 128] xx 128;
    my $regions = 0;
    for ^128 -> $i {
        my $row-key = "$data-$i";
        my $hash = knot-hash($row-key);
        my $binary = hex-to-binary($hash);
        for ^128 -> $j {
            @grid[$i][$j] = $binary.substr($j, 1).Int;
        }
    }
    for ^128 -> $i {
        for ^128 -> $j {
            if @grid[$i][$j] == 1 {
                $regions++;
                dfs($i, $j, @grid);
            }
        }
    }
    say $regions;
}
