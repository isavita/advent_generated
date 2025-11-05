
use v6.d;

constant SIZE = 50;

sub calculate-hash(@grid) {
    my int $hash = 0;
    for ^SIZE X ^SIZE -> ($i,$j) {
        $hash = $hash * 31 + @grid[$i][$j].ord;
    }
    $hash
}

sub MAIN() {
    my @grid = 'input.txt'.IO.lines.map: {.comb.Array};
    my @new = [[ '.' xx SIZE ] xx SIZE];

    my %seen;
    my int $minute = 0;
    my int $cycle-start;
    my int $cycle-len;

    loop {
        my $sig = calculate-hash(@grid);
        if %seen{$sig}:exists {
            my $idx = %seen{$sig};
            $cycle-start = $idx;
            $cycle-len   = $minute - $idx;
            last;
        }
        %seen{$sig} = $minute;

        for ^SIZE X ^SIZE -> ($i,$j) {
            my int $t = 0;
            my int $l = 0;
            for (-1..1) X (-1..1) -> ($x,$y) {
                next if $x == 0 && $y == 0;
                my int $ni = $i + $x;
                my int $nj = $j + $y;
                next unless 0 <= $ni < SIZE && 0 <= $nj < SIZE;
                $t++ if @grid[$ni][$nj] eq '|';
                $l++ if @grid[$ni][$nj] eq '#';
            }

            given @grid[$i][$j] {
                when '.' { @new[$i][$j] = $t >= 3 ?? '|' !! '.' }
                when '|' { @new[$i][$j] = $l >= 3 ?? '#' !! '|' }
                when '#' { @new[$i][$j] = $l && $t ?? '#' !! '.' }
            }
        }
        @grid = @new.deepmap: *.self;
        $minute++;
    }

    my int $rem = (1_000_000_000 - $cycle-start) % $cycle-len;
    $rem.say;
    for ^$rem {
        for ^SIZE X ^SIZE -> ($i,$j) {
            my int $t = 0;
            my int $l = 0;
            for (-1..1) X (-1..1) -> ($x,$y) {
                next if $x == 0 && $y == 0;
                my int $ni = $i + $x;
                my int $nj = $j + $y;
                next unless 0 <= $ni < SIZE && 0 <= $nj < SIZE;
                $t++ if @grid[$ni][$nj] eq '|';
                $l++ if @grid[$ni][$nj] eq '#';
            }

            given @grid[$i][$j] {
                when '.' { @new[$i][$j] = $t >= 3 ?? '|' !! '.' }
                when '|' { @new[$i][$j] = $l >= 3 ?? '#' !! '|' }
                when '#' { @new[$i][$j] = $l && $t ?? '#' !! '.' }
            }
        }
        @grid = @new.deepmap: *.self;
    }

    my int $wooded = 0;
    my int $lumber = 0;
    for ^SIZE X ^SIZE -> ($i,$j) {
        $wooded++ if @grid[$i][$j] eq '|';
        $lumber++  if @grid[$i][$j] eq '#';
    }
    say $wooded * $lumber;
}
