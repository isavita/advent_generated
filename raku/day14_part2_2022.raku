
use v6;

sub MAIN() {
    my $input = slurp 'input.txt';
    my @matrix := parse-input($input);
    my $origin-col = 0;
    for ^@matrix[0] -> $i {
        $origin-col = $i if @matrix[0][$i] eq '+';
        @matrix[*-1][$i] = '#';
    }
    my $ans = 0;
    while !drop-sand(@matrix, $origin-col) {
        $ans++;
        last if @matrix[0][$origin-col] eq 'o';
    }
    say $ans;
}

sub parse-input(Str $input) {
    my @coord-sets;
    my $lowest-col = ∞;
    my $highest-row = 0;
    for $input.lines -> $line {
        my @coords;
        for $line.split(' -> ') -> $raw {
            my ($col,$row) = $raw.split(',').map: +*;
            @coords.push: [$col,$row];
            $lowest-col min= $col;
            $highest-row max= $row;
        }
        @coord-sets.push: @coords;
    }
    my $extra = 200;
    my $highest-col = 0;
    for @coord-sets -> @set {
        for @set -> $p {
            $p[0] -= $lowest-col - $extra;
            $highest-col max= $p[0];
        }
    }
    my @m = [ ['.' xx $highest-col + $extra * 2] xx $highest-row + 3 ];
    for @coord-sets -> @set {
        for 1..^@set -> $i {
            my @cols = (@set[$i-1][0], @set[$i][0]).sort;
            my @rows = (@set[$i-1][1], @set[$i][1]).sort;
            if @cols[0] == @cols[1] {
                @m[$_][@cols[0]] = '#' for @rows[0]..@rows[1];
            } else {
                @m[@rows[0]][$_] = '#' for @cols[0]..@cols[1];
            }
        }
    }
    my $origin = 500 - $lowest-col + $extra;
    @m[0][$origin] = '+';
    @m;
}

sub drop-sand(@m, Int $oc) {
    my ($r,$c) = 0,$oc;
    while $r < @m - 1 {
        if @m[$r+1][$c] eq '.' {
            $r++;
        } elsif @m[$r+1][$c-1] eq '.' {
            $r++; $c--;
        } elsif @m[$r+1][$c+1] eq '.' {
            $r++; $c++;
        } else {
            @m[$r][$c] = 'o';
            return False;
        }
    }
    True;
}
