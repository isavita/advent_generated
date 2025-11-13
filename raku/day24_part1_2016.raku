
use v6;

sub find-distance(@map, $start, $finish) {
    my ($fx, $fy) = $finish.list;
    my @queue = ($start, 0);
    my uint32 @visited;
    my constant stride = 1000;
    while @queue {
        my ($x, $y) = @queue.shift.list;
        my $steps = @queue.shift;
        return $steps if $x == $fx && $y == $fy;
        for (1,0,-1,0,0,1,0,-1) -> $dx, $dy {
            my ($nx, $ny) = $x + $dx, $y + $dy;
            next if $nx < 0 || $ny < 0 || $ny >= @map || $nx >= @map[0].chars;
            next if substr(@map[$ny], $nx, 1) eq '#';
            my $key = $ny * stride + $nx;
            next if @visited[$key];
            @visited[$key] = 1;
            @queue.push: ($nx, $ny), $steps + 1;
        }
    }
}

sub find-shortest-path(@map) {
    my %loc;
    for @map.kv -> $y, $row {
        for $row.comb.kv -> $x, $char {
            %loc{$char} = ($x, $y) if $char ~~ /\d/
        }
    }
    my %dist;
    for %loc.keys -> $s {
        for %loc.keys -> $t {
            next if $s eq $t;
            %dist{$s}{$t} = %dist{$t}{$s} = find-distance(@map, %loc{$s}, %loc{$t})
        }
    }
    ([min] %loc.keys.permutations.map: -> @p {
        [+] @p.rotor(2 => -1).map: -> ($a, $b) { %dist{$a}{$b} }
    })
}

sub MAIN() {
    say find-shortest-path('input.txt'.IO.lines)
}
