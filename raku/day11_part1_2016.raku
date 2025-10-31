
use v6;
use List::Util <any>;

sub parse-input {
    my @floors = ([], [], [], []);
    my %elem;
    my $idx = 0;
    my @lines = "input.txt".IO.lines;
    for @lines.kv -> $floor, $line {
        for $line ~~ /(\w+)(?:-compatible)? (generator|microchip)/g -> $match {
            my $name = $match[0];
            my $type = $match[1];
            %elem{$name} //= $idx += 2;
            my $pos = %elem{$name};
            $pos = $pos - 2 if $type eq 'generator';
            @floors[$floor].push: $pos;
        }
    }
    my @item-floors = (0 xx %elem.values.max);
    for @floors.kv -> $floor, @items {
        for @items -> $pos {
            @item-floors[$pos] = $floor;
        }
    }
    @item-floors;
}

sub canonical-key(Int $e, @f) {
    my @pairs;
    for 0..^@f.elems -> $i {
        last if $i >= @f.elems;
        @pairs.push: [@f[$i], @f[$i+1]];
        $i++;
    }
    @pairs.sort(-> $a,$b {$a[0] <=> $b[0] || $a[1] <=> $b[1]});
    $e ~ ':' ~ @pairs.map({ $_[0], $_[1] }).flat.join(',');
}

sub valid(@f) {
    for 0..3 -> $fl {
        my @chips = gather for 0..^@f.elems -> $i {
            @f[$i] == $fl && $i %% 2 == 1 ?? $i !! ()
        }
        next unless @chips;
        my @gens = gather for 0..^@f.elems -> $i {
            @f[$i] == $fl && $i %% 2 == 0 ?? $i !! ()
        }
        for @chips -> $c {
            my $g = $c - 1;
            next if @f[$g] == $fl;
            return False if @gens;
        }
    }
    True
}

sub solve(@start) {
    my $init = { e => 0, f => @start, s => 0 };
    my @queue = ($init);
    my %seen = (canonical-key($init<e>, $init<f>) => True);
    while @queue {
        my $cur = @queue.shift;
        return $cur<s> if all($cur<f>) eq 3;
        my @candidates = gather for 0..^$cur<f>.elems -> $i {
            $cur<f>[$i] == $cur<e> ?? $i !! ()
        }
        for @candidates.combinations(1) -> @move {
            for @candidates.combinations(2) -> @move2 {
                @move = @move2 if @move2;
                for $cur<e>-1 .. $cur<e}+1 -> $nf {
                    next if $nf < 0 || $nf > 3 || $nf == $cur<e>;
                    my @nf = $cur<f>.Array;
                    @nf[$_] = $nf for @move;
                    next unless valid(@nf);
                    my $key = canonical-key($nf, @nf);
                    next if %seen{$key}:exists;
                    %seen{$key} = True;
                    @queue.push: { e => $nf, f => @nf, s => $cur<s>+1 };
                }
            }
        }
    }
    -1
}

my @initial-floors = parse-input;
say solve(@initial-floors);
