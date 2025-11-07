
use v6.d;
my @floors;
my $elevator-level = 0;
my $steps = 0;
my %seen;

sub MAIN {
    my @input = 'input.txt'.IO.lines;
    my @items;
    for @input.kv -> $idx, $line {
        for $line.words -> $w {
            if $w eq 'generator' {
                my $mat = @items.pop;
                @floors[$idx].push: { :chip(False), :$mat };
            }
            elsif $w eq 'microchip' {
                my $mat = @items.pop;
                $mat ~~ s/ '-comp' .* //;
                @floors[$idx].push: { :chip(True), :$mat };
            }
            else {
                @items.push: $w;
            }
        }
    }
    # add extra parts
    @floors[0].push: { :chip(False), :mat('elerium') };
    @floors[0].push: { :chip(True),  :mat('elerium') };
    @floors[0].push: { :chip(False), :mat('dilithium') };
    @floors[0].push: { :chip(True),  :mat('dilithium') };

    my @queue = [ { floors => @floors.map({[@$_]}), elevator-level => $elevator-level, steps => $steps } ];
    while @queue {
        my $cur = @queue.shift;
        if $cur<floors>[0..2].map(*.elems).sum == 0 {
            say $cur<steps>;
            exit;
        }
        my $key = hash-key($cur);
        next if %seen{$key}++;
        @queue.push: |next-states($cur);
    }
}

sub hash-key($s) {
    my %g; my %c;
    for $s<floors>.kv -> $fl, @items {
        for @items -> $i {
            if $i<chip> { %c{$i<mat>} = $fl }
            else        { %g{$i<mat>} = $fl }
        }
    }
    my @pairs = %g.keys.sort.map({ (%g{$_}, %c{$_}) }).sort;
    "$s<elevator-level>@pairs[]";
}

sub valid($s) {
    for $s<floors> -> @fl {
        my @gens = @fl.grep( *<chip> == False );
        next unless @gens;
        for @fl.grep( *<chip> ) -> $c {
            return False unless @gens.first( *<mat> eq $c<mat> );
        }
    }
    True
}

sub next-states($s) {
    my @next;
    my @cur = $s<floors>[$s<elevator-level>];
    my @move = (0..^@cur).combinations.grep( 1 <= *.elems <= 2 );
    for (-1,1) -> $Δ {
        my $tgt = $s<elevator-level> + $Δ;
        next unless 0 <= $tgt <= 3;
        for @move -> @idx {
            my @new = $s<floors>.map: {[@$_]};
            @new[$tgt].push: @new[$s<elevator-level>][@idx];
            @new[$s<elevator-level>] = @new[$s<elevator-level>].grep: { !(($_<mat> eq @new[$s<elevator-level>][@idx[0]]<mat>) && ($_<chip> == @new[$s<elevator-level>][@idx[0]]<chip>)) };
            my $st = { floors => @new, elevator-level => $tgt, steps => $s<steps> + 1 };
            @next.push: $st if valid($st);
        }
    }
    @next;
}
