
use v6;

sub find-portal(@maze, $label) {
    for @maze.kv -> $r, @row {
        for @row.kv -> $c, $ch {
            next unless $ch ~~ /<alpha>/;
            if $c+1 < @row && @row[$c+1] ~~ /<alpha>/ {
                my $tag = @row[$c] ~ @row[$c+1];
                next unless $tag eq $label;
                return ($r, $c+2) if $c+2 < @row && @row[$c+2] eq '.';
                return ($r, $c-1) if $c-1 >= 0 && @row[$c-1] eq '.';
            }
            if $r+1 < @maze && @maze[$r+1][$c] ~~ /<alpha>/ {
                my $tag = @maze[$r][$c] ~ @maze[$r+1][$c];
                next unless $tag eq $label;
                return ($r+2, $c) if $r+2 < @maze && @maze[$r+2][$c] eq '.';
                return ($r-1, $c) if $r-1 >= 0 && @maze[$r-1][$c] eq '.';
            }
        }
    }
    die "Portal $label not found";
}

sub find-portals(@maze) {
    my %loc;
    for @maze.kv -> $r, @row {
        for @row.kv -> $c, $ch {
            next unless $ch ~~ /<alpha>/;
            if $c+1 < @row && @row[$c+1] ~~ /<alpha>/ {
                my $tag = @row[$c] ~ @row[$c+1];
                my $p = $c+2 < @row && @row[$c+2] eq '.' ?? ($r,$c+2) !! ($r,$c-1);
                %loc{$tag}.push: $p;
            }
            if $r+1 < @maze && @maze[$r+1][$c] ~~ /<alpha>/ {
                my $tag = @maze[$r][$c] ~ @maze[$r+1][$c];
                my $p = $r+2 < @maze && @maze[$r+2][$c] eq '.' ?? ($r+2,$c) !! ($r-1,$c);
                %loc{$tag}.push: $p;
            }
        }
    }
    my %map;
    for %loc.values -> @v {
        if @v == 2 {
            %map{@v[0].Str} = @v[1];
            %map{@v[1].Str} = @v[0];
        }
    }
    %map;
}

sub bfs(@maze, $start, $end, %portals) {
    my @queue = $start => 0;
    my %seen = $start.Str => 1;
    my @dir = (0,1), (0,-1), (1,0), (-1,0);
    while @queue {
        my ($cur,$steps) = @queue.shift.kv;
        return $steps if $cur eq $end;
        my ($r,$c) = @$cur;
        for @dir -> ($dr,$dc) {
            my ($nr,$nc) = $r+$dr, $c+$dc;
            next unless 0 <= $nr < @maze && 0 <= $nc < @maze[0];
            next unless @maze[$nr][$nc] eq '.';
            my $key = "$nr,$nc";
            next if %seen{$key};
            %seen{$key} = 1;
            @queue.push: [$nr,$nc] => $steps+1;
        }
        if %portals{$cur.Str}:exists {
            my $t = %portals{$cur.Str};
            next if %seen{$t.Str};
            %seen{$t.Str} = 1;
            @queue.push: $t => $steps+1;
        }
    }
    -1
}

sub MAIN {
    my @maze = 'input.txt'.IO.lines>>.comb;
    my $start = find-portal(@maze, 'AA');
    my $end   = find-portal(@maze, 'ZZ');
    my %portals = find-portals(@maze);
    say bfs(@maze, $start, $end, %portals);
}
