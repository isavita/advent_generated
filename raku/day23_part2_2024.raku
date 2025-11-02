
#!/usr/bin/env raku

sub MAIN() {
    my %name-to-id;
    my @id-to-name;
    my SetHash @adj;
    my $node-count = 0;

    sub get-node-id($name) {
        %name-to-id{$name} //= do {
            my $id = $node-count++;
            @id-to-name[$id] = $name;
            @adj[$id] = SetHash.new;
            $id;
        };
    }

    for "input.txt".IO.lines -> $line {
        next unless $line && $line.contains('-');
        my ($name1, $name2) = $line.split('-');
        next unless $name1 && $name2;
        my $id1 = get-node-id($name1);
        my $id2 = get-node-id($name2);
        @adj[$id1]{$id2} = True;
        @adj[$id2]{$id1} = True;
    }

    my @max-clique-ids;
    my $max-clique-size = 0;

    sub find-max-clique(@r, @p) {
        if @p.elems == 0 {
            if @r.elems > $max-clique-size {
                $max-clique-size = @r.elems;
                @max-clique-ids = @r.clone;
            }
            return;
        }
        if @r.elems + @p.elems <= $max-clique-size {
            return;
        }
        for @p.kv -> $i, $v {
            find-max-clique(@r.clone.push($v), @p[$i+1 .. *].grep({ @adj[$v]{$_} }));
        }
    }

    if $node-count > 0 {
        find-max-clique([], (0 ..^ $node-count).Array);
    }

    if $max-clique-size > 0 {
        say @max-clique-ids.map({ @id-to-name[$_] }).sort.join(',');
    }
}
