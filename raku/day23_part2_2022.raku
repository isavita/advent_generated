
my @dirs  = ((-1,-1),(-1,0),(-1,1),(0,1),(1,1),(1,0),(1,-1),(0,-1));
my @order = (1,5,7,3);

my @elves;
my %map;

for 'input.txt'.IO.lines.kv -> $r, $line {
    for $line.comb.kv -> $c, $char {
        if $char eq '#' {
            my $p = [$r,$c];
            %map{$p} = 1;
            @elves.push: { :pos($p), :moving(False), :next-pos($p) };
        }
    }
}

sub around-all-empty($elf) {
    for @dirs -> ($dr,$dc) {
        my $adj = [$elf<pos>[0]+$dr, $elf<pos>[1]+$dc];
        return False if %map{$adj}:exists;
    }
    True
}

sub elf-in-direction($elf, $wanna) {
    for -1..1 -> $j {
        my $dir  = ($wanna + $j + 8) % 8;
        my ($dr,$dc) = @dirs[$dir];
        my $adj = [$elf<pos>[0]+$dr, $elf<pos>[1]+$dc];
        return True if %map{$adj}:exists;
    }
    False
}

my $curr = 0;
loop (my $i = 1; ; ++$i) {
    my %prop;
    for @elves -> $e { $e<moving> = False }

    for @elves -> $e {
        next if around-all-empty($e);

        for ^4 -> $k {
            my $dir = @order[($curr + $k) % 4];
            next if elf-in-direction($e, $dir);

            my ($dr,$dc) = @dirs[$dir];
            my $dest = [$e<pos>[0]+$dr, $e<pos>[1]+$dc];
            %prop{$dest}++;
            $e<next-pos> = $dest;
            $e<moving>    = True;
            last;
        }
    }

    my $moved = False;
    for @elves -> $e {
        next unless $e<moving>;
        if %prop{$e<next-pos>} > 1 {
            $e<moving> = False;
            next;
        }
        $moved = True;
        %map{$e<pos>}:delete;
        $e<pos> = $e<next-pos>;
        %map{$e<pos>} = 1;
        $e<moving> = False;
    }

    unless $moved {
        say $i;
        exit;
    }
    $curr = ($curr + 1) % 4;
}
