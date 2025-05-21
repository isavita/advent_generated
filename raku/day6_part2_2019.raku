
sub MAIN {
    my %orbit-map;
    for 'input.txt'.IO.lines -> $line {
        my ($orbiter, $orbitee) = $line.split(')');
        %orbit-map{$orbitee} = $orbiter;
    }

    sub get-orbit-count(Str $obj) {
        return 0 unless %orbit-map{$obj}:exists;
        1 + get-orbit-count(%orbit-map{$obj});
    }
    say sum %orbit-map.keys.map: &get-orbit-count;

    sub get-path-to-com(Str $obj) {
        my @path;
        my $current = $obj;
        while %orbit-map{$current}:exists {
            $current = %orbit-map{$current};
            @path.push($current);
        }
        return @path;
    }

    my @path-to-you = get-path-to-com('YOU');
    my @path-to-san = get-path-to-com('SAN');

    my $common-obj = @path-to-you.first: * (elem) @path-to-san.Set;

    my $idx-you = @path-to-you.first: * eq $common-obj, :k;
    my $idx-san = @path-to-san.first: * eq $common-obj, :k;

    say $idx-you + $idx-san;
}
