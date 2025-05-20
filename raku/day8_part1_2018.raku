
sub read-node (@data) {
    my $num-child-nodes    = @data.shift;
    my $num-metadata-entries = @data.shift;
    my $total = 0;
    for 1 .. $num-child-nodes {
        $total += read-node(@data);
    }
    for 1 .. $num-metadata-entries {
        $total += @data.shift;
    }
    return $total;
}

sub MAIN {
    my @data = 'input.txt'.IO.slurp.words.map: *.Int;
    my $result = read-node(@data);
    print $result;
}
