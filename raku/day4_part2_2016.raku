
sub MAIN {
    my @lines = 'input.txt'.IO.lines;

    for @lines -> $room {
        next unless is-real-room($room);
        my $decrypted = decrypt-name($room);
        if $decrypted.contains('northpole object') {
            say get-sector-id($room);
            exit;
        }
    }
}

sub is-real-room($room) {
    my ($body, $check) = $room.split('[');
    my $checksum = $check.substr(0, *-1);
    my @parts = $body.split('-')[0..*-2];

    my %count;
    %count{$_}++ for @parts.join.comb;

    my @sorted = %count.sort: { - .value, .key };
    my $expected = @sorted.map(*.key).join.substr(0, $checksum.chars);
    $expected eq $checksum;
}

sub get-sector-id($room) {
    $room.split('-')[*-1].split('[')[0].Int;
}

sub decrypt-name($room) {
    my @parts = $room.split('-');
    my $id = @parts[*-1].split('[')[0].Int;
    my @words = @parts[0..*-2];

    my @out;
    for @words -> $word {
        push @out, $word.comb.map: -> $c {
            $c eq '-' ?? ' ' !! chr((($c.ord - 97 + $id) % 26) + 97);
        }
    }
    @out.map(*.join).join(' ');
}
