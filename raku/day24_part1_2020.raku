
#!/usr/bin/env raku
use v6;

sub MAIN() {
    my $file = 'input.txt';
    my $fh   = open $file, :r or die "Cannot open $file: $!";
    my %black;                     # mutable set – stores coordinates that are black

    # axial hex‑grid directions (q, r)
    my %dir = (
        e  => ( 1,  0),
        w  => (-1,  0),
        ne => ( 1, -1),
        nw => ( 0, -1),
        se => ( 0,  1),
        sw => (-1,  1)
    );

    for $fh.lines -> $line {
        my ($q, $r) = (0, 0);

        # split the line into tokens: either a single letter (e,w) or a two‑letter
        # direction (ne,nw,se,sw).  The regex guarantees we never read a stray
        # character.
        for $line.comb( / <[ew]> | <[ns]> <[ew]> / ) -> $tok {
            my ($dq, $dr) = %dir{$tok};
            $q += $dq;
            $r += $dr;
        }

        my $key = "$q,$r";        # unique identifier for this tile

        # toggle black/white: delete if already present, insert otherwise
        if %black{$key}:exists {
            %black{$key}:delete;
        } else {
            %black{$key} = True;
        }
    }

    $fh.close;
    say %black.elems;            # number of black tiles
}
