
use v6;
use NativeCall;

sub MAIN() {
    my $file = 'input.txt';
    my $fh = open $file, :r or die "Could not open $file: $!";
    
    my $grid = CArray[uint32].new: 1000 * 1000;
    my uint32 $total = 0;
    
    for $fh.lines -> $line {
        my @parts = $line.split(/\s+/);
        my $command = @parts[0];
        
        my ($x1, $y1, $x2, $y2);
        if $command eq 'turn' {
            my $start = @parts[2];
            my $end = @parts[4];
            ($x1, $y1) = $start.split(',').map: { .Int };
            ($x2, $y2) = $end.split(',').map: { .Int };
        } else {
            my $start = @parts[1];
            my $end = @parts[3];
            ($x1, $y1) = $start.split(',').map: { .Int };
            ($x2, $y2) = $end.split(',').map: { .Int };
        }
        
        # Ensure coordinates are in correct order
        my $min-x = $x1 min $x2;
        my $max-x = $x1 max $x2;
        my $min-y = $y1 min $y2;
        my $max-y = $y1 max $y2;
        
        for $min-x .. $max-x -> $x {
            for $min-y .. $max-y -> $y {
                my $index = $x * 1000 + $y;
                if $command eq 'turn' && @parts[1] eq 'on' {
                    $grid[$index]++;
                    $total++;
                } elsif $command eq 'turn' && @parts[1] eq 'off' {
                    if $grid[$index] > 0 {
                        $grid[$index]--;
                        $total--;
                    }
                } else { # toggle
                    $grid[$index] += 2;
                    $total += 2;
                }
            }
        }
    }
    
    $fh.close;
    say $total;
}
