
#!/usr/bin/env raku

use v6;

# Direction mapping for hex coordinates
my %dir-map = (
    'e'  => 0,
    'se' => 1,
    'sw' => 2,
    'w'  => 3,
    'nw' => 4,
    'ne' => 5
);

# Hex direction vectors in axial coordinates
my @directions = [
    { q => 1,  r => 0 },
    { q => 0,  r => 1 },
    { q => -1, r => 1 },
    { q => -1, r => 0 },
    { q => 0,  r => -1 },
    { q => 1,  r => -1 }
];

sub get-neighbors($coord) {
    @directions.map({ $coord<q> + $^a<q>, $coord<r> + $^a<r> })
}

sub parse-direction-line(Str $line --> List) {
    my @coords = (0, 0);
    my $i = 0;
    
    while $i < $line.chars {
        my $dir = '';
        if $line.substr($i,1) eq 'e' | 'w' {
            $dir = $line.substr($i,1);
            $i += 1;
        }
        else {
            $dir = $line.substr($i,2);
            $i += 2;
        }
        
        my $dir-index = %dir-map{$dir};
        @coords[0] += @directions[$dir-index]<q>;
        @coords[1] += @directions[$dir-index]<r>;
    }
    return @coords;
}

# Read input and initialize black tiles
my $black-tiles = SetHash.new();
for "input.txt".IO.lines -> $line {
    my ($q, $r) = parse-direction-line($line);
    my $coord = "$q,$r";
    if $black-tiles{$coord}:exists {
        $black-tiles{$coord}:delete;
    } else {
        $black-tiles{$coord} = True;
    }
}

# Simulate for 100 days
for ^100 {
    my $candidates = SetHash.new($black-tiles.keys);
    
    # Add all neighbors of black tiles to candidates
    for $black-tiles.keys -> $coord {
        my ($q, $r) = $coord.split(',').map(*.Int);
        for get-neighbors({ q => $q, r => $r }) -> ($nq, $nr) {
            $candidates{"$nq,$nr"} = True;
        }
    }
    
    my $new-black = SetHash.new();
    for $candidates.keys -> $coord {
        my ($q, $r) = $coord.split(',').map(*.Int);
        my $black-neighbors = 0;
        
        for get-neighbors({ q => $q, r => $r }) -> ($nq, $nr) {
            $black-neighbors++ if $black-tiles{"$nq,$nr"}:exists;
        }
        
        if $black-tiles{$coord}:exists {
            $new-black{$coord} = True if $black-neighbors == 1 | 2;
        } else {
            $new-black{$coord} = True if $black-neighbors == 2;
        }
    }
    
    $black-tiles = $new-black;
}

say $black-tiles.elems;
