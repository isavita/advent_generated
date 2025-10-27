
my %grid;
my ($start-x, $start-y);

for 'input.txt'.IO.lines.kv -> $y, $line {
    for $line.comb.kv -> $x, $char {
        %grid{"$x,$y"} = 2 if $char eq '#';
    }
    $start-x = ($line.chars - 1) div 2;
    $start-y = $y div 2;
}

my ($x, $y)   = $start-x, $start-y;
my $dir       = 0;
my $infected  = 0;
my @dx        = 0, 1, 0, -1;
my @dy        = -1, 0, 1, 0;

for ^10_000_000 {
    my $key = "$x,$y";
    given %grid{$key} // 0 {
        when 0 { $dir = ($dir - 1 + 4) % 4; %grid{$key} = 1 }
        when 1 { %grid{$key} = 2; $infected++ }
        when 2 { $dir = ($dir + 1) % 4; %grid{$key} = 3 }
        when 3 { $dir = ($dir + 2) % 4; %grid{$key} = 0 }
    }
    $x += @dx[$dir];
    $y += @dy[$dir];
}

say $infected;
