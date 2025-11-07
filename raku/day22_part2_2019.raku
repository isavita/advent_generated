
my constant \Size = 119315717514047;
my constant \Iter = 101741582076661;

sub mod-pow(Int \base, Int \exp, Int \mod) returns Int {
    my Int $res = 1;
    my Int $b = base % mod;
    my Int $e = exp;
    while $e > 0 {
        $res = ($res * $b) % mod if $e +& 1;
        $b = ($b * $b) % mod;
        $e +>= 1;
    }
    $res
}

my Int $offset = 0;
my Int $increment = 1;

for 'input.txt'.IO.lines {
    when 'deal into new stack' {
        $increment = -$increment;
        $offset += $increment;
    }
    when /'cut ' (\S+)/ {
        $offset += ($0.Int * $increment);
    }
    when /'deal with increment ' (\S+)/ {
        $increment *= mod-pow($0.Int, Size - 2, Size);
    }
}

my Int $final-incr = mod-pow($increment, Iter, Size);
my Int $final-offs = (($offset * (1 - mod-pow($increment, Iter, Size))) % Size)
                   * mod-pow(1 - $increment, Size - 2, Size);

my Int $answer = (2020 * $final-incr + $final-offs) % Size;
$answer += Size if $answer < 0;
put $answer;
