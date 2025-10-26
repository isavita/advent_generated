
my @code = slurp('input.txt').trim.split(',').map: +*;

class VM {
    has @.code;
    has int $.ip;
    has Channel $.in;
    has Channel $.out;

    submethod BUILD(:@code, :$in, :$out) {
        @!code = @code;
        $!in   = $in;
        $!out  = $out;
    }

    method op-code { @!code[$!ip] % 100 }

    method imm(int $n) { ((@!code[$!ip] div (10**($n+1))) % 10) == 1 }

    method param(int $n, Bool $imm) {
        my $v = @!code[$!ip+$n];
        $imm ?? $v !! @!code[$v]
    }

    method run() {
        loop {
            given self.op-code {
                when 1 {
                    my $a = self.param(1, self.imm(1));
                    my $b = self.param(2, self.imm(2));
                    @!code[self.param(3,True)] = $a + $b;
                    $!ip += 4;
                }
                when 2 {
                    my $a = self.param(1, self.imm(1));
                    my $b = self.param(2, self.imm(2));
                    @!code[self.param(3,True)] = $a * $b;
                    $!ip += 4;
                }
                when 3 {
                    @!code[self.param(1,True)] = $!in.receive;
                    $!ip += 2;
                }
                when 4 {
                    $!out.send(self.param(1, self.imm(1)));
                    $!ip += 2;
                }
                when 5 {
                    my $a = self.param(1, self.imm(1));
                    my $b = self.param(2, self.imm(2));
                    $!ip = $a ?? $b !! $!ip + 3;
                }
                when 6 {
                    my $a = self.param(1, self.imm(1));
                    my $b = self.param(2, self.imm(2));
                    $!ip = $a == 0 ?? $b !! $!ip + 3;
                }
                when 7 {
                    my $a = self.param(1, self.imm(1));
                    my $b = self.param(2, self.imm(2));
                    @!code[self.param(3,True)] = $a < $b ?? 1 !! 0;
                    $!ip += 4;
                }
                when 8 {
                    my $a = self.param(1, self.imm(1));
                    my $b = self.param(2, self.imm(2));
                    @!code[self.param(3,True)] = $a == $b ?? 1 !! 0;
                    $!ip += 4;
                }
                when 99 { return }
                default   { die "bad op {@!code[$!ip]}" }
            }
        }
    }
}

sub run-loop(@phase) {
    my Channel @ch = (1..5).map: { Channel.new };
    my @promise;

    for 0..4 -> $i {
        @ch[$i].send(@phase[$i]);
        my $vm = VM.new(code => @code, in => @ch[$i], out => @ch[($i+1)%5]);
        @promise.push: start { $vm.run; }
    }
    @ch[0].send(0);
    await @promise;
    return @ch[0].receive;
}

my $max = 0;
for (5..9).permutations -> @p {
    my $out = run-loop(@p);
    $max = $out if $out > $max;
}
say $max;
