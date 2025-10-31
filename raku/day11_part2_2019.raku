
#!/usr/bin/env raku
use v6;

class IntcodeComputer {
    has $.ip = 0;
    has $.relative-base = 0;
    has %!mem;
    has $.halted = False;
    has $.awaiting-input = False;
    has $.input-addr;
    has $.input-mode;

    submethod BUILD(:@program) {
        %!mem = %();
        for @program.kv -> $i, $v { %!mem{$i} = $v }
    }

    method get-parameter(Int $mode, Int $off) {
        my $param = %!mem{$!ip + $off} // 0;
        $mode == 0 ?? (%!mem{$param} // 0)
               !! $mode == 1 ?? $param
               !! $mode == 2 ?? (%!mem{$!relative-base + $param} // 0)
               !! 0;
    }

    method set-parameter(Int $mode, Int $off, Int $val) {
        my $param = %!mem{$!ip + $off} // 0;
        if $mode == 0 { %!mem{$param} = $val }
        elsif $mode == 2 { %!mem{$!relative-base + $param} = $val }
    }

    method run-until-io(Int $input-val) {
        return (False, True, 0) if $!halted;
        if $!awaiting-input {
            $!awaiting-input = False;
            self.set-parameter($!input-mode, $!input-addr - $!ip, $input-val);
            $!ip += 2;
        }
        loop {
            my $instr = %!mem{$!ip} // 0;
            my $opcode = $instr % 100;
            my $m1 = ($instr div 100)   % 10;
            my $m2 = ($instr div 1000)  % 10;
            my $m3 = ($instr div 10000) % 10;
            given $opcode {
                when 1 {
                    self.set-parameter($m3, 3,
                        self.get-parameter($m1,1) + self.get-parameter($m2,2));
                    $!ip += 4;
                }
                when 2 {
                    self.set-parameter($m3, 3,
                        self.get-parameter($m1,1) * self.get-parameter($m2,2));
                    $!ip += 4;
                }
                when 3 {
                    $!input-addr = $!ip + 1;
                    $!input-mode = $m1;
                    $!awaiting-input = True;
                    return (False, False, 0);
                }
                when 4 {
                    my $out = self.get-parameter($m1,1);
                    $!ip += 2;
                    return (True, False, $out);
                }
                when 5 {
                    $!ip = self.get-parameter($m1,1) != 0
                        ?? self.get-parameter($m2,2) !! $!ip + 3;
                }
                when 6 {
                    $!ip = self.get-parameter($m1,1) == 0
                        ?? self.get-parameter($m2,2) !! $!ip + 3;
                }
                when 7 {
                    self.set-parameter($m3,3,
                        self.get-parameter($m1,1) < self.get-parameter($m2,2) ?? 1 !! 0);
                    $!ip += 4;
                }
                when 8 {
                    self.set-parameter($m3,3,
                        self.get-parameter($m1,1) == self.get-parameter($m2,2) ?? 1 !! 0);
                    $!ip += 4;
                }
                when 9 {
                    $!relative-base += self.get-parameter($m1,1);
                    $!ip += 2;
                }
                when 99 {
                    $!halted = True;
                    return (False, True, 0);
                }
            }
        }
    }
}

class Robot {
    has $.computer;
    has $.direction = 0;   # 0 up,1 right,2 down,3 left
    has $.x = 0;
    has $.y = 0;
    has %!panels;
    has %!painted;

    submethod BUILD(:@program, :$start-color) {
        $!computer = IntcodeComputer.new(:program(@program));
        %!panels{"0,0"} = $start-color;
    }

    method turn-and-move(Int $turn) {
        $!direction = $turn == 0
            ?? ($!direction - 1 + 4) % 4
            !! ($!direction + 1) % 4;
        given $!direction {
            when 0 { $!y-- }
            when 1 { $!x++ }
            when 2 { $!y++ }
            when 3 { $!x-- }
        }
    }

    method run {
        my $output-pending = False;
        my $paint-col;
        loop {
            my $cur = %!panels{"$!x,$!y"} // 0;
            my ($is-out, $is-halt, $val) = $!computer.run-until-io($cur);
            last if $is-halt;
            if $is-out {
                if !$output-pending {
                    $paint-col = $val;
                    $output-pending = True;
                } else {
                    %!panels{"$!x,$!y"} = $paint-col;
                    %!painted{"$!x,$!y"} = 1;
                    $output-pending = False;
                    self.turn-and-move($val);
                }
            }
        }
    }

    method painted-count { %!painted.elems }

    method render {
        my @coords = %!panels.keys.map: -> $k {
            my ($x,$y) = $k.split(',').map(*.Int);
            ($x,$y);
        };
        return unless @coords;
        my $minx = @coords.map(*[0]).min;
        my $maxx = @coords.map(*[0]).max;
        my $miny = @coords.map(*[1]).min;
        my $maxy = @coords.map(*[1]).max;
        say "\nRegistration Identifier:";
        for $miny .. $maxy -> $y {
            my $line = "";
            for $minx .. $maxx -> $x {
                $line ~= (%!panels{"$x,$y"} // 0) == 1 ?? '#' !! ' ';
            }
            say $line;
        }
    }
}

sub parse-input(Str $path) {
    slurp($path).trim.split(',').map(*.Int);
}

sub MAIN {
    my @program = parse-input('input.txt');

    my $r1 = Robot.new(:@program, :start-color(0));
    $r1.run;
    say $r1.painted-count;

    my $r2 = Robot.new(:@program, :start-color(1));
    $r2.run;
    $r2.render;
}
