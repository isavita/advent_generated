class Instruction {
    has Str $.name;
    has @.abc-values;
}

my %*OPCODE-FUNCS = (
    "addr" => sub (@registers, @abc-values) { @registers[@abc-values[2]] = @registers[@abc-values[0]] + @registers[@abc-values[1]]; },
    "addi" => sub (@registers, @abc-values) { @registers[@abc-values[2]] = @registers[@abc-values[0]] + @abc-values[1]; },
    "mulr" => sub (@registers, @abc-values) { @registers[@abc-values[2]] = @registers[@abc-values[0]] * @registers[@abc-values[1]]; },
    "muli" => sub (@registers, @abc-values) { @registers[@abc-values[2]] = @registers[@abc-values[0]] * @abc-values[1]; },
    "banr" => sub (@registers, @abc-values) { @registers[@abc-values[2]] = @registers[@abc-values[0]] +& @registers[@abc-values[1]]; },
    "bani" => sub (@registers, @abc-values) { @registers[@abc-values[2]] = @registers[@abc-values[0]] +& @abc-values[1]; },
    "borr" => sub (@registers, @abc-values) { @registers[@abc-values[2]] = @registers[@abc-values[0]] +| @registers[@abc-values[1]]; },
    "bori" => sub (@registers, @abc-values) { @registers[@abc-values[2]] = @registers[@abc-values[0]] +| @abc-values[1]; },
    "setr" => sub (@registers, @abc-values) { @registers[@abc-values[2]] = @registers[@abc-values[0]]; },
    "seti" => sub (@registers, @abc-values) { @registers[@abc-values[2]] = @abc-values[0]; },
    "gtir" => sub (@registers, @abc-values) { @registers[@abc-values[2]] = (@abc-values[0] > @registers[@abc-values[1]]).Int; },
    "gtri" => sub (@registers, @abc-values) { @registers[@abc-values[2]] = (@registers[@abc-values[0]] > @abc-values[1]).Int; },
    "gtrr" => sub (@registers, @abc-values) { @registers[@abc-values[2]] = (@registers[@abc-values[0]] > @registers[@abc-values[1]]).Int; },
    "eqir" => sub (@registers, @abc-values) { @registers[@abc-values[2]] = (@abc-values[0] == @registers[@abc-values[1]]).Int; },
    "eqri" => sub (@registers, @abc-values) { @registers[@abc-values[2]] = (@registers[@abc-values[0]] == @abc-values[1]).Int; },
    "eqrr" => sub (@registers, @abc-values) { @registers[@abc-values[2]] = (@registers[@abc-values[0]] == @registers[@abc-values[1]]).Int; },
);

class OpcodeComputer {
    has @.instructions;
    has @.registers = (0) xx 6;
    has Int $.instruction-pointer;

    method tick() {
        my $ip = self.registers[self.instruction-pointer];
        if $ip < 0 or $ip >= self.instructions.elems {
            return True;
        }

        my $inst = self.instructions[$ip];
        my $opcode-func = %*OPCODE-FUNCS{$inst.name};

        $opcode-func(self.registers, $inst.abc-values);

        self.registers[self.instruction-pointer]++;

        return False;
    }
}

sub parse-input(Str $input-data) returns OpcodeComputer {
    my @lines = $input-data.lines;
    my $instruction-pointer = @lines[0].words[*-1].Int;

    my @instructions;
    for @lines[1 .. *-1] -> $line {
        next unless $line.trim.chars;
        my @parts = $line.words;
        my $name = @parts[0];
        my @abc-values = @parts[1 .. *-1].map(*.Int);
        @instructions.push: Instruction.new(name => $name, abc-values => @abc-values);
    }
    return OpcodeComputer.new(instructions => @instructions, instruction-pointer => $instruction-pointer);
}

sub solve(Str $input-data) {
    my $opcode-computer = parse-input($input-data);

    loop {
        if $opcode-computer.tick() {
            last;
        }
        if $opcode-computer.registers[$opcode-computer.instruction-pointer] == 28 {
            return $opcode-computer.registers[5];
        }
    }
    return $opcode-computer.registers[5];
}

sub MAIN() {
    my $input-data = slurp 'input.txt';
    print solve($input-data);
}