import java.io.File
import java.util.Arrays

class Main {
    static final int MEM_SIZE = 16384
    static final int INPUT_BUFFER_SIZE = 256
    static final int OUTPUT_BUFFER_SIZE = 256

    static class VM {
        long[] code = new long[MEM_SIZE]
        int ip = 0
        long relativeBase = 0
        long[] inputBuffer = new long[INPUT_BUFFER_SIZE]
        int inputHead = 0
        int inputTail = 0
        long[] outputBuffer = new long[OUTPUT_BUFFER_SIZE]
        int outputHead = 0
        int outputTail = 0
        boolean halted = false
    }

    static void vmInit(VM vm) {
        Arrays.fill(vm.code, 0L)
        vm.ip = 0
        vm.relativeBase = 0
        vm.inputHead = 0
        vm.inputTail = 0
        vm.outputHead = 0
        vm.outputTail = 0
        vm.halted = false
    }

    static void vmLoad(VM vm, String filename) {
        File f = new File(filename)
        if (!f.exists()) {
            System.err.println("Input file not found: " + filename)
            System.exit(1)
        }
        String line = f.readLines()[0]
        String[] tokens = line.split(",")
        for (int i = 0; i < tokens.length; i++) {
            vm.code[i] = Long.parseLong(tokens[i])
        }
    }

    static int vmGetParamAddress(VM vm, int pos, int mode) {
        switch (mode) {
            case 0:
                return (int) vm.code[pos]
            case 1:
                return pos
            case 2:
                return (int) (vm.relativeBase + vm.code[pos])
            default:
                throw new IllegalArgumentException("Invalid parameter mode: " + mode)
        }
    }

    static long vmGetValue(VM vm, int pos, int mode) {
        int address = vmGetParamAddress(vm, pos, mode)
        return vm.code[address]
    }

    static void sendString(VM vm, String s) {
        for (int i = 0; i < s.length(); i++) {
            vm.inputBuffer[vm.inputTail++] = (long) s.charAt(i)
            if (vm.inputTail >= INPUT_BUFFER_SIZE) vm.inputTail = 0
        }
        vm.inputBuffer[vm.inputTail++] = (long) '\n'
        if (vm.inputTail >= INPUT_BUFFER_SIZE) vm.inputTail = 0
    }

    static void vmRun(VM vm) {
        while (!vm.halted) {
            long instruction = vm.code[vm.ip]
            int opcode = (int) (instruction % 100)
            int mode1 = (int) ((instruction / 100) % 10)
            int mode2 = (int) ((instruction / 1000) % 10)
            int mode3 = (int) ((instruction / 10000) % 10)

            long param1, param2
            int param3
            switch (opcode) {
                case 1:
                    param1 = vmGetValue(vm, vm.ip + 1, mode1)
                    param2 = vmGetValue(vm, vm.ip + 2, mode2)
                    param3 = vmGetParamAddress(vm, vm.ip + 3, mode3)
                    vm.code[param3] = param1 + param2
                    vm.ip += 4
                    break
                case 2:
                    param1 = vmGetValue(vm, vm.ip + 1, mode1)
                    param2 = vmGetValue(vm, vm.ip + 2, mode2)
                    param3 = vmGetParamAddress(vm, vm.ip + 3, mode3)
                    vm.code[param3] = param1 * param2
                    vm.ip += 4
                    break
                case 3:
                    int addr = vmGetParamAddress(vm, vm.ip + 1, mode1)
                    if (vm.inputHead == vm.inputTail) {
                        return
                    }
                    vm.code[addr] = vm.inputBuffer[vm.inputHead++]
                    if (vm.inputHead >= INPUT_BUFFER_SIZE) vm.inputHead = 0
                    vm.ip += 2
                    break
                case 4:
                    param1 = vmGetValue(vm, vm.ip + 1, mode1)
                    vm.outputBuffer[vm.outputTail++] = param1
                    if (vm.outputTail >= OUTPUT_BUFFER_SIZE) vm.outputTail = 0
                    vm.ip += 2
                    break
                case 5:
                    param1 = vmGetValue(vm, vm.ip + 1, mode1)
                    param2 = vmGetValue(vm, vm.ip + 2, mode2)
                    if (param1 != 0) {
                        vm.ip = (int) param2
                    } else {
                        vm.ip += 3
                    }
                    break
                case 6:
                    param1 = vmGetValue(vm, vm.ip + 1, mode1)
                    param2 = vmGetValue(vm, vm.ip + 2, mode2)
                    if (param1 == 0) {
                        vm.ip = (int) param2
                    } else {
                        vm.ip += 3
                    }
                    break
                case 7:
                    param1 = vmGetValue(vm, vm.ip + 1, mode1)
                    param2 = vmGetValue(vm, vm.ip + 2, mode2)
                    param3 = vmGetParamAddress(vm, vm.ip + 3, mode3)
                    vm.code[param3] = (param1 < param2) ? 1L : 0L
                    vm.ip += 4
                    break
                case 8:
                    param1 = vmGetValue(vm, vm.ip + 1, mode1)
                    param2 = vmGetValue(vm, vm.ip + 2, mode2)
                    param3 = vmGetParamAddress(vm, vm.ip + 3, mode3)
                    vm.code[param3] = (param1 == param2) ? 1L : 0L
                    vm.ip += 4
                    break
                case 9:
                    param1 = vmGetValue(vm, vm.ip + 1, mode1)
                    vm.relativeBase += param1
                    vm.ip += 2
                    break
                case 99:
                    vm.halted = true
                    break
                default:
                    System.err.println("Invalid opcode: " + opcode)
                    vm.halted = true
                    break
            }
        }
    }

    static void reader(VM vm) {
        StringBuilder sb = new StringBuilder()
        while (vm.outputHead != vm.outputTail) {
            int c = vm.outputBuffer[vm.outputHead++]
            if (vm.outputHead >= OUTPUT_BUFFER_SIZE) vm.outputHead = 0
            if (c > 127) {
                println(c)
                return
            } else {
                sb.append((char) c)
            }
        }
        print(sb.toString())
    }

    static void main(String[] args) {
        VM vm = new VM()
        vmInit(vm)
        vmLoad(vm, "input.txt")
        String[] instructions = [
            "NOT A J",
            "NOT B T",
            "OR T J",
            "NOT C T",
            "OR T J",
            "AND D J",
            "NOT A T",
            "AND A T",
            "OR E T",
            "OR H T",
            "AND T J",
            "RUN"
        ]
        instructions.each { s -> sendString(vm, s) }
        vmRun(vm)
        reader(vm)
    }
}