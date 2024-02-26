
BEGIN {
    FS=","
    getline program < "input.txt"
    split(program, arr, ",")
    input = 5
    output = 0
    i = 1
    while (i <= length(arr)) {
        opcode = arr[i] % 100
        modes = int(arr[i] / 100)
        param1Mode = modes % 10
        modes = int(modes / 10)
        param2Mode = modes % 10

        if (opcode == 1) {
            p1 = getValue(arr, i+1, param1Mode)
            p2 = getValue(arr, i+2, param2Mode)
            p3 = arr[i+3]
            arr[p3+1] = p1 + p2
            i += 4
        } else if (opcode == 2) {
            p1 = getValue(arr, i+1, param1Mode)
            p2 = getValue(arr, i+2, param2Mode)
            p3 = arr[i+3]
            arr[p3+1] = p1 * p2
            i += 4
        } else if (opcode == 3) {
            arr[arr[i+1]+1] = input
            i += 2
        } else if (opcode == 4) {
            output = getValue(arr, i+1, param1Mode)
            print output
            i += 2
        } else if (opcode == 5) {
            p1 = getValue(arr, i+1, param1Mode)
            p2 = getValue(arr, i+2, param2Mode)
            if (p1 != 0) {
                i = p2 + 1
            } else {
                i += 3
            }
        } else if (opcode == 6) {
            p1 = getValue(arr, i+1, param1Mode)
            p2 = getValue(arr, i+2, param2Mode)
            if (p1 == 0) {
                i = p2 + 1
            } else {
                i += 3
            }
        } else if (opcode == 7) {
            p1 = getValue(arr, i+1, param1Mode)
            p2 = getValue(arr, i+2, param2Mode)
            p3 = arr[i+3]
            if (p1 < p2) {
                arr[p3+1] = 1
            } else {
                arr[p3+1] = 0
            }
            i += 4
        } else if (opcode == 8) {
            p1 = getValue(arr, i+1, param1Mode)
            p2 = getValue(arr, i+2, param2Mode)
            p3 = arr[i+3]
            if (p1 == p2) {
                arr[p3+1] = 1
            } else {
                arr[p3+1] = 0
            }
            i += 4
        } else if (opcode == 99) {
            exit
        } else {
            print "Invalid opcode"
            exit 1
        }
    }
}

function getValue(arr, pos, mode) {
    if (mode == 0) {
        return arr[arr[pos]+1]
    } else {
        return arr[pos]
    }
}
