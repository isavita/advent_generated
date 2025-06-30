
import std.stdio, std.file, std.string, std.conv;

long calculate(string monkey, ref string[string] jobs, ref long[string] results) {
    if (auto p = monkey in results) {
        return *p;
    }

    auto job = jobs[monkey];
    long result;

    try {
        result = to!long(job);
    } catch (ConvException) {
        auto parts = job.split();
        auto a = calculate(parts[0], jobs, results);
        auto b = calculate(parts[2], jobs, results);

        switch (parts[1]) {
            case "+": result = a + b; break;
            case "-": result = a - b; break;
            case "*": result = a * b; break;
            case "/": result = a / b; break;
            default: assert(0);
        }
    }

    return results[monkey] = result;
}

void main() {
    string[string] jobs;
    long[string] results;

    foreach (line; readText("input.txt").splitLines()) {
        if (line.empty) continue;
        auto parts = line.split(": ");
        jobs[parts[0]] = parts[1];
    }

    calculate("root", jobs, results).writeln;
}
