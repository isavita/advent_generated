import std.stdio;
import std.file;
import std.json;
import std.conv;

int sumNumbers(JSONValue json, bool ignoreRed = false) {
    int sum = 0;
    if (json.type == JSON_TYPE.ARRAY) {
        foreach (val; json.array) {
            sum += sumNumbers(val, ignoreRed);
        }
    } else if (json.type == JSON_TYPE.OBJECT) {
        bool hasRed = false;
        foreach (key, val; json.object) {
            if (ignoreRed && val.type == JSON_TYPE.STRING && val.str == "red") {
                hasRed = true;
                break;
            }
            sum += sumNumbers(val, ignoreRed);
        }
        if (hasRed) {
            sum = 0;
        }
    } else if (json.type == JSON_TYPE.INTEGER) {
        sum += json.integer.to!int;
    }
    return sum;
}

void main() {
    string input = cast(string)std.file.read("input.txt");
    JSONValue json = parseJSON(input);

    int partOneSum = sumNumbers(json);
    writeln(partOneSum);

    int partTwoSum = sumNumbers(json, true);
    writeln(partTwoSum);
}