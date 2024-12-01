"use strict";
var __spreadArray = (this && this.__spreadArray) || function (to, from, pack) {
    if (pack || arguments.length === 2) for (var i = 0, l = from.length, ar; i < l; i++) {
        if (ar || !(i in from)) {
            if (!ar) ar = Array.prototype.slice.call(from, 0, i);
            ar[i] = from[i];
        }
    }
    return to.concat(ar || Array.prototype.slice.call(from));
};
Object.defineProperty(exports, "__esModule", { value: true });
var fs = require("fs");
// Function to parse the input file and return the left and right lists
function parseInput(filename) {
    var data = fs.readFileSync(filename, 'utf8');
    var leftList = [];
    var rightList = [];
    data.split('\n').forEach(function (line) {
        if (line.trim() === '')
            return;
        var _a = line.split(/\s+/).map(Number), left = _a[0], right = _a[1];
        leftList.push(left);
        rightList.push(right);
    });
    return { leftList: leftList, rightList: rightList };
}
// Function to calculate the total distance
function calculateTotalDistance(leftList, rightList) {
    var sortedLeft = __spreadArray([], leftList, true).sort(function (a, b) { return a - b; });
    var sortedRight = __spreadArray([], rightList, true).sort(function (a, b) { return a - b; });
    return sortedLeft.reduce(function (total, left, index) {
        var right = sortedRight[index];
        return total + Math.abs(left - right);
    }, 0);
}
// Main function
function main() {
    var filename = 'input.txt';
    var _a = parseInput(filename), leftList = _a.leftList, rightList = _a.rightList;
    var totalDistance = calculateTotalDistance(leftList, rightList);
    console.log('Total distance:', totalDistance);
}
main();
