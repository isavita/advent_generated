
import sys

def parse_input(input):
    histories = []
    for line in input:
        numbers = list(map(int, line.split()))
        histories.append(numbers)
    return histories

def all_zeros(nums):
    return all(num == 0 for num in nums)

def calculate_extrapolation(history):
    extrapolations = []
    for i in range(1, len(history)):
        extrapolation = history[i] - history[i-1]
        extrapolations.append(extrapolation)
    return extrapolations

def calculate_extrapolations(history):
    extrapolations_series = [history]
    for i in range(1, len(history)):
        previous_extrapolations = extrapolations_series[i-1]
        if all_zeros(previous_extrapolations):
            return extrapolations_series
        extrapolations = calculate_extrapolation(previous_extrapolations)
        extrapolations_series.append(extrapolations)
    return extrapolations_series

def solve(input):
    histories = parse_input(input)
    res = 0
    for history in histories:
        extrapolations_series = calculate_extrapolations(history)
        past_prediction = 0
        for i in range(len(extrapolations_series) - 1, -1, -1):
            past_prediction = extrapolations_series[i][0] - past_prediction
        res += past_prediction
    return res

def read_file(file_name):
    with open(file_name, 'r') as file:
        return file.read().strip().split('\n')

input_data = read_file("input.txt")
print(solve(input_data))
