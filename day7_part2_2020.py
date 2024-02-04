
import re

def parse_rules(rules):
    bag_dict = {}
    for rule in rules:
        outer_bag, inner_bags = re.match(r"(.+?) bags contain (.+)", rule).groups()
        inner_bags = re.findall(r"(\d+) (.+?) bags?", inner_bags)
        bag_dict[outer_bag] = [(int(num), color) for num, color in inner_bags]
    return bag_dict

def can_contain_shiny_gold(bag_dict, current_bag):
    if current_bag == "shiny gold":
        return True
    return any(can_contain_shiny_gold(bag_dict, bag) for _, bag in bag_dict.get(current_bag, []))

def count_individual_bags(bag_dict, current_bag):
    return sum(num + num * count_individual_bags(bag_dict, bag) for num, bag in bag_dict.get(current_bag, []))

with open("input.txt", "r") as file:
    rules = file.read().strip().split("\n")

bag_dict = parse_rules(rules)

result_part1 = sum(can_contain_shiny_gold(bag_dict, bag) for bag in bag_dict.keys() if bag != "shiny gold")
result_part2 = count_individual_bags(bag_dict, "shiny gold")

print(result_part1)
print(result_part2)
