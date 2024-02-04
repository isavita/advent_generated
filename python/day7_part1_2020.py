with open('input.txt') as f:
    rules = f.read().strip().split('\n')

contain_dict = {}
for rule in rules:
    bag_color, contents = rule.split(' bags contain ')
    contain_dict[bag_color] = []
    if contents != 'no other bags.':
        bags = contents.split(', ')
        for bag in bags:
            bag = bag.split(' ')
            contain_dict[bag_color].append(' '.join(bag[1:3]))

def check_bag(bag_color):
    if bag_color == 'shiny gold':
        return True
    return any(check_bag(color) for color in contain_dict[bag_color])

print(sum(check_bag(color) for color in contain_dict.keys()) - 1)