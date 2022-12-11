import re

monkey_id_parser = re.compile(r'^Monkey (\d+):$')
monkey_list = []


class Monkey:
    starting_items_parser = re.compile(r'^ {2}Starting items: (\d+(?:, \d+)*)$')
    operation_parser = re.compile(r'^ {2}Operation: new = (old [*+] (?:old|\d+))$')
    divisor_parser = re.compile(r'^ {2}Test: divisible by (\d+)$')
    destination_parser = re.compile(r'^ {4}If (true|false): throw to monkey (\d+)$')
    test_divisor_product = 1

    def __init__(self):
        self.items = []
        self.operation = ''
        self.test_divisor = 1
        self.recipent_monkeys = {True: 0, False: 0}
        self.inspection_count = 0

    def parse(self, text_block):
        lines = text_block.split('\n')
        self.items = [int(x) for x in Monkey.starting_items_parser.match(lines.pop(1)).group(1).split(', ')]
        self.operation = Monkey.operation_parser.match(lines.pop(1)).group(1)
        self.test_divisor = int(Monkey.divisor_parser.match(lines.pop(1)).group(1))
        Monkey.test_divisor_product *= self.test_divisor
        for x in range(2):
            truth_value, destination_monkey = Monkey.destination_parser.match(lines.pop(1)).groups()
            self.recipent_monkeys[truth_value == 'true'] = int(destination_monkey)

    def worry_human(self, monkey_list, calm_human=True):
        for old in self.items:
            worry_level = eval(self.operation)
            if calm_human:
                worry_level //= 3
            if worry_level > Monkey.test_divisor_product:
                worry_level = worry_level % Monkey.test_divisor_product
            truth_value = ((worry_level % self.test_divisor) == 0)
            recipient_monkey = self.recipent_monkeys[truth_value]
            monkey_list[recipient_monkey].catch_item(worry_level)
            self.inspection_count += 1
        self.items = []

    def catch_item(self, item):
        self.items.append(item)

    def get_items(self):
        return self.items

    def get_inspection_count(self):
        return self.inspection_count

    def get_test_divisior(self):
        return self.test_divisor


with open('input.txt', 'r') as fh:
    input_blocks = fh.read().split('\n\n')
    for monkey_spec in input_blocks:
        monkey = Monkey()
        monkey.parse(monkey_spec)
        monkey_list.append(monkey)

for game_round in range(20):
    for monkey in monkey_list:
        monkey.worry_human(monkey_list, calm_human=True)

top_monkey_inspectors = sorted(monkey_list, reverse=True, key=lambda x: x.get_inspection_count())[:2]

print(top_monkey_inspectors[0].get_inspection_count() * top_monkey_inspectors[1].get_inspection_count())

monkey_list = []

with open('input.txt', 'r') as fh:
    input_blocks = fh.read().split('\n\n')
    for monkey_spec in input_blocks:
        monkey = Monkey()
        monkey.parse(monkey_spec)
        monkey_list.append(monkey)

for game_round in range(10000):
    for monkey in monkey_list:
        monkey.worry_human(monkey_list, calm_human=False)

top_monkey_inspectors = sorted(monkey_list, reverse=True, key=lambda x: x.get_inspection_count())[:2]

print(top_monkey_inspectors[0].get_inspection_count() * top_monkey_inspectors[1].get_inspection_count())