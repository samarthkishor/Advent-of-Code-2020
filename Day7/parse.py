#!/usr/bin/env python3


def format_bag(bag):
    num, color = bag.split()[0], " ".join(bag.split()[1:3])
    if num == "no":
        return "{0, 'no other'}"

    return "{" + f"{num}, '{color}'" + "}"


new_lines = []
with open("input.txt", "r") as f:
    for line in f:
        bag_color = " ".join(line.split()[:2])
        contained_bags = line.split("contain")[1].strip().split(",")
        contained_bags = map(format_bag, contained_bags)
        contained_bags = f"[{', '.join(contained_bags)}]"
        baggins = f"baggins('{bag_color}', {contained_bags})"
        new_lines.append(baggins)

with open("parsed.pl", "w") as f:
    f.write(f"parse(Data) :- Data = [{', '.join(new_lines)}].")
