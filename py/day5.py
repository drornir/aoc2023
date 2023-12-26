from typing import List, Dict, Optional, Tuple
from dataclasses import dataclass


@dataclass
class Range:
    dest: int
    source: int
    range: int

    def in_range(self, n: int) -> bool:
        return self.source <= n < self.source + self.range

    def to(self, n: int):
        return n + (self.dest - self.source)

    def from_(self, n: int):
        return n - (self.dest - self.source)

    # def __str__(self) -> str:
    #     return f"(s={self.source};d={self.dest};r={self.range})"


class Mapping:
    def __init__(self, ranges: List[Range] = []) -> None:
        self.ranges: List[Range] = ranges

    def find_range(self, n: int) -> Optional[Range]:
        for r in self.ranges:
            if r.in_range(n):
                return r
        return None


class Mappings:
    def __init__(self) -> None:
        self.m: Dict[str, Mapping] = {}

    def __str__(self) -> str:
        items = self.m.items()
        item_to_str = lambda i: i[0] + "=" + i[1].ranges.__str__()

        sep = "\n\t\t"
        content = sep.join([item_to_str(item) for item in items])
        ne = len(content) > 0
        return f"Mappings({sep if ne else''}{content}{sep[0] if ne else''})"


def part1(maps: Mappings, seeds: List[int]) -> int:
    order = [
        "seed-to-soil",
        "soil-to-fertilizer",
        "fertilizer-to-water",
        "water-to-light",
        "light-to-temperature",
        "temperature-to-humidity",
        "humidity-to-location",
    ]
    idxs = seeds
    for key in order:
        m = maps.m[key]
        idxs = progress_through_mapping_part1(m, idxs)

    return min(idxs)


def part2(maps: Mappings, seeds: List[int]) -> int:
    order = [
        "seed-to-soil",
        "soil-to-fertilizer",
        "fertilizer-to-water",
        "water-to-light",
        "light-to-temperature",
        "temperature-to-humidity",
        "humidity-to-location",
    ]

    all_seeds = parseSeedsPart2(seeds)
    order.reverse()

    for key in order:
        m = maps.m[key]
        ranges = progress_through_mapping_part2(m, ranges)

    loc = minimal_location(ranges)

    return loc


def progress_through_mapping_part2(m: Mapping, ranges: List[Range]) -> List[Range]:
    next_ranges: List[Range] = []
    for rng in ranges:
        next_start = m.find_range(rng.source)
        if next_start is None:
            TODO
            continue
        # what the minimum length between start and rng?
        min_len = next_start if next_start.range < rng.range else rng
        

    return next_ranges


def minimal_location(locations: List[Range]) -> int:
    pass


def progress_through_mapping_part1(m: Mapping, idxs: List[int]) -> List[int]:
    new_idxs = []
    for idx in idxs:
        rng = m.find_range(idx)
        if rng is None:
            new_idxs.append(idx)
            continue
        new_idx = rng.to(idx)
        new_idxs.append(new_idx)

    return new_idxs


def parseFile(lines: List[str]) -> Tuple[Mappings, List[int]]:
    maps = Mappings()

    seeds = [int(s.strip()) for s in lines[0].removeprefix("seeds: ").split(" ")]

    current_title = ""
    for line in lines[1:]:
        line = line.strip()
        if line == "":
            continue
        if line.endswith("map:"):
            current_title = line.removesuffix("map:").strip()
            continue

        ns = [int(s.strip()) for s in line.split(" ")]
        range = Range(ns[0], ns[1], ns[2])

        current_map = maps.m.setdefault(current_title, Mapping())
        current_map.ranges.append(range)

    return (maps, seeds)


def parseSeedsPart2(seeds: List[int]) -> List[Range]:
    all_seeds = []
    while len(seeds) > 0:
        head, seeds = seeds[0:2], seeds[2:]
        start, add = head[0], head[1]
        all_seeds.append(Range(start, start, add))

    return all_seeds


def main():
    with open("inputs/day5.txt", "r") as file:
        lines = file.readlines()
        maps, seeds = parseFile(lines)
        # print(maps.__str__(), seeds)

        print("solution:", part2(maps, seeds))


if __name__ == "__main__":
    main()
