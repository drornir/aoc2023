from dataclasses import dataclass
from typing import List
from math import sqrt, floor, ceil


@dataclass
class Game:
    time: int
    distance: int


def part1(game: Game) -> int:
    wins = 0

    # game.time determines the maximum distance
    #
    # let d be the function that determines the distance traveled
    #   given time held (ht)
    #   and game time (gt)
    # d(ht, gt) = speed(ht) * time_left(ht, gt)
    #           = ht * (gt - ht) = -ht**2 + ht*gt
    # the derivative in relation to ht is
    # d'(ht, gt) = -2*ht + gt
    # d' = 0 <==> 0 = -2ht + gt <==> ht = gt/2

    # let d1 be the inverse of d in relation to ht (has two solutions but we take the lower)
    # d1(di, gt) = 0.5 * (gt - sqrt(gt**2 - 4*di))

    best_holding_time = game.time / 2
    if game.time % 2 == 0:
        wins = 1
        best_holding_time -= 0.5

    best_holding_time = floor(best_holding_time)
    best_distance = best_holding_time * (game.time - best_holding_time)
    print("best_distance", best_distance)
    lower_time_to_get_game_distance = 0.5 * (
        game.time - sqrt(game.time**2 - 4 * game.distance)
    )
    print("lower_time_to_get_game_distance", lower_time_to_get_game_distance)
    print("best_holding_time", best_holding_time)
    ints_between_best_and_record = best_holding_time - floor(lower_time_to_get_game_distance)
    # multiply by two (one for above, one for below best)
    wins += 2 * ints_between_best_and_record

    return wins


def parseFile(content: str) -> List[Game]:
    times_line, distances_line = content.split("\n", maxsplit=1)
    times_line = times_line[len("Time:") :]
    distances_line = distances_line[len("Distance:") :]
    times_strs = times_line.split()
    distances_strs = distances_line.split()

    times = list(map(int, times_strs))
    distances = list(map(int, distances_strs))

    games: List[Game] = []
    for i in range(len(times)):
        games.append(
            Game(
                time=times[i],
                distance=distances[i],
            )
        )

    return games


def main():
    with open("inputs/day6.txt", "r") as f:
        games = parseFile(f.read())
    product = 1
    for game in games:
        n = part1(game)
        print(game, n)

        product *= max(1, n)
    print("solution=", product)


# Time:      7  15   30
# Distance:  9  40  200

if __name__ == "__main__":
    main()
