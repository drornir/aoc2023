from dataclasses import dataclass
from functools import total_ordering
from typing import List, Self

CARD_VALUES = {
    "A": 14,
    "K": 13,
    "Q": 12,
    "J": 11,
    "T": 10,
}


def card_value(c: str) -> int:
    return CARD_VALUES[c] if not c.isdigit() else int(c)


def card_value2(c: str) -> int:
    if c == "J":
        return 1
    return CARD_VALUES[c] if not c.isdigit() else int(c)


@dataclass
@total_ordering
class Hand:
    cards: str
    bid: int
    _type: float | None = None

    def type(self) -> float:
        if self._type is not None:
            return self._type

        ordered_cards = list(self.cards)
        ordered_cards.sort(
            key=card_value,
            reverse=True,  # just easier for my human eye to see when printing
        )

        print(ordered_cards)

        streaks = []
        current_streak = 1
        previous_card = ordered_cards[0]
        for card in ordered_cards[1:]:
            if previous_card != card:
                streaks.append(current_streak)
                current_streak = 0
            previous_card = card
            current_streak += 1
        if current_streak > 0:
            streaks.append(current_streak)

        streaks.sort(reverse=True)

        print(streaks)
        n = 0
        match streaks[0]:
            case 5:
                n = 5
            case 4:
                n = 4
            case 3:
                if streaks[1] == 2:
                    n = 3.5
                else:
                    n = 3
            case 2:
                if streaks[1] == 2:
                    n = 2.5
                else:
                    n = 2
            case 1:
                n = 1
            case _:
                raise Exception(
                    f"unexpected streak number {streaks[0]} in '{self.cards}'"
                )
        self._type = n
        return n

    def card_values(self) -> List[int]:
        return list(map(card_value, self.cards))

    def __lt__(self, other: Self):
        self_type = self.type()
        other_type = other.type()

        if self_type == other_type:
            return self.card_values() < other.card_values()

        return self_type < other_type

    def __eq__(self, other: Self) -> bool:
        # no need to check types if the cards are identical
        return self.cards == other.cards


@dataclass
@total_ordering
class Hand2:
    cards: str
    bid: int
    _type: float | None = None

    def type(self) -> float:
        if self._type is not None:
            return self._type

        ordered_cards = list(self.cards)
        ordered_cards.sort(
            key=card_value2,
            reverse=True,  # just easier for my human eye to see when printing
        )

        print(ordered_cards)

        streaks = []
        current_streak = 1
        previous_card = ordered_cards[0]
        for card in ordered_cards[1:]:
            if previous_card != card:
                streaks.append(current_streak)
                current_streak = 0
            previous_card = card
            current_streak += 1
        if current_streak > 0:
            streaks.append(current_streak)

        # recalculate streaks according to Jokers
        amount_of_j = 0
        if "J" in self.cards:
            # the last streak must be of J's, since we ordered by card value2
            amount_of_j = streaks[-1]
            if amount_of_j == 5:
                return 5
            # take it out
            streaks = streaks[0 : len(streaks) - 1]

        ##
        streaks.sort(reverse=True)
        streaks[0] += amount_of_j

        print(streaks)
        n = 0
        match streaks[0]:
            case 5:
                n = 5
            case 4:
                n = 4
            case 3:
                if streaks[1] == 2:
                    n = 3.5
                else:
                    n = 3
            case 2:
                if streaks[1] == 2:
                    n = 2.5
                else:
                    n = 2
            case 1:
                n = 1
            case _:
                raise Exception(
                    f"unexpected streak number {streaks[0]} in '{self.cards}'"
                )
        self._type = n
        return n

    def card_values(self) -> List[int]:
        return list(map(card_value2, self.cards))

    def __lt__(self, other: Self):
        self_type = self.type()
        other_type = other.type()

        if self_type == other_type:
            return self.card_values() < other.card_values()

        return self_type < other_type

    def __eq__(self, other: Self) -> bool:
        # no need to check types if the cards are identical
        return self.cards == other.cards


def parseFile(lines: List[str]) -> List[Hand]:
    hands = []
    for line in lines:
        if line.isspace():
            continue
        cards, bid = line.split()
        hand = Hand(cards=cards, bid=int(bid))
        hands.append(hand)
    return hands


def reparseForPart2(hands: List[Hand]) -> List[Hand2]:
    return list(
        map(
            lambda h: Hand2(h.cards, h.bid),
            hands,
        )
    )


def part2():
    with open("inputs/day7.txt", "r") as f:
        hands = parseFile(f.readlines())
        hands = reparseForPart2(hands)
    print(hands)

    hands.sort()  # ascending order by rank
    print(hands)

    acc = 0
    for i in range(len(hands)):
        hand = hands[i]
        acc += (i + 1) * hand.bid

    print("solution=", acc)


def part1():
    with open("inputs/day7_ex.txt", "r") as f:
        hands = parseFile(f.readlines())
    print(hands)

    hands.sort()  # ascending order by rank
    print(hands)

    acc = 0
    for i in range(len(hands)):
        hand = hands[i]
        acc += (i + 1) * hand.bid

    print("solution=", acc)


if __name__ == "__main__":
    part2()
