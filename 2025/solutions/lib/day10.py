from scipy.optimize import linprog


def parse_word(word: str) -> list[int]:
    return [int(x) for x in word[1:-1].split(",")]


def part2(line: str) -> int:
    words = line.split(" ")
    buttons = [parse_word(word) for word in words[1:-1]]
    joltage = parse_word(words[-1])

    cols = [[1 if i in button else 0 for i in range(len(joltage))] for button in buttons]
    rows = [list(row) for row in zip(*cols)]
    target = [1] * len(rows[0])
    result = linprog(c=target, A_eq=rows, b_eq=joltage, integrality=1)

    return int(result.fun)


if __name__ == "__main__":
    with open("input/day10.txt") as f:
        lines = f.read().splitlines()

    answer = sum([part2(line) for line in lines])
    print(answer)
