from collections import defaultdict

POSSIBLE_ROLLS = [a + b + c for a in range(1, 4) for b in range(1, 4) for c in range(1, 4)]

def move(p, roll):
    pos, score = p
    pos += roll
    while pos > 10:
        pos -= 10

    return (pos, score + pos)

def won(p):
    return p[1] >= 21

P1_WON_KEY = (-1, -1, True)
P2_WON_KEY = (-1, -1, False)

def step_universes(d):
    nxt = defaultdict(lambda: 0)
    nxt[P1_WON_KEY] = d[P1_WON_KEY]
    nxt[P2_WON_KEY] = d[P2_WON_KEY]
    for ((p1, p2, p1_turn), count) in d.items():
        if p1 == -1:
            continue
        elif won(p1):
            nxt[P1_WON_KEY] += count
            continue
        elif won(p2):
            nxt[P2_WON_KEY] += count
            continue

        for roll in POSSIBLE_ROLLS:
            if p1_turn:
                nxt[(move(p1, roll), p2, False)] += count
            else:
                nxt[(p1, move(p2, roll), True)] += count

    return nxt

inp = open("./input").read().split("\n")
p1_start = int(inp[0].split(": ")[1])
p2_start = int(inp[1].split(": ")[1])

d = defaultdict(lambda: 0)
d[((p1_start, 0), (p2_start, 0), True)] = 1
d = step_universes(d)

while len(d) > 2:
    d = step_universes(d)

w1 = d[P1_WON_KEY]
w2 = d[P2_WON_KEY]
print("Part 2: %d" % max([w1, w2]))

