
die_val = 1
rolls = 0
def roll():
    global die_val, rolls
    x = die_val

    die_val += 1
    if die_val > 100:
        die_val = 1

    rolls += 1

    return x


class Player:
    def __init__(self, pos):
        self.pos = pos
        self.score = 0

    def move(self, roll):
        self.pos += roll
        while self.pos > 10:
            self.pos -= 10

    def take_turn(self):
        r = sum([roll() for _ in range(0, 3)])
        self.move(r)
        self.score += self.pos

    def __str__(self):
        return "<Player pos=%d score=%d>" % (self.pos, self.score)

inp = open("./input").read().split("\n")
p1_start = int(inp[0].split(": ")[1])
p2_start = int(inp[1].split(": ")[1])

p1 = Player(p1_start)
p2 = Player(p2_start)
turn = p1
while p1.score < 1000 and p2.score < 1000:
    turn.take_turn()
    if turn == p1:
        turn = p2
    else:
        turn = p1

ans = p1.score * rolls
if p1.score >= 1000:
    ans = p2.score * rolls
print("Part 1: %d" % ans)
