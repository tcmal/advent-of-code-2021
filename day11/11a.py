def adjacent(coord, arr):
    x, y = coord
    max_x = len(arr[y]) - 1
    max_y = len(arr) - 1
    for dx in [-1, 0, 1]:
        for dy in [-1, 0, 1]:
            xp, yp = (x + dx, y + dy)
            if ((xp, yp) == coord or min(xp, yp) < 0 or xp > max_x or yp > max_y):
                continue
            yield (xp, yp)

def flash(coord, arr, flashed):
    flashed.add(coord)
    for (x, y) in adjacent(coord, arr):
        arr[y][x] += 1
        visit((x, y), arr, flashed)

def visit(coord, arr, flashed):
    x, y = coord
    v = arr[y][x]
    if v > 9 and coord not in flashed:
        flash(coord, arr, flashed)

def step(arr):
    flashed = set()
    for (x,y) in ((x,y) for (y,l) in enumerate(arr) for (x,_) in enumerate(l)):
        arr[y][x] = arr[y][x] + 1
        visit((x,y), arr, flashed)
    for (x,y) in flashed:
        arr[y][x] = 0
    return len(flashed)

def print_board(arr):
    print("\n".join(["".join(map(str, l)) for l in arr]))

state = [[int(n) for n in l] for l in open("./input").read().strip().split("\n")]
total_flashes = 0
for _ in range(0, 100):
    flashes = step(state)
    total_flashes += flashes

print(total_flashes)
