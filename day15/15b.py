import heapq

grid = [[int(x) for x in l] for l in open("./input").read().strip().split("\n")]
GRID_SIZE_X = len(grid)
GRID_SIZE_Y = len(grid[0])

def in_bounds(c, tiles):
    x, y = c
    return x >= 0 and y >= 0 and y < (GRID_SIZE_Y * tiles) and x < (GRID_SIZE_X * tiles)

def next_steps(path, tiles):
    sx, sy = path[0]
    for c in [(sx + 1, sy), (sx - 1, sy), (sx, sy + 1), (sx, sy - 1)]:
        if c not in path and in_bounds(c, tiles):
            yield c

def step_path(path, tiles):
    for c in next_steps(path, tiles):
        yield [c] + path

def risk_for(x, y):
    tile_x = x // GRID_SIZE_X
    tile_y = y // GRID_SIZE_Y
    delta = tile_x + tile_y
    risk = grid[y % GRID_SIZE_Y][x % GRID_SIZE_X] + delta
    while risk > 9:
        risk -= 9
    return risk

def total_risk(path):
    acc = 0
    for x, y in path:
        if (x, y) == (0, 0):
            continue
        risk = risk_for(x, y)
        acc += risk

    return acc

def manhattan(a, b):
    dx = abs(a[0] - b[0])
    dy = abs(a[1] - b[1])
    return dx + dy

def heuristic(path, dst):
    return total_risk(path) - manhattan(path[0], dst)

class HeapItem:
    def __init__(self, heur, path):
        self.heur = heur
        self.path = path

    def __lt__(self, other):
        return self.heur < other.heur

def find_path(start, tiles):
    q = []
    max_x = (GRID_SIZE_X * tiles) - 1
    max_y = (GRID_SIZE_Y * tiles) - 1
    dst = (max_x, max_y)
    visited = set()
    heapq.heappush(q, HeapItem(0, [start]))
    while True:
        i = heapq.heappop(q)
        heur = i.heur
        path = i.path
        if path[0] == (max_x, max_y):
            return path

        if path[0] in visited:
            continue
        visited.add(path[0])

        for new in step_path(path, tiles):
            loc = new[0]
            heapq.heappush(q, HeapItem(heur + risk_for(loc[0], loc[1]), new))

path = find_path((0, 0), 5)
print(path)
print(total_risk(path))
