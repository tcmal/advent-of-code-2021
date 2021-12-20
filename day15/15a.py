import heapq

grid = [[int(x) for x in l] for l in open("./input").read().strip().split("\n")]

def in_bounds(c):
    x, y = c
    return x >= 0 and y >= 0 and y < len(grid) and x < len(grid[y])

def next_steps(path):
    sx, sy = path[0]
    for c in [(sx + 1, sy), (sx - 1, sy), (sx, sy + 1), (sx, sy - 1)]:
        if c not in path and in_bounds(c):
            yield c

def step_path(path):
    for c in next_steps(path):
        yield [c] + path

def total_risk(path):
    acc = 0
    for x, y in path:
        if (x, y) == (0, 0):
            continue
        risk = grid[y][x]
        acc += risk

    return acc

def manhattan(a, b):
    dx = abs(a[0] - b[0])
    dy = abs(a[1] - b[1])
    return dx + dy

def heuristic(path):
    return total_risk(path) - manhattan(path[0], (len(grid), len(grid[0])))

class HeapItem:
    def __init__(self, path):
        self.heur = heuristic(path)
        self.path = path

    def __lt__(self, other):
        return self.heur < other.heur

def find_path(start):
    q = []
    visited = set()
    heapq.heappush(q, HeapItem([start]))
    while True:
        path = heapq.heappop(q).path
        if path[0] == (len(grid) - 1, len(grid[0]) - 1):
            return path

        if path[0] in visited:
            continue
        visited.add(path[0])

        for new in step_path(path):
            heapq.heappush(q, HeapItem(new))

path = find_path((0, 0))
print(path)
print(total_risk(path))
