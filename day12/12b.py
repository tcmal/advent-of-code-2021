from collections import deque

def parseInput(contents):
    edges = [l.split("-") for l in contents.strip().split("\n")]
    edges_set = set([])
    for (f, t) in edges:
        edges_set.add((f, t))
        if t != "end" and f != "start":
            edges_set.add((t, f))

    return edges_set

def visitableNeighbours(edges, path, doubleVisited):
    curr = path[0]
    banned = [e for e in path if e.islower()] if doubleVisited else ["start"]
    for (f, t) in edges:
        if f == curr and t not in banned:
            yield t


edges = parseInput(open("./input").read())

locs = deque() # each element is a tuple of (double visited, list of all the visited nodes)
finished = set()
locs.append((False, ["start"]))
while len(locs) > 0:
    doubleVisited, path = locs.pop()
    if path[0] == "end":
        finished.add("-".join(path))
    else:
        for neighbour in visitableNeighbours(edges, path, doubleVisited):
            locs.append((doubleVisited or (neighbour.islower() and neighbour in path), [neighbour] + path))
