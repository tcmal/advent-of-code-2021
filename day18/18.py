from math import ceil

class SnailfishItem:
    def __init__(self, parent, contents):
        self.parent = parent
        self.atomic = not isinstance(contents, list)
        if not self.atomic:
            if isinstance(contents[0], SnailfishItem):
                self.left = contents[0]
                self.left.parent = self
            else:
                self.left = SnailfishItem(self, contents[0])
            if isinstance(contents[1], SnailfishItem):
                self.right = contents[1]
                self.right.parent = self
            else:
                self.right = SnailfishItem(self, contents[1])
        else:
            self.contents = contents

    def __str__(self):
        if self.atomic:
            return str(self.contents)
        else:
            return "[%s, %s]" % (str(self.left), str(self.right))

    def add(self, other):
        if self.atomic and other.atomic:
            self.contents += other.contents
        else:
            if self.atomic:
                self.left = SnailfishItem(self, self.contents)
            else:
                self.left = SnailfishItem(self, [self.left, self.right])
            self.atomic = False
            self.right = other
            other.parent = self
            self.contents = None
            while self.reduce():
                pass

    def check_explodes(self, n=0):
        if self.atomic:
            return False
        elif n < 4:
            return self.left.check_explodes(n+1) or self.right.check_explodes(n+1)

        self.explode_left()
        self.explode_right()

        self.atomic = True
        self.contents = 0
        self.left = None
        self.right = None

        return True

    def check_splits(self):
        if not self.atomic:
            return self.left.check_splits() or self.right.check_splits()
        elif self.contents < 10:
            return False

        self.atomic = False
        self.left = SnailfishItem(self, self.contents // 2)
        self.right = SnailfishItem(self, ceil(self.contents / 2))
        self.contents = None
        return True

    def reduce(self, n=0):
        if not self.atomic:
            return self.check_explodes(n) or self.check_splits()
        else:
            return self.check_splits()

    def explode_left(self):
        last_node = self
        node = self.parent
        while node != None and node.left == last_node:
            last_node = node
            node = last_node.parent

        if node == None:
            return # leftmost element of tree

        node = node.left
        while not node.atomic:
            node = node.right

        node.add(self.left)

    def explode_right(self):
        last_node = self
        node = self.parent
        while node != None and node.right == last_node:
            last_node = node
            node = last_node.parent

        if node == None:
            return # rightmost element of tree

        node = node.right
        while not node.atomic:
            node = node.left

        node.add(self.right)

    def magnitude(self):
        if self.atomic:
            return self.contents
        else:
            return (3 * self.left.magnitude()) + (2 * self.right.magnitude())

lines = open("./input").read().strip().split("\n")
val = SnailfishItem(None, eval(lines[0])) # cope, seethe, mald, etc.
for line in lines[1:]:
    val.add(SnailfishItem(None, eval(line)))

print("Part 1: %d" % val.magnitude())

max_mag = 0
for x_str in lines:
    for y_str in lines:
        if x_str == y_str:
            continue
        x = SnailfishItem(None, eval(x_str))
        x.add(SnailfishItem(None, eval(y_str)))

        if x.magnitude() > max_mag:
            max_mag = x.magnitude()

print("Part 2: %d" % max_mag)
