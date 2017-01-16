import 'dart:collection';
import 'dart:io';

RegExp NUMBER = new RegExp(r'\d');

class Node {
    List<Node> children;
    int x;
    int y;
    bool visited;

    Node(this.x, this.y)
        : children = new List<Node>(),
          visited = false;

    void add(Node n) {
        if (! n.children.contains(this))
            n.children.add(this);
        if (! this.children.contains(n))
            this.children.add(n);
    }

    String toString() {
        return '($x, $y)';
    }

    Node unvisited() {
        return children.firstWhere((x) => ! x.visited, orElse: () => null);
    }
}

List<Node> numbers(List<String> content) {
    List<List> nums = new List<List>();
    for (int y = 0; y < content.length; y++) {
        for (Match m in NUMBER.allMatches(content[y])) {
            int x = m.start;
            nums.add([int.parse(content[y][x]), new Node(m.start, y)]);
        }
    }
    nums.sort((x, y) => x[0].compareTo(y[0]));
    return new List<Node>.generate(nums.length, (i) => nums[i][1]);
}


List<Node> _translateLine(String line, int y) {
    return new List<Node>.generate(line.length, (int x) {
        return line[x] == '#'
            ? null
            : new Node(x, y);
    });
}

List<List<Node>> translate(List<String> m) {
    return new List<List<Node>>.generate(m.length, (int y) {
        return _translateLine(m[y], y);
    });
}


/// Join each node in [m], next to another node in [m].
List<List<Node>> join(List<List<Node>> m) {
    bool valid(int y, int x) {
        return (y >= 0 && y < m.length)
            && (x >= 0 && x < m[y].length)
            && (m[y][x] != null);
    }
    for (int y = 0; y < m.length; y++) {
        for (int x = 0; x < m[y].length; x++) {
            Node current = m[y][x];
            if (current == null)
                continue;
            if (valid(y-1, x))
                current.add(m[y-1][x]);
            if (valid(y+1, x))
                current.add(m[y+1][x]);
            if (valid(y, x-1))
                current.add(m[y][x-1]);
            if (valid(y, x+1))
                current.add(m[y][x+1]);
        }
    }
    return m;
}

/// Return a list of only the nodes in [m].
List<Node> flatten(List<List<Node>> m) {
    return new List<Node>.from((() sync* {
        for (List<Node> nodes in m)
            for (Node node in nodes)
                if (node != null)
                    yield node;
    })());
}

/// Find a node with the given coordinate, from the ordered list, [nodes].
/// [width] is the original width of the graph, or the highest x-value of
/// all of the nodes.  Implemented using binary search.
Node find(List<Node> nodes, int y, int x, int width) {
    int index_sought = (y * width) + x;
    int lower = 0;
    int upper = nodes.length - 1;
    int middle = 0;
    while (lower <= upper) {
        middle = lower + ((upper - lower) ~/ 2);
        Node mid = nodes[middle];
        int index = (mid.y * width) + mid.x;
        if (index == index_sought) {
            return mid;
        } else if (index < index_sought) {
            lower = middle + 1;
        } else {
            upper = middle;
        }
    }
    return null;
}

/// Perform a DFS, returning the first path which leads to the [end].
/// Also forms a tree from the given graph at the same time.
List<Node> DFS(List<Node> nodes, Node start, Node end) {
    Queue<Node> stack = new Queue<Node>()
        ..addLast(start);
    stack.last.visited = true;
    while (stack.length > 0) {
        if (stack.last == end) {
            return new List<Node>.from(stack);
        }
        Node unvisited = stack.last.unvisited();
        if (unvisited == null) {
            stack.removeLast();
        } else {
            stack.addLast(unvisited);
            stack.last.visited = true;
        }
    }
    return <Node>[];
}


main(List<String> args) async {
    List<String> content = await new File(args[0]).readAsLines();
    List<Node> nodes = flatten(join(translate(content)));
    List<Node> nums = numbers(content);
    for (int i = 0; i < nums.length - 1; i++) {
        Node start = find(nodes, nums[i].y, nums[i].x, content.length);
        Node end = find(nodes, nums[i+1].y, nums[i+1].x, content.length);
        List<Node> path = DFS(nodes, start, end);
        print(path.join(' '));
    }
}
