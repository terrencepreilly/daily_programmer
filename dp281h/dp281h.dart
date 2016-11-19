
enum PointType { mine, number, space, question, query, safe }

class Point {
    final int x, y;
    int score;
    PointType ptype;

    Point(this.x, this.y, [this.ptype=PointType.query, this.score]);

    void decrement() { this.score--; }

    String toString() {
        if (this.ptype == PointType.mine)
            return 'X';
        else if (this.ptype == PointType.space)
            return ' ';
        else if (this.ptype == PointType.question)
            return '?';
        else if (this.ptype == PointType.safe)
            return 's';
        else
            return '${this.score}';
    }
}

class Field {
    List<List<Point>> field = new List<List<Point>>();

    List<Point> _split_line(String line, int x) {
        List<Point> l = new List<Point>();
        for (var y = 0; y < line.length; y++) {
            Point p = new Point(x, y);
            if (line[y] == ' ') {
                p.ptype = PointType.space;
            } else if (line[y] == '?') {
                p.ptype = PointType.question;
            } else {
                p.ptype = PointType.number;
                p.score = int.parse(line[y]);
            }
            l.add(p);
        }
        return l;
    }

    Field(List<String> f) {
        for (int y = 0; y < f.length; y++)
            this.field.add(_split_line(f[y], y));
    }

    get length => this.field.length;
    get width => this.field[0].length;

    Point query(Point p) {
        return this.field[p.x][p.y];
    }

    String toString() {
        return this.field.map((line) {
            return line.map((x) => x.toString()).join('');
        }).join('\n');
    }
}

bool is_valid(Field f, Point p, int x, int y) {
    return (x >= 0 && y >= 0) &&
           (x < p.x+2 && y < p.y+2) &&
           (y < f.width && x < f.length) &&
           (x != p.x || y != p.y);
}

Iterable surrounding(Field f, Point p) sync* {
    for (int x = p.x-1; x <= p.x+1; x++)
        for (int y = p.y-1; y <= p.y+1; y++)
            if (is_valid(f, p, x, y))
                yield f.query(new Point(x, y));
}

List<Point> questions(Field f, Point p) {
    List<Point> qs = new List<Point>();
    for (Point possible_question in surrounding(f, p)) {
        if (possible_question.ptype == PointType.question)
            qs.add(possible_question);
    }
    return qs;
}

Iterable all_numbers(Field f) sync* {
    for (int x = 0; x < f.length; x++) {
        for (int y = 0; y < f.width; y++) {
            Point p = f.query(new Point(x, y));
            if (p.ptype == PointType.number)
                yield p;
        }
    }
}

List<Point> scan(Field field, Point p) {
    List<Point> qs = questions(field, p);
    if (qs.length == p.score)
        return qs;
    return new List<Point>();
}

void flag(Field f, Point p) {
    for (Point spoint in surrounding(f, p)) {
        if (spoint.ptype == PointType.number)
            spoint.decrement();
    }
    p.ptype = PointType.mine;
}

void mark_safe(Field f, Point p) {
    for (Point spoint in surrounding(f, p)) {
        if (spoint.ptype == PointType.question)
            spoint.ptype = PointType.safe;
    }
}

void main() {
    List<String> f =
'''
??????
???2??
???4??
?2??2?
?2222?
?1001?'''.split('\n');
    Field field = new Field(f);
    print(field);
    bool cont = true;
    while (cont) {
        cont = false;
        for (Point p in all_numbers(field)) {
            if (p.score == 0) {
                mark_safe(field, p);
            }
            for (Point q in scan(field, p)) {
                cont = true;
                flag(field, q);
            }
        }
    }
    List<Point> safe = new List<Point>();
    for (int x = 0; x < field.length; x++) {
        for (int y = 0; y < field.width; y++) {
            Point p = field.query(new Point(x, y));
            if (p.ptype == PointType.safe && !safe.contains(p))
                safe.add(p);
        }
    }
    print('\n');
    print(field);
    for (Point p in safe) {
        print('${p.x}, ${p.y}');
    }
}
