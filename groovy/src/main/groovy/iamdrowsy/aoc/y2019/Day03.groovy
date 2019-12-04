package iamdrowsy.aoc.y2019

import groovy.transform.Canonical
import groovy.transform.Sortable
import iamdrowsy.aoc.Util

// but extending it leads to hashcode like point, which we want
@Canonical
@Sortable(includes=['x', 'y'])
class Point {
    int x,y;

    def static add(Point p1, Point p2) {
        return (new Point(p1.x + p2.x, p1.y + p2.y));
    }

    def String toString() {
        '[' + x + ',' + y + ']';
    }

    def int distFromZero() {
        Math.abs(x) + Math.abs(y);
    }

    def int compare(Point p) {
        if (this.x == p.x && this.y == p.y) {
            return 0;
        } else {
            return (p.distFromZero() - this.distFromZero())
        }
    }
}

class Wire {
    List<Point> wire = new ArrayList<>();
    Point last = new Point(0,0);

    def Wire(String wireString) {
        this.addCommands(wireString);
    }

    def addRelative(Point relativeMovement) {
        def newPoint = Point.add(last, relativeMovement);
        wire.add(newPoint);
        last = newPoint;
    }

    def firstIndex(Point p) {
        wire.findIndexOf {it == p} + 1;
    }

    def add(String command) {
        def dir = command.substring(0,1);
        def distance = command.substring(1).toInteger();
        def Point relativeMovement = new Point(0,0);
        switch(dir) {
            case "U":
                relativeMovement = new Point(0,1);
                break;
            case "D":
                relativeMovement = new Point(0,-1);
                break;
            case "L":
                relativeMovement = new Point(-1, 0);
                break;
            case "R":
                relativeMovement = new Point(1,0);
            }
        for (int i = 0; i < distance; i++) {
            addRelative(relativeMovement);
        }
    }

    def addCommands(String commands) {
        for (command in commands.split(',')) {
            add(command);
        };
    }

    def String toString() {
        wire.collect({p -> p.toString()}).join(',');
    }
}

class Day03 {

    static Integer day = 3;

    static String solve1(String input) {
        def wireStrings = input.split();
        def wire1 = new Wire(wireStrings[0]);
        def wire2 = new Wire(wireStrings[1]);
        def intersections = wire1.wire.intersect(wire2.wire);
        intersections.sort({p -> p.distFromZero()})[0].distFromZero();
    }

    static String solve2(String input) {
        def wireStrings = input.split();
        def wire1 = new Wire(wireStrings[0]);
        def wire2 = new Wire(wireStrings[1]);
        def intersections = wire1.wire.intersect(wire2.wire);
        def result = intersections.sort({p -> wire1.firstIndex(p) + wire2.firstIndex(p)})[0];
        wire1.firstIndex(result) + wire2.firstIndex(result);
    }

    static void main(String[] args) {
        Util.check(day,1,solve1(Util.input(day)));
        Util.check(day,2,solve2(Util.input(day)));
    }

}