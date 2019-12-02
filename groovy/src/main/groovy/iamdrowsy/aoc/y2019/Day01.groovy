package iamdrowsy.aoc.y2019
import iamdrowsy.aoc.Util

class Day01 {

    static Integer day = 1;

    static Integer neededFuel(Integer weight) {
        return (weight.intdiv(3)) - 2
    }

    static String solve1(String input) {
        input.split().collect{neededFuel(it.toInteger())}.sum();
    }

    static Integer allNeededFuel(Integer weight) {
        Integer needed = 0;
        Integer additional = neededFuel(weight);
        while (additional > 0) {
            needed += additional;
            additional = neededFuel(additional);
        }
        return needed;
    }

    static String solve2(String input) {
        input.split().collect{allNeededFuel(it.toInteger())}.sum();
    }

    static void main(String[] args) {
        Util.check(day,1,solve1(Util.input(day)));
        Util.check(day,2,solve2(Util.input(day)));
    }
}