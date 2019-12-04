package iamdrowsy.aoc.y2019
import iamdrowsy.aoc.Util

class Day04 {

    static Integer day = 4;

    // it's easier to get them revers and for the tasks we don't care
    static Integer[] reversedDigits(Integer i) {
        def result = new ArrayList<Integer>();
        while(i != 0) {
            result.add(i % 10);
            i = i.intdiv(10);
        }
        return result;
    }

    static smallestDiff(Integer[] digits) {
        def result = Integer.MAX_VALUE;
        for (def i = 1; i < digits.length; i++) {
            def diff = digits[i-1] - digits[i];
            if (diff < result) {
                result = diff;
            }
        }
        return result;
    }

    static Integer[] candidates(String input) {
        def bounds = input.split('-');
        def upper = bounds[0].toInteger();
        def lower = bounds[1].toInteger();
        lower..upper;
    }

    static String solve1(String input) {
        candidates(input).findAll {smallestDiff(reversedDigits(it)) == 0}.size();
    }

    static hasTwoOfANumber(Integer[] digits) {
        (digits.countBy {it}).containsValue(2);
    }

    static String solve2(String input) {
        (candidates(input).findAll {
            def digits = reversedDigits(it);
            smallestDiff(digits) == 0 && hasTwoOfANumber(digits)}).size();
    }

    static void main(String[] args) {
        Util.check(day,1,solve1(Util.input(day)));
        Util.check(day,2,solve2(Util.input(day)));
    }
}