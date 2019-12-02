package iamdrowsy.aoc.y2019

import iamdrowsy.aoc.Intcode
import iamdrowsy.aoc.Util

class Day02 {

    static Integer day = 2;

    static String solve1(String input) {
        Intcode ic = new Intcode(input);
        ic.setMemory(1,12);
        ic.setMemory(2,2);
        return ic.getResult();
    }

    static String solve2(String input) {
        Intcode ic = new Intcode(input);
        for (Integer noun = 0; noun < 100; noun++) {
            for (Integer verb = 0; verb < 100; verb++) {
                Intcode current = ic.clone();
                current.setMemory(1,noun);
                current.setMemory(2,verb);

                if (current.getResult() == 19690720) {
                    return (100 * noun + verb);
                }
            }
        }
        return null;
    }

    static void main(String[] args) {
        Util.check(day,1,solve1(Util.input(day)));
        Util.check(day,2,solve2(Util.input(day)));
    }
}
