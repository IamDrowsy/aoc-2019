package iamdrowsy.aoc

import java.io.File

class Util {

    static def input(Integer day) {
        String fileName = String.format('../data/d%02d-input.txt', day);
        new File(fileName).text;
    }

    static def check(Integer day, Integer part, String solution) {
        String fileName = String.format('../data/d%02d-part-%d.txt', day, part);
        String expected = new File(fileName).text;
        assert expected == solution
        println("Got correct solution for day ${day} part ${part}: ${solution}");
    }
}
