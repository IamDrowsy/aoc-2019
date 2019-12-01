
import { check, input } from '../util';

const day = 1;

function fuel(weight: number) {
    return Math.floor(weight / 3) - 2
}

function solve1(input) {
    return input.split('\n').map(fuel).reduce((x, y) => x + y, 0);
}

function allFuel(weight: number) {
    return allFuelIntern(weight, 0);
}

function allFuelIntern(weight: number, sum: number) {
    const additional = fuel(weight);
    if (additional > 0) {
        return allFuelIntern(additional, sum + additional);
    } else {
        return sum;
    }
}

function solve2(input) {
    return input.split('\n').map(allFuel).reduce((x, y) => x + y, 0);
}

describe('Day 01', function() {
    describe('Part 01', function() {
        check(day,1,solve1(input(day)));
    })
    describe('Part 02', function() {
        check(day,2,solve2(input(day)));
    })
})