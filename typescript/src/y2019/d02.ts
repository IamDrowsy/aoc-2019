import { check, input } from '../util';

const day = 2;

type Intcode = {
    memory: number[];
    ip: number;
    done: boolean;
    result?: number;
}

function step(m: Intcode) {
    const opCode = m.memory[m.ip];
    switch (opCode) {
        case 1: {
            m.memory[m.memory[m.ip + 3]] = m.memory[m.memory[m.ip + 1]] + m.memory[m.memory[m.ip + 2]];
            break;
        }
        case 2: {
            m.memory[m.memory[m.ip + 3]] = m.memory[m.memory[m.ip + 1]] * m.memory[m.memory[m.ip + 2]];
            break;
        }
        case 99: {
            m.done = true;
            m.result = m.memory[0];
        }
    } 
    m.ip += 4;
    return m;
}

function run(m: Intcode) {
    if (!m.done) {
        return run(step(m));
    } else {
        return m;
    }
}

function newIntcode(input: String) {
    return {memory: input.split(',').map(Number),
            ip: 0,
            done: false};
}

function solve1(input) {
    const m = newIntcode(input);
    m.memory[1] = 12;
    m.memory[2] = 2;
    return run(m).result;
}

function range(end) {
    return Array.from(Array(end).keys());
}

function solve2(input) {
    for (let noun in range(100)) {
        for (let verb in range(100)) {
            const m = newIntcode(input);
            m.memory[1] = Number(noun);
            m.memory[2] = Number(verb);
            if(run(m).result == 19690720) {
                return (100 * Number(noun) + Number(verb));
            }
        }
    }
}

describe('Day 01', function() {
    describe('Part 01', function() {
        check(day,1,solve1(input(day)));
    })
    describe('Part 02', function() {
        check(day,2,solve2(input(day)));
    })
})