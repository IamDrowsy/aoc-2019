import { readFileSync } from 'fs';
import { equal } from 'assert';

export function input(day: number) {
    const padded = day.toString().padStart(2, '0');
    const fileName = `../data/d${padded}-input.txt`;
    return readFileSync(fileName, 'utf8').trim();
}

export function check(day: number, part: number, solution: any) {
    const padded = day.toString().padStart(2, '0');
    const fileName = `../data/d${padded}-part-${part}.txt`
    const expected = readFileSync(fileName, 'utf8').trim();
    it(`day ${day}, ${part}`, function() {equal (solution.toString(), expected)});
}