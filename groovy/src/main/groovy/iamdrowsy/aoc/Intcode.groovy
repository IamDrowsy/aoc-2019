package iamdrowsy.aoc

class Intcode {

    def Integer[] memory;
    def instructionPointer = 0;
    def Integer result;

    def Intcode(String memoryString) {
        memory = memoryString.split(',').collect{it.toInteger()};
    }

    def Intcode(memory) {
        this.memory = memory;
    }

    def Intcode clone() {
        return new Intcode(this.memory.clone());
    }

    def setMemory(index, val) {
        memory[index] = val
    }

    def step() {
        Integer instruction = memory[instructionPointer];
        switch(instruction) {
            case 1:
                memory[memory[instructionPointer + 3]] = memory[memory[instructionPointer + 1]] + memory[memory[instructionPointer + 2]];
                instructionPointer += 4;
                break;
            case 2:
                memory[memory[instructionPointer + 3]] = memory[memory[instructionPointer + 1]] * memory[memory[instructionPointer + 2]];
                instructionPointer += 4;
                break;
            case 99:
                result = memory[0];
        }
    }

    def getResult() {
        while (result == null) {
            step();
        }
        return result;
    }
}
