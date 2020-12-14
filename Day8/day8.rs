use std::fs::File;
use std::io::{self, BufRead, BufReader};

#[derive(Eq, PartialEq, Hash, Copy, Clone, Debug)]
enum Op {
    NOP(i32),
    ACC(i32),
    JMP(i32),
}

#[derive(Debug)]
struct VM {
    instructions: Vec<Op>,
    ip: usize, // instruction pointer
    acc: i32,  // global accumulator
}

fn part1(ops: Vec<Op>) -> i32 {
    let mut vm = VM::new(ops);
    let num_instructions = vm.instructions.len();
    let mut executed_ops = vec![false; num_instructions];

    while vm.ip < num_instructions {
        if executed_ops[vm.ip] {
            break;
        }
        executed_ops[vm.ip] = true;
        vm.execute();
    }

    return vm.acc;
}

fn part2(ops: Vec<Op>) -> i32 {
    let mut vm = VM::new(ops.clone());
    let num_instructions = vm.instructions.len();
    let mut executed_ops = vec![false; num_instructions];

    for ip in 0..num_instructions {
        if !vm.patch_instruction(ip) {
            continue;
        }
        // run the patched VM
        while vm.ip < num_instructions {
            if executed_ops[vm.ip] {
                // found loop: reset everything
                executed_ops = vec![false; num_instructions];
                vm.reset(ops.clone());
                break;
            }
            executed_ops[vm.ip] = true;
            vm.execute();
        }
        if vm.ip == num_instructions {
            return vm.acc;
        }
    }

    // shouldn't reach this point
    return -1;
}

impl VM {
    fn new(ops: Vec<Op>) -> Self {
        VM {
            instructions: ops,
            ip: 0,
            acc: 0,
        }
    }

    fn reset(&mut self, ops: Vec<Op>) -> () {
        self.instructions = ops;
        self.acc = 0;
        self.ip = 0;
    }

    fn execute(&mut self) -> () {
        match self.instructions[self.ip] {
            Op::NOP(_) => self.ip += 1,
            Op::ACC(i) => {
                self.acc += i;
                self.ip += 1
            }
            Op::JMP(i) => {
                // avoid errors due to casts
                if i.is_negative() {
                    self.ip -= i.wrapping_abs() as u32 as usize;
                } else {
                    self.ip += i as usize;
                }
            }
        }
    }

    fn patch_instruction(&mut self, i: usize) -> bool {
        match self.instructions[i] {
            Op::NOP(n) => {
                self.instructions[i] = Op::JMP(n);
                true
            }
            Op::JMP(n) => {
                self.instructions[i] = Op::NOP(n);
                true
            }
            Op::ACC(_) => false,
        }
    }
}

fn parse_line(line: &str) -> Op {
    let op = &line[..3];
    let inc = &line[4..];
    let num = inc.parse::<i32>().unwrap();
    match op {
        "nop" => Op::NOP(num),
        "acc" => Op::ACC(num),
        "jmp" => Op::JMP(num),
        _ => panic!("invalid opcode"),
    }
}

fn main() -> Result<(), io::Error> {
    let file = File::open("input.txt")?;
    let reader = BufReader::new(file);
    let ops: Vec<Op> = reader
        .lines()
        .map(|line| parse_line(&line.unwrap()))
        .collect();

    // TODO make this more efficient by avoiding clones
    println!("Part 1: {}", part1(ops.clone()));
    println!("Part 2: {}", part2(ops.clone()));
    Ok(())
}
