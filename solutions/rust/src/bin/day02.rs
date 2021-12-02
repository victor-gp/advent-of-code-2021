// https://adventofcode.com/2021/day/2

use std::error::Error;
use std::fs::File;
use std::io::{prelude::*, BufReader};
use std::str::FromStr;

fn main() -> Result<(), Box<dyn Error>> {
    let input_file = File::open("../../input/day02-in.txt")?;
    let input_reader = BufReader::new(input_file);
    let commands_iter = input_reader.lines().map(|line| {
        line.expect("error when unwrapping a line")
            .parse::<Command>()
            .expect("error when parsing a Command str")
    });

    let starting_pos = Position { horizontal: 0, depth: 0 };
    let eval_cmd = |pos: Position, cmd| pos.after_command(&cmd);
    let final_pos = commands_iter.fold(starting_pos, eval_cmd);

    eprintln!("final position: {:?}", final_pos);
    println!("{}", final_pos.horizontal * final_pos.depth);
    Ok(())
}

type Units = u32;

#[derive(Debug)]
struct Position {
    horizontal: Units,
    depth: Units,
}

enum Command {
    Forward(Units),
    Down(Units),
    Up(Units),
}

impl Position {
    fn after_command(&self, cmd: &Command) -> Position {
        match cmd {
            Command::Forward(units) => Position {
                horizontal: self.horizontal + units,
                ..*self
            },
            Command::Down(units) => Position {
                depth: self.depth + units,
                ..*self
            },
            Command::Up(units) => Position {
                depth: self.depth - units,
                ..*self
            },
        }
    }
}

impl FromStr for Command {
    type Err = Box<dyn Error>;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let tokens: Vec<&str> = s.split_whitespace().collect();
        let units = tokens[1].parse::<Units>()?;
        let command = match tokens[0] {
            "forward" => Command::Forward(units),
            "down" => Command::Down(units),
            "up" => Command::Up(units),
            _ => panic!("Unrecognized command verb"), // nice: change to error
        };
        Ok(command)
    }
}
