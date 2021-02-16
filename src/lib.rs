extern crate pest;

#[macro_use]
extern crate pest_derive;

mod command {
    use std::fmt;
    use std::str::FromStr;

    #[derive(Copy, Clone, Debug, Eq, PartialEq)]
    struct Square(u8, u8);

    impl Square {
        fn new(file: u8, rank: u8) -> Option<Self> {
            if file < 26 && rank < 26 {
                Some(Square(file, rank))
            } else {
                None
            }
        }
    }

    impl fmt::Display for Square {
        fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
            let file = (('a' as u8) + self.0) as char;
            write!(f, "{}{}", file, self.1 + 1)
        }
    }

    #[derive(Clone, Debug, Eq, PartialEq)]
    struct Position {}

    #[derive(Copy, Clone, Debug, Eq, PartialEq)]
    struct Move(Square, Square);

    impl Move {}

    impl fmt::Display for Move {
        fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
            write!(f, "{}-{}", self.0, self.1)
        }
    }

    #[derive(Copy, Clone, Debug, Eq, PartialEq)]
    enum Side {
        Attackers,
        Deffenders,
    }

    impl FromStr for Side {
        type Err = String;

        fn from_str(s: &str) -> Result<Self, Self::Err> {
            match s {
                "attackers" => Ok(Side::Attackers),
                "defenders" => Ok(Side::Deffenders),
                _ => Err(format!("Invalid side: {}", s).into()),
            }
        }
    }

    #[derive(Copy, Clone, Debug, Eq, PartialEq)]
    enum ErrorCode {
        WrongSide,
        IllegalMove,
        BerserkModeWrongSide,
        BerserkModeIllegalMove,
    }

    impl FromStr for ErrorCode {
        type Err = String;

        fn from_str(s: &str) -> Result<Self, Self::Err> {
            match s {
                "1" => Ok(ErrorCode::WrongSide),
                "2" => Ok(ErrorCode::IllegalMove),
                "3" => Ok(ErrorCode::BerserkModeWrongSide),
                "4" => Ok(ErrorCode::BerserkModeIllegalMove),
                _ => Err(format!("Invalid error code: {}", s)),
            }
        }
    }

    #[derive(Copy, Clone, Debug, Eq, PartialEq)]
    enum FinishReason {
        ExitBeforeVictory,
        Draw,
        AttackersWin,
        DeffendersWin,
    }

    impl FromStr for FinishReason {
        type Err = String;

        fn from_str(s: &str) -> Result<Self, Self::Err> {
            match s {
                "0" => Ok(FinishReason::ExitBeforeVictory),
                "1" => Ok(FinishReason::Draw),
                "2" => Ok(FinishReason::AttackersWin),
                "3" => Ok(FinishReason::DeffendersWin),
                _ => Err(format!("Invalid reason: {}", s)),
            }
        }
    }

    #[derive(Clone, Debug, Eq, PartialEq)]
    enum OpenTaflCommand {
        Rules(String),      // TODO
        Position(Position), // TODO
        Side(Side),
        Clock(u64, u64, u64, u64, u64),
        Analyze(u64, u64),
        Play(Side),
        Move(Position), // TODO
        Error(ErrorCode),
        OponentMove(Vec<Move>, Position), // TODO
        Finish(FinishReason),
        Goodbye,
    }

    use anyhow::{Error, Result};
    use pest::Parser;

    #[derive(Parser)]
    #[grammar = "command_grammar.pest"]
    struct CommandParser;

    impl FromStr for OpenTaflCommand {
        type Err = Error;

        fn from_str(s: &str) -> Result<Self> {
            use pest::iterators::Pair;

            let cmd = CommandParser::parse(Rule::cmd, s)?.next().unwrap(); // unfailable

            fn parse_value(pair: Pair<Rule>) -> OpenTaflCommand {
                match pair.as_rule() {
                    Rule::cmd => parse_value(pair.into_inner().next().unwrap()),

                    Rule::goodbye_cmd => OpenTaflCommand::Goodbye,

                    Rule::finish_cmd => {
                        let s = pair.into_inner().next().unwrap().as_str();
                        OpenTaflCommand::Finish(s.parse().unwrap())
                    }

                    Rule::error_cmd => {
                        let s = pair.into_inner().next().unwrap().as_str();
                        OpenTaflCommand::Error(s.parse().unwrap())
                    }

                    Rule::side_cmd => {
                        let s = pair.into_inner().next().unwrap().as_str();
                        OpenTaflCommand::Side(s.parse().unwrap())
                    }

                    Rule::play_cmd => {
                        let s = pair.into_inner().next().unwrap().as_str();
                        OpenTaflCommand::Play(s.parse().unwrap())
                    }

                    Rule::clock_cmd => {
                        let vals = pair
                            .into_inner()
                            .map(|subp| subp.as_str().parse::<u64>().unwrap())
                            .collect::<Vec<u64>>();
                        OpenTaflCommand::Clock(vals[0], vals[1], vals[2], vals[3], vals[4])
                    }

                    Rule::analyze_cmd => {
                        let vals = pair
                            .into_inner()
                            .map(|subp| subp.as_str().parse::<u64>().unwrap())
                            .collect::<Vec<u64>>();
                        OpenTaflCommand::Analyze(vals[0], vals[1])
                    }

                    Rule::WHITESPACE
                    | Rule::finish_code
                    | Rule::error_code
                    | Rule::side
                    | Rule::int
                    | Rule::ot_marker => {
                        unreachable!()
                    }
                }
            }

            Ok(parse_value(cmd))
        }
    }

    enum EngineCommand {
        Hello,
        Rules,
        Position,
        Side,
        Clock,
        SimpleMoves(bool),
        Analysis(u64, Vec<(Vec<Move>, f64)>),
        Move(Move),
        Error(bool, String),
        Status(String),
    }

    impl fmt::Display for EngineCommand {
        fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
            match self {
                EngineCommand::Hello => write!(f, "hello"),
                EngineCommand::Rules => write!(f, "rules"),
                EngineCommand::Position => write!(f, "position"),
                EngineCommand::Side => write!(f, "side"),
                EngineCommand::Clock => write!(f, "clock"),

                EngineCommand::SimpleMoves(on) => {
                    write!(f, "simple-moves {}", if *on { "on" } else { "off" })
                }

                EngineCommand::Error(critical, message) => write!(
                    f,
                    "error {} {}",
                    if *critical { "-1" } else { "0" },
                    message
                ),

                EngineCommand::Status(message) => write!(f, "status {}", message),

                EngineCommand::Move(m) => write!(f, "move {}", m.to_string()),

                EngineCommand::Analysis(_, _) => write!(f, "error -1 not implemented"),
            }
        }
    }

    #[cfg(test)]
    mod tests {
        use super::*;
        use fehler::throws;

        #[test]
        fn square_new() {
            assert_eq!(Square::new(0, 0), Some(Square(0, 0)));
            assert_eq!(Square::new(25, 25), Some(Square(25, 25)));

            assert_eq!(Square::new(26, 0), None);
            assert_eq!(Square::new(0, 26), None);
        }

        #[test]
        fn square_display() {
            assert_eq!(format!("{}", Square(0, 0)), "a1");
            assert_eq!(format!("{}", Square(1, 9)), "b10");
            assert_eq!(format!("{}", Square(25, 0)), "z1");

            assert_eq!(format!("{}", Square(0, 25)), "a26");
            assert_eq!(format!("{}", Square(25, 25)), "z26");
        }

        #[test]
        fn move_display() {
            let m = Move(Square(0, 0), Square(25, 25));
            let expected = "a1-z26";
            assert_eq!(format!("{}", m), expected);
        }

        #[test]
        fn engine_command_display() {
            assert_eq!(format!("{}", EngineCommand::Hello), "hello");
            assert_eq!(format!("{}", EngineCommand::Rules), "rules");
            assert_eq!(format!("{}", EngineCommand::Position), "position");
            assert_eq!(format!("{}", EngineCommand::Side), "side");
            assert_eq!(format!("{}", EngineCommand::Clock), "clock");

            assert_eq!(
                format!("{}", EngineCommand::SimpleMoves(true)),
                "simple-moves on"
            );
            assert_eq!(
                format!("{}", EngineCommand::SimpleMoves(false)),
                "simple-moves off"
            );

            assert_eq!(
                format!("{}", EngineCommand::Error(true, "asdf".into())),
                "error -1 asdf"
            );
            assert_eq!(
                format!("{}", EngineCommand::Error(false, "asdf".into())),
                "error 0 asdf"
            );

            assert_eq!(
                format!("{}", EngineCommand::Status("asdf".into())),
                "status asdf"
            );

            let m = Move(Square(0, 0), Square(25, 25));
            assert_eq!(format!("{}", EngineCommand::Move(m)), "move a1-z26");

            assert_eq!(
                format!("{}", EngineCommand::Analysis(0, vec![])),
                "error -1 not implemented"
            );
        }

        #[test]
        fn side_from_str() {
            assert_eq!(Side::from_str("attackers"), Ok(Side::Attackers));
            assert_eq!(Side::from_str("defenders"), Ok(Side::Deffenders));
            assert!(Side::from_str("asdf").is_err());
        }

        #[test]
        fn error_code_from_str() {
            assert_eq!(ErrorCode::from_str("1"), Ok(ErrorCode::WrongSide));
            assert_eq!(ErrorCode::from_str("2"), Ok(ErrorCode::IllegalMove));
            assert_eq!(
                ErrorCode::from_str("3"),
                Ok(ErrorCode::BerserkModeWrongSide)
            );
            assert_eq!(
                ErrorCode::from_str("4"),
                Ok(ErrorCode::BerserkModeIllegalMove)
            );
            assert!(ErrorCode::from_str("").is_err());
            assert!(ErrorCode::from_str("0").is_err());
            assert!(ErrorCode::from_str("5").is_err());
        }

        #[test]
        fn finish_reason_from_str() {
            assert_eq!(
                FinishReason::from_str("0"),
                Ok(FinishReason::ExitBeforeVictory)
            );
            assert_eq!(FinishReason::from_str("1"), Ok(FinishReason::Draw));
            assert_eq!(FinishReason::from_str("2"), Ok(FinishReason::AttackersWin));
            assert_eq!(FinishReason::from_str("3"), Ok(FinishReason::DeffendersWin));
            assert!(FinishReason::from_str("").is_err());
            assert!(FinishReason::from_str("-1").is_err());
            assert!(FinishReason::from_str("4").is_err());
        }

        #[throws]
        #[test]
        fn open_tafl_command_from_str() {
            // Goodbye
            assert_eq!(
                OpenTaflCommand::from_str("goodbye")?,
                OpenTaflCommand::Goodbye
            );

            // Finish
            assert_eq!(
                OpenTaflCommand::from_str("finish 0")?,
                OpenTaflCommand::Finish(FinishReason::ExitBeforeVictory),
            );
            assert_eq!(
                OpenTaflCommand::from_str("finish 1")?,
                OpenTaflCommand::Finish(FinishReason::Draw),
            );
            assert_eq!(
                OpenTaflCommand::from_str("finish 2")?,
                OpenTaflCommand::Finish(FinishReason::AttackersWin),
            );
            assert_eq!(
                OpenTaflCommand::from_str("finish 3")?,
                OpenTaflCommand::Finish(FinishReason::DeffendersWin),
            );
            assert!(OpenTaflCommand::from_str("finish -1").is_err());
            assert!(OpenTaflCommand::from_str("finish 4").is_err());

            // Error
            assert_eq!(
                OpenTaflCommand::from_str("error 1")?,
                OpenTaflCommand::Error(ErrorCode::WrongSide),
            );
            assert_eq!(
                OpenTaflCommand::from_str("error 2")?,
                OpenTaflCommand::Error(ErrorCode::IllegalMove),
            );
            assert_eq!(
                OpenTaflCommand::from_str("error 3")?,
                OpenTaflCommand::Error(ErrorCode::BerserkModeWrongSide),
            );
            assert_eq!(
                OpenTaflCommand::from_str("error 4")?,
                OpenTaflCommand::Error(ErrorCode::BerserkModeIllegalMove),
            );
            assert!(OpenTaflCommand::from_str("error 0").is_err());
            assert!(OpenTaflCommand::from_str("error 5").is_err());

            // Side
            assert_eq!(
                OpenTaflCommand::from_str("side attackers")?,
                OpenTaflCommand::Side(Side::Attackers),
            );
            assert_eq!(
                OpenTaflCommand::from_str("side defenders")?,
                OpenTaflCommand::Side(Side::Deffenders),
            );
            assert!(OpenTaflCommand::from_str("side asdf").is_err());

            // Play
            assert_eq!(
                OpenTaflCommand::from_str("play attackers")?,
                OpenTaflCommand::Play(Side::Attackers),
            );
            assert_eq!(
                OpenTaflCommand::from_str("play defenders")?,
                OpenTaflCommand::Play(Side::Deffenders),
            );
            assert!(OpenTaflCommand::from_str("play asdf").is_err());

            // Clock
            assert_eq!(
                OpenTaflCommand::from_str("clock 15 15 10 2 2")?,
                OpenTaflCommand::Clock(15, 15, 10, 2, 2),
            );
            assert_eq!(
                OpenTaflCommand::from_str("clock 15* 15* 10 2 2")?,
                OpenTaflCommand::Clock(15, 15, 10, 2, 2),
            );
            assert_eq!(
                OpenTaflCommand::from_str("clock 15* 15 10 2 2")?,
                OpenTaflCommand::Clock(15, 15, 10, 2, 2),
            );
            assert_eq!(
                OpenTaflCommand::from_str("clock 15 15* 10 2 2")?,
                OpenTaflCommand::Clock(15, 15, 10, 2, 2),
            );
            assert_eq!(
                OpenTaflCommand::from_str("clock 1 1 0 0 0")?,
                OpenTaflCommand::Clock(1, 1, 0, 0, 0),
            );
            assert!(OpenTaflCommand::from_str("clock 15** 15 10 2 2").is_err());
            assert!(OpenTaflCommand::from_str("clock 15 15** 10 2 2").is_err());
            assert!(OpenTaflCommand::from_str("clock 1 1 1 1").is_err());
            assert!(OpenTaflCommand::from_str("clock asdf").is_err());
            assert!(OpenTaflCommand::from_str("clock asdf").is_err());

            // Clock
            assert_eq!(
                OpenTaflCommand::from_str("analyze 15 15")?,
                OpenTaflCommand::Analyze(15, 15),
            );
            assert_eq!(
                OpenTaflCommand::from_str("analyze 1 1")?,
                OpenTaflCommand::Analyze(1, 1),
            );
            assert!(OpenTaflCommand::from_str("analyze 1").is_err());
            assert!(OpenTaflCommand::from_str("analyze").is_err());
            assert!(OpenTaflCommand::from_str("analyze asdf").is_err());
        }
    }
}
