mod command {
    use std::fmt;

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

    struct Position {}

    struct Move(Square, Square);

    impl Move {}

    impl fmt::Display for Move {
        fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
            write!(f, "{}-{}", self.0, self.1)
        }
    }

    enum Side {
        Attackers,
        Deffenders,
    }

    enum ErrorCode {
        WrongSide,
        IllegalMove,
        BerserkModeWrongSide,
        BerserkModeIllegalMove,
    }

    enum FinishReason {
        ExitBeforeVictory,
        Draw,
        AttackersWin,
        DeffendersWin,
    }

    enum OpenTaflCommand {
        Rules(String),
        Position(Position),
        Side(Side),
        Clock(u64, u64, u64, u64, u64),
        Analyze(u64, u64),
        Play(Side),
        Move(Position),
        Error(ErrorCode),
        OponentMove(Vec<Move>, Position),
        Finish(FinishReason),
        Goodbye,
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

                EngineCommand::Error(critical, message) => {
                    write!(
                        f,
                        "error {} {}",
                        if *critical { "-1" } else { "0" },
                        message
                    )
                }

                EngineCommand::Status(message) => write!(f, "status {}", message),

                EngineCommand::Move(m) => write!(f, "move {}", m.to_string()),

                EngineCommand::Analysis(_, _) => write!(f, "error -1 not implemented"),
            }
        }
    }

    #[cfg(test)]
    mod tests {
        use super::*;

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
    }
}
