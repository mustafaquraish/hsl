use crate::ast::{Node, NodeKind, Operator};
use crate::vm::Value;
use crate::vm::bytecode::{Chunk, OpCode};

pub struct Compiler {
    pub chunk: Chunk,
}

impl Compiler {
    pub fn new() -> Self {
        Self {
            chunk: Chunk::new(),
        }
    }

    pub fn compile_program(&mut self, program: &Node) {
        let NodeKind::Block(stmts) = &program.kind else {
            unreachable!()
        };
        for stmt in stmts {
            self.compile(stmt);
        }
        if crate::ARGS.debug() {
            self.chunk.disassemble();
        }
    }
    pub fn handle_integer_const(&mut self, val: isize) {
        match val {
            -128..=127 => self.chunk.write_const_short(val as i8),
            _ => self.chunk.write_const_long(Value::Integer(val)),
        }
    }

    pub fn compile(&mut self, node: &Node) {
        match &node.kind {
            NodeKind::Block(_) => unimplemented!("awaiting scopes"),
            NodeKind::Echo(expr) => {
                self.compile(expr);
                self.chunk.write_op(OpCode::Echo);
            }
            NodeKind::UnaryOperation(op, val) => {
                match op {
                    Operator::Minus => match val.kind {
                        NodeKind::IntegerLiteral(val) => {
                            self.handle_integer_const(-(val as isize));
                            return;
                        }
                        NodeKind::FloatLiteral(val) => {
                            self.chunk.write_const_long(Value::Float(-val));
                            return;
                        }
                        _ => (),
                    },
                    _ => (),
                }
                self.compile(val);
                self.chunk.write_op(match op {
                    Operator::Minus => OpCode::Neg,
                    _ => unreachable!(),
                })
            }
            NodeKind::BinaryOperation(op, lhs, rhs) => {
                // Todo: handle compound expressions, i.e. 1<x<=3
                self.compile(lhs);
                self.compile(rhs);
                self.chunk.write_op(match op {
                    Operator::Plus => OpCode::Add,
                    Operator::Minus => OpCode::Sub,
                    Operator::Star => OpCode::Mul,
                    Operator::Slash => OpCode::Div,
                    _ => unreachable!(),
                });
            }
            NodeKind::Identifier(_) => unimplemented!("awaiting var declaration"),
            NodeKind::StringLiteral(val) => self.chunk.write_const_long(Value::String(val.clone())),
            NodeKind::FloatLiteral(val) => self.chunk.write_const_long(Value::Float(*val)),
            NodeKind::IntegerLiteral(val) => self.handle_integer_const(*val as isize),
            NodeKind::BooleanLiteral(val) => {
                self.chunk
                    .write_op(if *val { OpCode::True } else { OpCode::False })
            }
        }
    }
}
