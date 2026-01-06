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
            self.compile_statement(stmt);
        }
        if crate::ARGS.debug() {
            self.chunk.disassemble();
        }
    }

    pub fn compile_statement(&mut self, node: &Node) {
        match &node.kind {
            NodeKind::Echo(expr) => {
                self.compile_expression(expr);
                self.chunk.write_op(OpCode::Echo);
            }
            NodeKind::Assert(expr, message) => {
                self.compile_expression(expr);
                self.chunk.write_op(OpCode::Not);
                self.chunk.write_op(OpCode::JumpIfFalse);
                let start = self.chunk.source.len() - 1;
                self.chunk.write_op(OpCode::Nop);
                self.chunk.write_op(OpCode::Nop);
                self.chunk.write_const_long(Value::String(message.clone()));
                self.chunk.write_op(OpCode::ErrEcho);
                self.chunk.write_op(OpCode::Stop);
                self.chunk.write_u8(1);
                let end = self.chunk.source.len() - 1;
                let jump_offset = (end - start - 2) as u16;
                self.chunk.source[start + 1] = (jump_offset >> 8) as u8;
                self.chunk.source[start + 2] = (jump_offset) as u8;
            }
            _ => {
                self.compile_expression(node);
                if node.expr_stmt {
                    self.chunk.write_op(OpCode::Pop);
                }
            }
        }
    }

    pub fn compile_expression(&mut self, node: &Node) {
        macro_rules! unimplemented_op {
            ($op:expr) => {
                unimplemented!("Operator {:?}", $op)
            };
        }

        match &node.kind {
            NodeKind::Block(_) => unimplemented!("awaiting scopes"),
            NodeKind::UnaryOperation(Operator::UnaryPlus, val) => self.compile_expression(val),
            NodeKind::UnaryOperation(op, val) => {
                self.compile_expression(val);
                self.chunk.write_op(match op {
                    Operator::UnaryMinus => OpCode::Neg,
                    Operator::Not => OpCode::Not,
                    _ => unimplemented_op!(op),
                })
            }
            NodeKind::BinaryOperation(op, lhs, rhs) => {
                // Todo: handle compound expressions, i.e. 1<x<=3
                self.compile_expression(lhs);
                self.compile_expression(rhs);
                self.chunk.write_op(match op {
                    Operator::Or => OpCode::Or,
                    Operator::And => OpCode::And,
                    Operator::Equal => OpCode::Equal,
                    Operator::NotEqual => OpCode::NotEqual,
                    Operator::Greater => OpCode::Greater,
                    Operator::GreaterEqual => OpCode::GreaterEqual,
                    Operator::Less => OpCode::Less,
                    Operator::LessEqual => OpCode::LessEqual,
                    Operator::BitwiseOr => OpCode::BitOr,
                    Operator::BitwiseXor => OpCode::BitXor,
                    Operator::BitwiseAnd => OpCode::BitAnd,
                    Operator::ShiftLeft => OpCode::ShiftLeft,
                    Operator::ShiftRight => OpCode::ShiftRight,
                    Operator::Add => OpCode::Add,
                    Operator::Subtract => OpCode::Sub,
                    Operator::Multiply => OpCode::Mul,
                    Operator::Divide => OpCode::Div,
                    Operator::Modulo => OpCode::Mod,
                    Operator::Power => OpCode::Pow,
                    _ => unimplemented_op!(op),
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
            _ => panic!("{:?}", node),
        }
    }

    pub fn handle_integer_const(&mut self, val: isize) {
        // todo: Need to add a pass to the compiler to allow negative numbers to be inlined.
        match val {
            -128..=127 => self.chunk.write_const_short(val as i8),
            _ => self.chunk.write_const_long(Value::Integer(val)),
        }
    }
}
