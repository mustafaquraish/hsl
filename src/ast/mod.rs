use crate::ast::span::Span;
use name_variant::NamedVariant;
use std::fmt::{Debug, Display, Formatter};
use token::TokenKind;

pub mod lexer;
pub mod parser;
pub mod span;
pub mod token;

#[derive(NamedVariant, Copy, Clone)]
pub enum Operator {
    ShiftLeft,
    ShiftRight,
    BitwiseOr,
    BitwiseAnd,
    BitwiseXor,
    UnaryPlus,
    UnaryMinus,
    Add,
    And,
    Assign,
    Divide,
    Equal,
    Greater,
    GreaterEqual,
    Less,
    LessEqual,
    Modulo,
    Multiply,
    Not,
    NotEqual,
    Or,
    Power,
    Subtract,
}

impl Debug for Operator {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}", self.variant_name())
    }
}

impl TokenKind {
    pub fn as_prefix(self) -> Option<(Operator, (), u8)> {
        Some(match self {
            TokenKind::Bang => (Operator::Not, (), 1),
            TokenKind::Plus => (Operator::UnaryPlus, (), 2),
            TokenKind::Minus => (Operator::UnaryMinus, (), 2),
            _ => return None,
        })
    }

    pub fn as_infix(self) -> Option<(Operator, u8, u8)> {
        Some(match self {
            // Lower Precedence
            TokenKind::Equals => (Operator::Assign, 2, 1),

            TokenKind::Or => (Operator::Or, 3, 4),
            TokenKind::And => (Operator::And, 4, 5),

            TokenKind::EqualsEquals => (Operator::Equal, 3, 4),
            TokenKind::BangEquals => (Operator::NotEqual, 3, 4),
            TokenKind::Greater => (Operator::Greater, 3, 4),
            TokenKind::GreaterEquals => (Operator::GreaterEqual, 3, 4),
            TokenKind::Less => (Operator::Less, 3, 4),
            TokenKind::LessEquals => (Operator::LessEqual, 3, 4),

            TokenKind::Pipe => (Operator::BitwiseOr, 4, 5),
            TokenKind::Caret => (Operator::BitwiseXor, 5, 6),
            TokenKind::Ampersand => (Operator::BitwiseAnd, 6, 7),
            TokenKind::LessLess => (Operator::ShiftLeft, 7, 8),
            TokenKind::GreaterGreater => (Operator::ShiftRight, 7, 8),

            TokenKind::Plus => (Operator::Add, 8, 9),
            TokenKind::Minus => (Operator::Subtract, 8, 9),
            TokenKind::Star => (Operator::Multiply, 9, 10),
            TokenKind::Slash => (Operator::Divide, 9, 10),
            TokenKind::Percent => (Operator::Modulo, 9, 10),

            TokenKind::StarStar => (Operator::Power, 19, 18),
            // Higher Precedence
            _ => return None,
        })
    }

    pub fn as_postfix(self) -> Option<(Operator, u8, ())> {
        // Some(match self {
        //     _ => return None,
        // })
        None
    }
}

#[derive(NamedVariant, Clone)]
pub enum NodeKind {
    Block(Vec<Node>),
    Echo(Box<Node>),
    Assert(Box<Node>, String),
    UnaryOperation(Operator, Box<Node>),
    BinaryOperation(Operator, Box<Node>, Box<Node>),
    Identifier(String),
    StringLiteral(String),
    FloatLiteral(f64),
    IntegerLiteral(usize),
    BooleanLiteral(bool),
}

impl NodeKind {
    pub fn make(self, span: Span) -> Node {
        Node {
            kind: self,
            span,
            expr: false,
            expr_stmt: false,
        }
    }
}

#[derive(Clone)]
pub struct Node {
    pub kind: NodeKind,
    pub span: Span,
    pub expr: bool,
    pub expr_stmt: bool,
}

impl Display for Node {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            NodeFormatter {
                node: self,
                indent: 0,
            }
        )
    }
}

impl Debug for Node {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{self}")
    }
}

struct Indent<F> {
    f: F,
    indent: usize,
    stored_space: usize,
}

impl<F: std::fmt::Write> Indent<F> {
    pub fn new(f: F, indent: usize) -> Self {
        Self {
            f,
            indent,
            stored_space: indent,
        }
    }

    pub fn indent(&mut self, indent: usize) {
        self.indent += indent;
        self.stored_space = self.indent;
    }
    pub fn dedent(&mut self, indent: usize) {
        self.indent = self.indent.saturating_sub(indent);
        self.stored_space = self.indent;
    }
}

impl<F: std::fmt::Write> std::fmt::Write for Indent<F> {
    fn write_str(&mut self, s: &str) -> std::fmt::Result {
        for c in s.chars() {
            self.write_char(c)?;
        }
        Ok(())
    }

    fn write_char(&mut self, c: char) -> std::fmt::Result {
        match c {
            '\n' => {
                self.f.write_char('\n')?;
                self.stored_space = self.indent;
            }
            '\r' => {
                self.stored_space = 0;
            }
            '\t' => {
                self.indent(2);
            }
            '\0' => {
                self.dedent(2);
            }
            ' ' => {
                self.stored_space += 1;
            }
            _ if c.is_whitespace() => {
                unimplemented!("unusual space characters aren't allowed");
            }
            _ => {
                for _ in 0..std::mem::take(&mut self.stored_space) {
                    self.f.write_char(' ')?;
                }
                self.f.write_char(c)?;
            }
        }
        Ok(())
    }
}

impl<F: std::fmt::Write> Indent<F> {
    fn write_fmt(&mut self, args: std::fmt::Arguments<'_>) -> std::fmt::Result {
        std::fmt::Write::write_fmt(self, args)
    }
}

struct NodeFormatter<'n> {
    node: &'n Node,
    indent: usize,
}

impl<'n> NodeFormatter<'n> {
    fn child(&self, node: &'n Node) -> Self {
        Self { node, indent: 2 }
    }
}

impl<'a> Display for NodeFormatter<'a> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let mut f = Indent::new(f, self.indent);
        let node = self.node;
        write!(f, "{}", node.kind.variant_name())?;
        match &node.kind {
            NodeKind::UnaryOperation(op, expr) => {
                write!(f, "({}) {{\n{}\n}}", op.variant_name(), self.child(expr))?;
            }
            NodeKind::Echo(expr) => {
                write!(f, "{{\n{}\n}}", self.child(expr))?;
            }
            NodeKind::Assert(expr, message) => {
                write!(f, "{{\n{}\n{}\n}}", self.child(expr), message)?;
            }
            NodeKind::BinaryOperation(op, lhs, rhs) => {
                write!(
                    f,
                    "({}) {{\n{}\n{}\n}}",
                    op.variant_name(),
                    self.child(lhs),
                    self.child(rhs)
                )?;
            }
            NodeKind::StringLiteral(val) => write!(f, "({val:?})")?,
            NodeKind::FloatLiteral(val) => write!(f, "({val})")?,
            NodeKind::IntegerLiteral(val) => write!(f, "({val})")?,
            NodeKind::BooleanLiteral(val) => write!(f, "({val})")?,
            NodeKind::Block(stmts) => {
                writeln!(f, "({} statements) {{", stmts.len())?;
                for stmt in stmts {
                    writeln!(f, "{}", self.child(stmt))?;
                }
                write!(f, "}}")?;
            }
            NodeKind::Identifier(val) => write!(f, "({val:?})")?,
        }
        write!(f, "[{:?}]", self.node.span)?;
        Ok(())
    }
}
