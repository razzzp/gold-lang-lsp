use std::slice::Iter;


#[derive(Debug, PartialEq, Eq, Clone)]
pub enum TokenType {
    // types
    Absolute,
    Abstract,
    Array,
    Boolean,
    Char,
    Const,
    CString,
    Decimal,
    External,
    Final,
    Forward,
    InOut,
    Int,
    Int1,
    Int2,
    Int4,
    Int8,
    Inverse,
    ListOf,
    Memory,
    Num,
    Num4,
    Num8,
    Num10,
    Of,
    Override,
    Private,
    Protected,
    Record,
    RefTo,
    ReImplem,
    String,
    Type,
    Var,
    Versioned,
    Class,
    Module,
    Text,

    // control
    If,
    EndIf,
    Begin,
    Break,
    Catch,
    Continue,
    DownTo,
    Else,
    ElseIf,
    End,
    For,
    ForEach,
    EndFor,
    EndClass,
    Func,
    EndFunc,
    Proc,
    EndProc,
    EndLoop,
    EndRecord,
    EndSwitch,
    EndTry,
    EndWhen,
    EndWhile,
    Exit,
    Finally,
    Loop,
    NativeRecord,
    EndNativeRecord,
    Repeat,
    Return,
    Step,
    Switch,
    Throw,
    To,
    Try,
    Until,
    When,
    While,

    // operators
    OBracket,
    CBracket,
    OSqrBracket,
    CSqrBracket,
    OCurBracket,
    CCurBracket,
    Plus,
    Minus,
    Equals,
    LessThan,
    LessThanOrEqual,
    GreaterThan,
    GreaterThanOrEqual,
    Multiply,
    Divide,
    Modulus,
    StringConcat,
    LeftShift,
    RightShift,
    NotEquals,
    Increment,
    IncrementAssign,
    Decrement,
    DecrementAssign,
    DeepAssign,
    TypeAssign,
    Dot,
    AddressOf,
    And,
    BAnd,
    BNot,
    BOr,
    BXor,
    In,
    Like,
    Not,
    Or,
    Xor,

    // intrinsics
    MethodName,
    ModuleName,
    Move,
    Chr,
    Concat,
    Dispose,
    First,
    Last,
    Inherited,
    InstanceOf,
    Length,
    Member,
    MetaModelEntity,
    New,
    Nil,
    Ord,
    Pass,
    Pred,
    Scenario,
    SizeOf,
    Succ,
    Upcase,
    Uses,
    Using,
    Write,
    WriteLn,
    TSelf,
    Result,

    // oql
    AllVersionsOf,
    Descending,
    Distinct,
    From,
    OQL,
    Fetch,
    Select,
    OQLClassId,
    OQLCount,
    OQLMax,
    OQLMin,
    OQLSum,
    OQLUpdateDate,
    OQLUpdateTime,
    Order,
    By,
    PhantomsToo,
    Where,

    // others
    SingleQuote,
    DoubleQuotes,
    Comment,
    Identifier,
    Comma
}

#[derive(Debug,Clone)]
pub struct Token {
    pub pos: usize,
    pub token_type: TokenType,
    pub value: Option<String>
}

pub struct Range {
    pub start: Position,
    pub end: Position
}
pub struct Position {
    pub line: usize,
    pub character: usize
}

pub struct Tokens<'a> {
    tokens: &'a [Token],
    start: usize,
    end: usize
}

impl<'a> Tokens<'a>{
    pub fn new(tokens: &'a [Token]) -> Tokens<'a> {
        Self {
            tokens,
            start: 0,
            end: tokens.len()
        }
    }

    pub fn iter(&'a self) -> Iter<'a, Token> {
        self.tokens.iter()
    }
}