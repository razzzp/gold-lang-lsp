
use crate::utils::{IRange, Position, Range};

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
    Colon,
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
    MultiLang,

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
    Comma,

    // constants
    StringConstant,
    NumericConstant
}

#[derive(Debug,Clone)]
pub struct Token {
    pub raw_pos: usize,
    pub pos: Position,
    pub range: Range,
    pub token_type: TokenType,
    pub value: Option<String>
}

impl IRange for Token {
    fn get_range(&self) -> Range{
        self.range.clone()
    }
}

