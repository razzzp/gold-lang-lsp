

use std::sync::Arc;

use crate::utils::{IRange, Position, Range};

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum TokenType {
    
    Absolute,
    Abstract,
    Array,
    Const,
    External,
    Final,
    Forward,
    InOut,
    Inverse,
    ListOf,
    Memory,
    Of,
    Override,
    Private,
    Protected,
    Record,
    RefTo,
    ReImplem,
    Type,
    Var,
    Versioned,
    Class,
    Module,
    Sequence,

    //native types
    //  treat as identifier and handle later in semantic analzer
    // CString,
    // Boolean,
    // Char,
    // Decimal,
    // Int,
    // Int1,
    // Int2,
    // Int4,
    // Int8,
    // Num,
    // Num4,
    // Num8,
    // Num10,
    // String,
    // Text,

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
    Asterisk,
    Divide,
    Modulus,
    StringConcat,
    // single &, concat without space (not documented?)
    StringConcat2,
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
    // MethodName,
    // ModuleName,
    // Move,
    // Chr,
    // Concat,
    // Dispose,
    // First,
    // Last,
    Inherited,
    InstanceOf,
    // Length,
    // Member,
    // MetaModelEntity,
    // New,
    Nil,
    // Ord,
    Pass,
    // Pred,
    // Scenario,
    // SizeOf,
    // Succ,
    // Upcase,
    Uses,
    Using,
    // Write,
    // WriteLn,
    // TSelf,
    // Result,
    MultiLang,

    // oql
    AllVersionsOf,
    Descending,
    Distinct,
    From,
    OQL,
    Fetch,
    Select,
    // OQLClassId,
    // OQLCount,
    // OQLMax,
    // OQLMin,
    // OQLSum,
    // OQLUpdateDate,
    // OQLUpdateTime,
    Order,
    By,
    PhantomsToo,
    Where,
    Top,
    Conditional,
    Into,

    // others
    SingleQuote,
    DoubleQuotes,
    Comment,
    Identifier,
    Comma,
    Pound,

    // lterals
    StringLiteral,
    NumericLiteral,
    BooleanTrue,
    BooleanFalse
}

#[derive(Debug,Clone,PartialEq)]
pub struct Token {
    pub raw_pos: usize,
    pub range: Range,
    pub token_type: TokenType,
    pub value: Arc<str>
}

impl IRange for Token {
    fn get_range(&self) -> Range {
        self.range.clone()
    }
    fn set_range(&mut self, new_range: Range) {
        self.range=new_range
    }
    fn as_range(&self) -> &dyn IRange {
        self
    }
}

impl Token{
    pub fn get_raw_pos(&self) -> usize{
        self.raw_pos
    }
    pub fn get_pos(&self) -> Position{
        self.get_range().start
    }
    pub fn get_value_as_str(&self) -> &str {
        return &self.value
    }
    pub fn get_value(&self) -> Arc<str> {
        return self.value.clone()
    }
    pub fn to_string_val_and_pos(&self) -> String{
        format!("{}:{}", self.get_value_as_str(), self.get_pos().to_string_brief())
    }
}

