enum TokenType {
  Let = 'let',
  Mut = 'mut',
  If = 'if',
  Else = 'else',
  Match = 'match',
  For = 'for',
  Type = 'type',
  Trait = 'trait',
  Where = 'where',
  Use = 'use',
  Mod = 'mod',
  Return = 'return',

  Number = 'Number',
  String = 'String',
  Bool = 'Bool',

  Identifier = 'Identifier',
  NumberLiteral = 'NumberLiteral',
  True = 'true',
  False = 'false',

  Assign = '=',
  Or = 'or',
  And = 'and',
  Eq = '==',
  Not_Eq = '!=',
  Lt = '<',
  Gt = '>',
  Lte = '<=',
  Gte = '>=',
  Plus = '+',
  Minus = '-',
  Bang = '!',
  ForwardSlash = '/',
  HASH = '#',
  TRIPLE_HASH = '###',
  Asterisk = '*',
  LParen = '(',
  RParen = ')',
  LBrace = '{',
  RBrace = '}',
  LBracket = '[',
  RBracket = ']',
  DoublePoint = ':',
  Underscore = '_',
  Comma = ',',
  Arrow = '=>',
  Dot = '.',
  Spread = '...',

  NewLine = '\n',
  Indent = 'INDENT',
  Dedent = 'DEDENT',

  Illegal = 'ILLEGAL',
  EOF = '\0',
}

const TokenTypeByKeyword = {
  let: TokenType.Let,
  mut: TokenType.Mut,
  if: TokenType.If,
  else: TokenType.Else,
  match: TokenType.Match,
  for: TokenType.For,
  type: TokenType.Type,
  trait: TokenType.Trait,
  where: TokenType.Where,
  use: TokenType.Use,
  mod: TokenType.Mod,
  return: TokenType.Return,
  true: TokenType.True,
  false: TokenType.False,
  or: TokenType.Or,
  and: TokenType.And,
} as const;

type Token = {
  type: TokenType;
  literal: string;
};

const createToken = (tokenType: TokenType, literal: string) => {
  return {
    type: tokenType,
    literal,
  };
};

export class Lexer {
  tokens = Array<Token>();
  private input: string;
  private currPos: number = 0;
  private nextPos: number = 1;
  private currIndentationLevel: number = 0;

  constructor(input: string) {
    this.input = input;
    this.generateTokens();
  }

  generateTokens = () => {
    let token = this.getNextToken();
    this.tokens.push(token);

    while (token.type !== TokenType.EOF) {
      this.skipWhitespace();
      token = this.getNextToken();
      this.tokens.push(token);
    }
  };

  getNextToken = () => {
    let curChar = this.readChar();
    let token = null;

    switch (curChar) {
      case TokenType.Assign:
        if (this.input[this.currPos] === TokenType.Assign) {
          curChar = this.readChar();
          token = createToken(TokenType.Eq, TokenType.Eq);
          break;
        }

        if (this.input[this.currPos] === TokenType.Gt) {
          curChar = this.readChar();
          token = createToken(TokenType.Arrow, TokenType.Arrow);
          break;
        }

        token = createToken(TokenType.Assign, curChar);
        break;
      case TokenType.Bang:
        if (this.input[this.currPos] === TokenType.Assign) {
          curChar = this.readChar();
          token = createToken(TokenType.Not_Eq, TokenType.Not_Eq);
          break;
        }
        token = createToken(TokenType.Bang, curChar);
        break;

      case TokenType.Lt:
        if (this.input[this.currPos] === TokenType.Assign) {
          this.readChar();
          token = createToken(TokenType.Lte, TokenType.Lte);
          break;
        }
        token = createToken(TokenType.Lt, TokenType.Lt);
        break;
      case TokenType.Gt:
        if (this.input[this.currPos] === TokenType.Assign) {
          curChar = this.readChar();
          token = createToken(TokenType.Gte, TokenType.Gte);
          break;
        }
        token = createToken(TokenType.Gt, TokenType.Gt);
        break;

      case TokenType.Plus:
        token = createToken(TokenType.Plus, TokenType.Plus);
        break;

      case TokenType.Minus:
        token = createToken(TokenType.Minus, TokenType.Minus);
        break;

      case TokenType.HASH:
        if (this.input[this.currPos] === TokenType.HASH) {
          curChar = this.readChar();
          if (this.input[this.currPos] === TokenType.HASH) {
            curChar = this.readChar();
            token = createToken(TokenType.TRIPLE_HASH, TokenType.TRIPLE_HASH);
            break;
          }
          token = createToken(TokenType.Illegal, TokenType.Illegal);
        }
        token = createToken(TokenType.HASH, TokenType.HASH);
        break;

      case TokenType.ForwardSlash:
        token = createToken(TokenType.ForwardSlash, TokenType.ForwardSlash);
        break;

      case TokenType.Asterisk:
        token = createToken(TokenType.Asterisk, TokenType.Asterisk);
        break;

      case TokenType.LParen:
        token = createToken(TokenType.LParen, TokenType.LParen);
        break;

      case TokenType.RParen:
        token = createToken(TokenType.RParen, TokenType.RParen);
        break;

      case TokenType.LBrace:
        token = createToken(TokenType.LBrace, TokenType.LBrace);
        break;

      case TokenType.RBrace:
        token = createToken(TokenType.RBrace, TokenType.RBrace);
        break;

      case TokenType.LBracket:
        token = createToken(TokenType.LBracket, TokenType.LBracket);
        break;

      case TokenType.RBracket:
        token = createToken(TokenType.RBracket, TokenType.RBracket);
        break;

      case TokenType.DoublePoint:
        token = createToken(TokenType.DoublePoint, TokenType.DoublePoint);
        break;

      case TokenType.Underscore:
        if (this.isIdentifierChar(this.input[this.currPos])) {
          this.unreadChar();
          const identifier = this.readIdentifier();
          token = createToken(TokenType.Identifier, identifier);
          break;
        }
        token = createToken(TokenType.Underscore, TokenType.Underscore);
        break;

      case TokenType.Comma:
        token = createToken(TokenType.Comma, TokenType.Comma);
        break;

      case TokenType.Dot:
        if (this.input[this.currPos] === TokenType.Dot) {
          curChar = this.readChar();
          if (this.input[this.currPos] === TokenType.Dot) {
            curChar = this.readChar();
            token = createToken(TokenType.Spread, TokenType.Spread);
            break;
          }
          token = createToken(TokenType.Illegal, TokenType.Illegal);
        }
        token = createToken(TokenType.Dot, TokenType.Dot);
        break;

      case TokenType.NewLine:
        const tabsLevel = this.skipAndCountTabs();

        if (tabsLevel > this.currIndentationLevel) {
          this.currIndentationLevel = tabsLevel;
          token = createToken(TokenType.Indent, TokenType.Indent);
        } else if (tabsLevel < this.currIndentationLevel) {
          this.currIndentationLevel = tabsLevel;
          token = createToken(TokenType.Dedent, TokenType.Dedent);
        } else {
          token = createToken(TokenType.NewLine, TokenType.NewLine);
        }

        break;
      case TokenType.EOF:
        token = createToken(TokenType.EOF, TokenType.EOF);
    }

    if (this.isLetter(curChar)) {
      this.unreadChar();
      const identifier = this.readIdentifier();

      const tokenType =
        TokenTypeByKeyword[identifier as keyof typeof TokenTypeByKeyword] ||
        TokenType.Identifier;

      token = createToken(tokenType, identifier);
    } else if (this.isDigit(curChar)) {
      this.unreadChar();
      const number = this.readNumber();
      token = createToken(TokenType.NumberLiteral, number);
    } else if (!token) {
      token = createToken(TokenType.Illegal, curChar);
    }

    return token as Token;
  };

  isLetter = (char: string) => {
    const charCode = char.charCodeAt(0);

    return (
      ('a'.charCodeAt(0) <= charCode && charCode <= 'z'.charCodeAt(0)) ||
      ('A'.charCodeAt(0) <= charCode && charCode <= 'Z'.charCodeAt(0))
    );
  };

  isDigit = (char: string) => {
    const charCode = char.charCodeAt(0);
    return '0'.charCodeAt(0) <= charCode && charCode <= '9'.charCodeAt(0);
  };

  isIdentifierChar = (char: string) => {
    return this.isLetter(char) || this.isDigit(char) || char === '_';
  };

  skipWhitespace = () => {
    let curChar = this.input[this.currPos];

    while (curChar === ' ' || curChar === '\t' || curChar === '\r') {
      this.readChar();
      curChar = this.input[this.currPos];
    }
  };

  skipAndCountTabs = () => {
    let tabs = 0;
    while (this.input[this.currPos] === '\t') {
      tabs++;
      this.readChar();
    }
    return tabs;
  };

  readIdentifier = () => {
    let startIdPosition = this.currPos;

    while (this.isIdentifierChar(this.input[this.currPos])) {
      this.readChar();
    }

    return this.input.slice(startIdPosition, this.currPos);
  };

  readNumber = () => {
    let startNumberPos = this.currPos;
    while (this.isDigit(this.input[this.currPos])) {
      this.readChar();
    }

    return this.input.slice(startNumberPos, this.currPos);
  };

  readChar = () => {
    if (this.nextPos >= this.input.length) {
      return '\0';
    }

    const char = this.input[this.currPos];
    this.currPos++;
    this.nextPos++;
    return char;
  };

  unreadChar = () => {
    this.currPos--;
    this.nextPos--;
  };
}
