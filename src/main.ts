import { read } from 'fs';

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

  Illegal = 'ILLEGAL',
  EOF = 'EOF',
}

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

class Lexer {
  tokens = Array<Token>();
  private input: string;
  private currPos: number = 0;
  private nextPos: number = 1;

  constructor(input: string) {
    this.input = input;
    this.generateTokens();
  }

  generateTokens = () => {
    const token = this.getNextToken();
    this.tokens.push(token);
  };

  getNextToken = () => {
    let curChar = this.readChar();
    let token = null;

    switch (curChar) {
      case TokenType.Assign:
        if (this.peakNextChar() === TokenType.Assign) {
          curChar = this.readChar();
          token = createToken(TokenType.Eq, TokenType.Eq);
          break;
        }

        if (this.peakNextChar() === TokenType.Gt) {
          curChar = this.readChar();
          token = createToken(TokenType.Arrow, TokenType.Arrow);
          break;
        }

        token = createToken(TokenType.Assign, curChar);
        break;
      case TokenType.Bang:
        if (this.peakNextChar() === TokenType.Assign) {
          curChar = this.readChar();
          token = createToken(TokenType.Not_Eq, TokenType.Not_Eq);
          break;
        }
        token = createToken(TokenType.Bang, curChar);
        break;
      case TokenType.Lt:
        if (this.peakNextChar() === TokenType.Eq) {
          curChar = this.readChar();
          token = createToken(TokenType.Lte, TokenType.Lte);
          break;
        }
        token = createToken(TokenType.Lt, TokenType.Lt);
        break;
      case TokenType.Gt:
        if (this.peakNextChar() === TokenType.Eq) {
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
        if (this.peakNextChar() === TokenType.HASH) {
          curChar = this.readChar();
          if (this.peakNextChar() === TokenType.HASH) {
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
      case TokenType.DoublePoint:
        token = createToken(TokenType.DoublePoint, TokenType.DoublePoint);
        break;
      case TokenType.Underscore:
        token = createToken(TokenType.Underscore, TokenType.Underscore);
        break;
      case TokenType.Comma:
        token = createToken(TokenType.Comma, TokenType.Comma);
        break;
      case TokenType.Dot:
        if (this.peakNextChar() === TokenType.Dot) {
          curChar = this.readChar();
          if (this.peakNextChar() === TokenType.Dot) {
            curChar = this.readChar();
            token = createToken(TokenType.Spread, TokenType.Spread);
            break;
          }
          token = createToken(TokenType.Illegal, TokenType.Illegal);
        }
        token = createToken(TokenType.Dot, TokenType.Dot);
        break;

      default:
        token = createToken(TokenType.Illegal, curChar);
        break;
    }

    return token as Token;
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

  peakNextChar = () => {
    if (this.nextPos >= this.input.length) {
      return '\0';
    }

    return this.input[this.nextPos];
  };
}
