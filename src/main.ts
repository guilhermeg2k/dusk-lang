import { read } from 'fs';

enum Token {
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
  Return = 'return',

  Number = 'Number',
  String = 'String',
  Bool = 'Bool',

  Identifier = 'Identifier',
  NumberLiteral = 'NumberLiteral',
  True = 'True',
  False = 'False',

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
  Asterisk = '*',
  LParen = '(',
  RParen = ')',
  LBrace = '{',
  RBrace = '}',
  LBracket = '[',
  RBracket = ']',
  Hash = '#',
  TripleHash = '###',
  DoublePoint = ':',
  Underscore = '_',
  Comma = ',',
  Arrow = '=>',
  Spread = '...',

  Illegal = 'ILLEGAL',
  EOF = 'EOF',
}

class Lexer {
  private input: string;
  private currPos: number = 0;
  private nextPos: number = 1;
  private tokens = new Map<Token, string>();

  constructor(input: string) {
    this.input = input;
    this.analyzeTokens();
  }

  analyzeTokens = () => {
    let curChar = this.readChar();

    switch (curChar) {
      case '':
        break;

      default:
        break;
    }
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
