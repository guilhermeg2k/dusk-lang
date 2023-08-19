package Lexer

type TokenType = string

const (
	Let    TokenType = "let"
	Mut    TokenType = "mut"
	If     TokenType = "if"
	Else   TokenType = "else"
	Match  TokenType = "match"
	For    TokenType = "for"
	Type   TokenType = "type"
	Trait  TokenType = "trait"
	Where  TokenType = "where"
	Use    TokenType = "use"
	Mod    TokenType = "mod"
	Return TokenType = "return"

	Number TokenType = "Number"
	String TokenType = "String"
	Bool   TokenType = "Bool"

	Identifier    TokenType = "Identifier"
	NumberLiteral TokenType = "NumberLiteral"
	True          TokenType = "true"
	False         TokenType = "false"

	Assign       TokenType = "="
	Or           TokenType = "or"
	And          TokenType = "and"
	Eq           TokenType = "=="
	Not_Eq       TokenType = "!="
	Lt           TokenType = "<"
	Gt           TokenType = ">"
	Lte          TokenType = "<="
	Gte          TokenType = ">="
	Plus         TokenType = "+"
	Minus        TokenType = "-"
	Bang         TokenType = "!"
	ForwardSlash TokenType = "/"
	Hash         TokenType = "#"
	TripleHash   TokenType = "###"
	Asterisk     TokenType = "*"
	LParen       TokenType = "("
	RParen       TokenType = ")"
	LBrace       TokenType = "{"
	RBrace       TokenType = "}"
	LBracket     TokenType = "["
	RBracket     TokenType = "]"
	DoublePoint  TokenType = ":"
	Underscore   TokenType = "_"
	Comma        TokenType = " "
	Arrow        TokenType = "=>"
	Dot          TokenType = "."
	Spread       TokenType = "..."

	NewLine TokenType = "\n"
	Indent  TokenType = "INDENT"
	Dedent  TokenType = "DEDENT"

	Illegal TokenType = "ILLEGAL"
	EOF     TokenType = "\x00"
)

type Token struct {
	name    TokenType
	literal string
}

type Tokenizer struct {
	input string
	char  rune
	pos   uint
}

func Create(input string) Tokenizer {
	return Tokenizer{input: input, pos: 0}
}

func (t *Tokenizer) getNextToken() Token {
	t.readChar()
	switch t.char {
		case '='
	}
}

func (t *Tokenizer) readChar() {
	nextPos := t.pos + 1

	if nextPos > uint(len(t.input)) {
		t.char = '\x00'
		return
	}

	t.char = rune(t.input[t.pos])
	t.pos++
}

func (t *Tokenizer) skipWhiteSpaces() {
	for t.char == ' ' || t.char == '\r' || t.char == '\t' {
		t.readChar()
	}
}
