package token

import "strconv"

type Token int

const (
	_ Token = iota

	ILLEGAL
	EOF
	COMMENT

	// Types
	STRING
	BOOLEAN
	NULL
	INT
	IDENT

	// BOOLEAN VALUES
	TRUE
	FALSE

	// Delimiters
	COMMA     // ,
	SEMICOLON // ;
	LPAREN    // (
	RPAREN    // )

	// Operators
	PLUS      // +
	MINUS     // -
	ASTERISK  // *
	SLASH     // /
	REMAINDER // %
	BANG      // !

	EQ     // ==
	NOT_EQ // !=
	LT     // <
	GT     // >

	// Function miscellaneous
	RETURN

	// Basic flow
	IF
	IN
	ELSE

	// Declarations
	LET
	ASSIGN
)

var token2string = [...]string{
	ILLEGAL:   "ILLEGAL",
	EOF:       "EOF",
	COMMENT:   "COMMENT",
	STRING:    "STRING",
	BOOLEAN:   "BOOLEAN",
	NULL:      "NULL",
	INT:       "INT",
	IDENT:     "IDENT",
	ASSIGN:    "=",
	BANG:      "!",
	EQ:        "==",
	NOT_EQ:    "!=",
	LT:        "<",
	GT:        ">",
	COMMA:     ",",
	SEMICOLON: ";",
	LPAREN:    "(",
	RPAREN:    ")",
	PLUS:      "+",
	MINUS:     "-",
	ASTERISK:  "*",
	SLASH:     "/",
	REMAINDER: "%",
	RETURN:    "return",
	IF:        "if",
	IN:        "in",
	LET:       "let",
	TRUE:      "true",
	FALSE:     "false",
}

var keywords = map[string]Token{
	"return": RETURN,
	"let":    LET,
	"true":   TRUE,
	"false":  FALSE,
	"if":     IF,
	"else":   ELSE,
	"in":     IN,
}

func LookupIdent(ident string) Token {
	if tok, ok := keywords[ident]; ok {
		return tok
	}
	return IDENT
}

func (tok Token) String() string {
	switch {
	case tok == 0:
		return "UNKNOWN"
	case tok < Token(len(token2string)):
		return token2string[tok]
	default:
		return "token(" + strconv.Itoa(int(tok)) + ")"
	}
}
