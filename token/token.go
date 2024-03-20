package token

import "strconv"

type Token struct {
	Type    Tokentype
	Literal string
}

type Tokentype int

const (
	_ Tokentype = iota

	ILLEGAL
	EOF
	COMMENT
	EOEXP // End of Expression

	// Types
	STRING
	BOOLEAN
	NULL
	INT
	IDENT

	// BOOLEAN VALUES
	TRUE
	FALSE
	OTHERWISE

	// Delimiters
	COMMA     // ,
	COLON     // :
	SIGNATURE // ::
	SEMICOLON // ;
	LPAREN    // (
	RPAREN    // )

	// Operators
	PLUS        // +
	MINUS       // -
	ASTERISK    // *
	SLASH       // /
	REMAINDER   // %
	BANG        // !
	BANGBANG    // !!
	CONCAT      // ++
	APPLY       // $
	COMPOSITION // .
	AND         // &&
	OR          // ||

	// Comparison
	EQ     // ==
	NOT_EQ // !=
	LT     // <
	GT     // >

	// Function miscellaneous
	RETURN
	FUNCTION

	// Basic flow
	IF
	IN
	ELSE
	GUARD // |
	THEN

	// Declarations
	LET
	ASSIGN // =
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
	EOEXP:     "EOEXP",
	ASSIGN:    "=",
	BANG:      "!",
	BANGBANG:  "!!",
	EQ:        "==",
	NOT_EQ:    "!=",
	LT:        "<",
	GT:        ">",
	COMMA:     ",",
	COLON:     ":",
	SIGNATURE: "::",
	SEMICOLON: ";",
	LPAREN:    "(",
	RPAREN:    ")",
	PLUS:      "+",
	CONCAT:    "++",
	MINUS:     "-",
	ASTERISK:  "*",
	SLASH:     "/",
	REMAINDER: "%",
	AND:       "&&",
	OR:        "||",
	GUARD:     "|",
	THEN:      "then",
	RETURN:    "return",
	IF:        "if",
	IN:        "in",
	LET:       "let",
	TRUE:      "true",
	FALSE:     "false",
	OTHERWISE: "true",
}

var keywords = map[string]Tokentype{
	"return":    RETURN,
	"let":       LET,
	"true":      TRUE,
	"false":     FALSE,
	"if":        IF,
	"else":      ELSE,
	"in":        IN,
	"then":      THEN,
	"otherwise": OTHERWISE,
}

func LookupIdent(ident string) Tokentype {
	if tok, ok := keywords[ident]; ok {
		return tok
	}
	return IDENT
}

func (tok Tokentype) String() string {
	switch {
	case tok == 0:
		return "UNKNOWN"
	case tok < Tokentype(len(token2string)):
		return token2string[tok]
	default:
		return "token(" + strconv.Itoa(int(tok)) + ")"
	}
}
