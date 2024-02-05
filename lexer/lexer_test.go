package lexer

import (
	"testing"

	"github.com/Ericrulec/haskell-interpreter/token"
)

func TestNextToken(t *testing.T) {
	input := `f x y = (+) x y 
g a b = if a == b then (*) a b else a
`

	tests := []struct {
		expectedType    token.Token
		expectedLiteral string
	}{
		{token.IDENT, "f"},
		{token.IDENT, "x"},
		{token.IDENT, "y"},
		{token.ASSIGN, "="},
        {token.LPAREN, "("},
        {token.PLUS, "+"},
        {token.RPAREN, ")"},
		{token.IDENT, "x"},
		{token.IDENT, "y"},
		{token.IDENT, "g"},
		{token.IDENT, "a"},
		{token.IDENT, "b"},
		{token.ASSIGN, "="},
        {token.IF, "if"},
		{token.IDENT, "a"},
        {token.EQ, "=="},
        {token.IDENT, "b"},
        {token.THEN, "then"},
        {token.LPAREN, "("},
        {token.ASTERISK,"*"},
        {token.RPAREN, ")"},
		{token.IDENT, "a"},
		{token.IDENT, "b"},
		{token.ELSE, "else"},
		{token.IDENT, "a"},
	}

	l := New(input)

	for i, tt := range tests {
        tok, literal := l.NextToken()

		if tok != tt.expectedType {
			t.Fatalf("tests[%d] - tokentype wrong. expected=%q, got=%q",
				i, tt.expectedType, tok)
		}

		if literal != tt.expectedLiteral {
			t.Fatalf("tests[%d] - literal wrong. expected=%q, got=%s",
				i, tt.expectedLiteral, literal)
		}
	}
}
