package lexer

import (
	"testing"

	"github.com/Ericrulec/haskell-interpreter/token"
)

func TestNextToken(t *testing.T) {
	input := `f x y = (+) x y
g a b = a + b
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
        {token.EOEXP, "EOEXP"},
        {token.IDENT, "g"},
        {token.IDENT, "a"},
        {token.IDENT, "b"},
		{token.ASSIGN, "="},
        {token.IDENT, "a"},
		{token.PLUS, "+"},
        {token.IDENT, "b"},
        {token.EOEXP, "EOEXP"},
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
