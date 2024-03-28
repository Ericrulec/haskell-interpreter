package lexer

import (
	"strings"
	"unicode"
	"unicode/utf8"

	"github.com/Ericrulec/haskell-interpreter/token"
)

type Lexer struct {
	input        string
	position     int  // current position in input (points to current char)
	readPosition int  // current reading position in input (after current char)
	ch           rune // current char under examination
}

type Counter struct {
}

func New(input string) *Lexer {
	l := &Lexer{input: input}
	l.readChar()
	return l
}

func (l *Lexer) ContainsSubstr(substr string) bool {
	howFar := len(l.input) - 1 - l.position
	if howFar < 0 {
		return false
	}
	str := l.input[l.position : l.position+howFar]
	i := strings.Index(str, substr)
	if i == -1 {
		return false
	}
	if i+1 <= howFar && str[i+1] == '=' {
		return false
	}
	return true
}

func (l *Lexer) NextToken() (tok token.Token) {
	literal := string(l.ch)

	// Check if newline into space otherwise skip whitespace
	for {
		switch l.ch {
		case '\u2028', '\u2029', '\n', '\r':
			if unicode.IsSpace(rune(l.peekChar())) {
				l.readChar()
				continue
			} else {
				tok.Type = token.EOEXP
				tok.Literal = token.EOEXP.String()
				goto END // Skip over all the cases
			}
		case ' ', '\t', '\f', '\v', '\u00a0', '\ufeff':
			l.readChar()
			continue
		}
		if l.ch >= utf8.RuneSelf {
			if unicode.IsSpace(l.ch) {
				l.readChar()
				continue
			}
		}
		break
	}
	switch l.ch {
	case '=':
		if l.peekChar() == '=' {
			l.readChar()
			tok.Type = token.EQ
			tok.Literal = token.EQ.String()
		} else {
			tok.Type = token.ASSIGN
			tok.Literal = token.ASSIGN.String()
		}
	case '+':
		if l.peekChar() == '+' {
			l.readChar()
			tok.Type = token.CONCAT
			tok.Literal = token.CONCAT.String()
		} else {
			tok.Type = token.PLUS
			tok.Literal = token.PLUS.String()
		}
	case '-':
		tok.Type = token.MINUS
		tok.Literal = token.MINUS.String()
	case '!':
		if l.peekChar() == '=' {
			l.readChar()
			tok.Type = token.NOT_EQ
			tok.Literal = token.NOT_EQ.String()
		} else if l.peekChar() == '!' {
			l.readChar()
			tok.Type = token.BANGBANG
			tok.Literal = token.BANGBANG.String()
		} else {
			tok.Type = token.BANG
			tok.Literal = token.BANG.String()
		}
	case '/':
		tok.Type = token.SLASH
		tok.Literal = token.SLASH.String()
	case '*':
		tok.Type = token.ASTERISK
		tok.Literal = token.ASTERISK.String()
	case '<':
		tok.Type = token.LT
		tok.Literal = token.LT.String()
	case '>':
		tok.Type = token.GT
		tok.Literal = token.GT.String()
	case ';':
		tok.Type = token.SEMICOLON
		tok.Literal = token.SEMICOLON.String()
	case ':':
		if l.peekChar() == ':' {
			l.readChar()
			tok.Type = token.SIGNATURE
			tok.Literal = token.SIGNATURE.String()
		} else {
			tok.Type = token.COLON
			tok.Literal = token.COLON.String()
		}
	case '|':
		if l.peekChar() == '|' {
			l.readChar()
			tok.Type = token.OR
			tok.Literal = token.OR.String()
		} else {
			tok.Type = token.GUARD
			tok.Literal = token.GUARD.String()
		}
	case '(':
		tok.Type = token.LPAREN
		tok.Literal = token.LPAREN.String()
	case ')':
		tok.Type = token.RPAREN
		tok.Literal = token.RPAREN.String()
	case ',':
		tok.Type = token.COMMA
		tok.Literal = token.COMMA.String()
	case 0:
		tok.Type = token.EOF
		tok.Literal = token.EOF.String()
	default:
		if isLetter(l.ch) {
			literal = l.readIdentifier()
			tok.Type = token.LookupIdent(literal)
			tok.Literal = literal
			return tok
		} else if isDigit(l.ch) {
			tok.Type = token.INT
			literal = l.readNumber()
			tok.Literal = literal
			return tok
		} else {
			tok.Type = token.ILLEGAL
			tok.Literal = token.ILLEGAL.String()
		}
	}
END: // END label skipping over char cases
	l.readChar()
	return tok
}

func (l *Lexer) readIdentifier() string {
	position := l.position
	for isLetter(l.ch) {
		l.readChar()
	}
	return l.input[position:l.position]
}

func (l *Lexer) readNumber() string {
	position := l.position
	for isDigit(l.ch) {
		l.readChar()
	}
	return l.input[position:l.position]
}

func (l *Lexer) readChar() {
	if l.readPosition >= len(l.input) {
		l.ch = 0
	} else {
		l.ch = rune(l.input[l.readPosition]) // ugly
	}
	l.position = l.readPosition
	l.readPosition += 1
}

func (l *Lexer) peekChar() byte {
	if l.readPosition >= len(l.input) {
		return 0
	} else {
		return l.input[l.readPosition]
	}
}

func isLetter(ch rune) bool {
	return 'a' <= ch && ch <= 'z' || 'A' <= ch && ch <= 'Z' || ch == '_' || ch >= utf8.RuneSelf && unicode.IsLetter(ch)
}

func isDigit(ch rune) bool {
	return '0' <= ch && ch <= '9'
}
