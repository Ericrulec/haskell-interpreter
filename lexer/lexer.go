package lexer

import (
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

func New(input string) *Lexer {
	l := &Lexer{input: input}
	l.readChar()
	return l
}

func (l *Lexer) NextToken() (tok token.Token, literal string) {
	literal = string(l.ch)

	l.skipWhiteSpace()

	switch l.ch {
	case '=':
		if l.peekChar() == '=' {
			l.readChar()
			tok = token.EQ
			literal = token.EQ.String()
		} else {
			tok = token.ASSIGN
			literal = token.ASSIGN.String()
		}
	case '+':
		tok = token.PLUS
	case '-':
		tok = token.MINUS
	case '!':
		if l.peekChar() == '=' {
			l.readChar()
			tok = token.NOT_EQ
		} else {
			tok = token.BANG
		}
	case '/':
		tok = token.SLASH
	case '*':
		tok = token.ASTERISK
	case '<':
		tok = token.LT
	case '>':
		tok = token.GT
	case ';':
		tok = token.SEMICOLON
	case '(':
		tok = token.LPAREN
	case ')':
		tok = token.RPAREN
	case ',':
		tok = token.COMMA
	case 0:
		tok = token.EOF
	default:
		if isLetter(l.ch) {
			literal = l.readIdentifier()
			tok = token.LookupIdent(literal)
			return tok, literal
		} else if isDigit(l.ch) {
			tok = token.INT
			literal = l.readNumber()
			return tok, literal
		} else {
			tok = token.ILLEGAL
		}
	}
	l.readChar()
	return tok, literal
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

func (l *Lexer) skipWhiteSpace() {
	for {
		switch l.ch {
		case ' ', '\t', '\f', '\v', '\u00a0', '\ufeff':
			l.readChar()
			continue
		case '\r':
			if l.peekChar() == '\n' {
				l.readChar()
			}
			fallthrough
		case '\u2028', '\u2029', '\n':
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
}
