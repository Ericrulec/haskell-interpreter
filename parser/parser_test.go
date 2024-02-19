package parser

import (
	"testing"

	"github.com/Ericrulec/haskell-interpreter/ast"
	"github.com/Ericrulec/haskell-interpreter/lexer"
)

func TestLetStatements(t *testing.T) {
	tests := []struct {
		input              string
		expectedIdentifier string
		expectedValue      interface{}
	}{
		{"let a = 5", "a", 5},
		{"let b = 7", "b", 7},
	}

	for _, tt := range tests {
		l := lexer.New(tt.input)
		p := New(l)
		program := p.ParseProgram()
        checkParserErrors(t,p)

		if len(program.Statements) != 1 {
			t.Fatalf("program.Statements does not contain 1 statement. got=%d",
				len(program.Statements))
		}

		stmt := program.Statements[0]
		if !testLetStatement(t, stmt, tt.expectedIdentifier) {
			return
		}
	}
}

func testLetStatement(t *testing.T, s ast.Statement, name string) bool {
	if s.TokenLiteral() != "let" {
		t.Errorf("s.TokenLiteral not 'let'. got=%q", s.TokenLiteral())
		return false
	}

	letStmt, ok := s.(*ast.LetStatement)
	if !ok {
		t.Errorf("s not *ast.LetStatement. got=%T", s)
		return false
	}

	if letStmt.Name.Value != name {
		t.Errorf("letStmt.Name.Value not '%s'. got=%s", name, letStmt.Name.Value)
		return false
	}

	if letStmt.Name.TokenLiteral() != name {
		t.Errorf("s.Name not '%s'. got=%s", name, letStmt.Name)
		return false
	}

	return true
}

func checkParserErrors(t *testing.T,p *Parser) {
    errors := p.Errors()

    if len(errors) == 0{
        return
    }

    t.Errorf("parser has %d errors",len(errors))
    for _,msg:=range errors {
        t.Errorf("parser error: %q",msg)
    }
    t.FailNow()
}

func TestReturnStatements(t *testing.T) {
	tests := []struct {
		input              string
		expectedIdentifier string
		expectedValue      interface{}
	}{
		{"a = 1", "a", 1},
		{"b = 1 + 1", "b", 2},
	}
	for _, tt := range tests {
		l := lexer.New(tt.input)
		p := New(l)
		program := p.ParseProgram()
        checkParserErrors(t,p)
        t.Log(program)

		stmt := program.Statements[0]
		if !testReturnStatement(t, stmt, tt.expectedIdentifier) {
			return
		}
        t.Errorf("Something went wrong with stmt %T",stmt)
	}
}

func testReturnStatement(t *testing.T, s ast.Statement, name string) bool {
    returnStmt, ok := s.(*ast.ReturnStatement)
    if !ok {
        t.Errorf("stmt not *ast.ReturnStatement. got=%T",s)
        return false
    }

    if returnStmt.TokenLiteral() != name {
        t.Errorf("returnStmt.TokenLiteral not '%s', got %q",name,returnStmt.TokenLiteral())
        return false
    }

    return true
}
