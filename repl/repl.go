package repl

import (
	"bufio"
	"fmt"
	"github.com/Ericrulec/haskell-interpreter/lexer"
	"github.com/Ericrulec/haskell-interpreter/token"
	"io"
)

const PROMPT = ">> "

func Start(in io.Reader, out io.Writer) {
	scanner := bufio.NewScanner(in)

	for {
		fmt.Printf(PROMPT)
		scanned := scanner.Scan()
		if !scanned {
			return
		}

		line := scanner.Text()
		l := lexer.New(line)

		for tok, literal := l.NextToken(); tok != token.EOF; tok, literal = l.NextToken() {
			fmt.Printf("%s ==> %+v\n", literal, tok)
		}
	}
}
