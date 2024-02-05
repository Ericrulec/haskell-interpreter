package main

import (
	"fmt"
	"github.com/Ericrulec/haskell-interpreter/repl"
	"os"
)

func main() {
	fmt.Printf("Hello and welcome to the Haskell Interpreter written in Go!\n")
	repl.Start(os.Stdin, os.Stdout)
}

