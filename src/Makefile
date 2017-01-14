all: minilambda

minilambda: AST.hs  Interpreter.hs  Parser.hs  Scanner.hs  Semantics.hs  Token.hs
	ghc -o minilambda Interpreter.hs

clean:
	rm -f *.hi *.o
