all:
	happy -gca ParNumbers.y
	alex -g LexNumbers.x
	ghc --make TestNumbers.hs -o TestNumbers

clean:
	-rm -f *.log *.aux *.hi *.o *.dvi

distclean: clean
	-rm -f DocNumbers.* LexNumbers.* ParNumbers.* LayoutNumbers.* SkelNumbers.* PrintNumbers.* TestNumbers.* AbsNumbers.* TestNumbers ErrM.* SharedString.* ComposOp.* numbers.dtd XMLNumbers.* Makefile*
	

