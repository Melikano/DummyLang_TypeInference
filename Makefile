## File generated by the BNF Converter (bnfc 2.9.4.1).

# Makefile for building the parser and test program.

GHC        = ghc
HAPPY      = happy
HAPPY_OPTS = --array --info --ghc --coerce
ALEX       = alex
ALEX_OPTS  = --ghc

# List of goals not corresponding to file names.

.PHONY : all clean distclean

# Default goal.

all : Dummy/Test

# Rules for building the parser.

Dummy/Abs.hs Dummy/Lex.x Dummy/Par.y Dummy/Print.hs Dummy/Test.hs : BNFC/Dummy.cf
	bnfc --haskell -d BNFC/Dummy.cf

%.hs : %.y
	${HAPPY} ${HAPPY_OPTS} $<

%.hs : %.x
	${ALEX} ${ALEX_OPTS} $<

Dummy/Test : Dummy/Abs.hs Dummy/Lex.hs Dummy/Par.hs Dummy/Print.hs Dummy/Test.hs
	${GHC} ${GHC_OPTS} $@

# Rules for cleaning generated files.

clean :
	-rm -f Dummy/*.hi Dummy/*.o Dummy/*.log Dummy/*.aux Dummy/*.dvi

distclean : clean
	-rm -f Dummy/Abs.hs Dummy/Abs.hs.bak Dummy/ComposOp.hs Dummy/ComposOp.hs.bak Dummy/Doc.txt Dummy/Doc.txt.bak Dummy/ErrM.hs Dummy/ErrM.hs.bak Dummy/Layout.hs Dummy/Layout.hs.bak Dummy/Lex.x Dummy/Lex.x.bak Dummy/Par.y Dummy/Par.y.bak Dummy/Print.hs Dummy/Print.hs.bak Dummy/Skel.hs Dummy/Skel.hs.bak Dummy/Test.hs Dummy/Test.hs.bak Dummy/XML.hs Dummy/XML.hs.bak Dummy/AST.agda Dummy/AST.agda.bak Dummy/Parser.agda Dummy/Parser.agda.bak Dummy/IOLib.agda Dummy/IOLib.agda.bak Dummy/Main.agda Dummy/Main.agda.bak Dummy/Dummy.dtd Dummy/Dummy.dtd.bak Dummy/Test Dummy/Lex.hs Dummy/Par.hs Dummy/Par.info Dummy/ParData.hs Makefile
	-rmdir -p Dummy/

# EOF
