
all: textgame gfxgame

textgame: textmain.hs Game.hs ICFPRandom.hs
	ghc -O2 -o textgame textmain.hs Game.hs ICFPRandom.hs

gfxgame: gfxmain.hs Game.hs ICFPRandom.hs
	ghc -o gfxgame gfxmain.hs Game.hs ICFPRandom.hs

init:
	cabal install json
	cabal install gloss
	caval install MissingH
