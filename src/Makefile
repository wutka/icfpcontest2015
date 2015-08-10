
all: textgame gfxgame showboard

textgame: textmain.hs Game.hs ICFPRandom.hs
#	ghc -prof -auto-all -caf-all -fforce-recomp -o textgame textmain.hs Game.hs ICFPRandom.hs
	ghc -O2 -o textgame textmain.hs Game.hs ICFPRandom.hs

gfxgame: gfxmain.hs Game.hs ICFPRandom.hs
	ghc -o gfxgame gfxmain.hs Game.hs ICFPRandom.hs

showboard: showboard.hs Game.hs ICFPRandom.hs
	ghc -o showboard showboard.hs Game.hs ICFPRandom.hs

init:
	cabal install -p json
	cabal install -p gloss
	cabal install -p MissingH
