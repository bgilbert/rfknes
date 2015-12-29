.PHONY: all
all: robotfindskitten.nes

SOURCES = \
	font/glyphs.asm \
	nki/test.asm \
	nki/vanilla.asm \
	src/board.asm \
	src/chr.asm \
	src/nes.asm \
	src/nki.asm \
	src/nmi.asm \
	src/rfk.asm \
	src/robot.asm \
	src/string.asm

robotfindskitten.nes robotfindskitten.lst: $(SOURCES)
	64tass --flat --quiet -o "$@" -L "$(@:.nes=.lst)" src/rfk.asm

font/glyphs.asm: font/makeglyphs
	$< > $@

font/makeglyphs: font/makeglyphs.c font/font.h

nki/%.asm: nki/%.nki nki/maketable.py
	nki/maketable.py < $< > $@

.PHONY: clean
clean:
	@rm -f robotfindskitten.nes robotfindskitten.lst
	@rm -f font/glyphs.asm font/makeglyphs
	@rm -f nki/test.asm nki/vanilla.asm
