.PHONY: all
all: robotfindskitten.nes

.PHONY: nrom
nrom: robotfindskitten-nrom.prg robotfindskitten-nrom.chr

SOURCES = \
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

BUILD = 64tass --flat --quiet -o "$@" -L "$(@:.nes=.lst)" src/rfk.asm

robotfindskitten.nes robotfindskitten.lst: $(SOURCES) font/glyphs.asm
	$(BUILD) -D MAPPER=1

robotfindskitten-nrom.nes robotfindskitten-nrom.lst: $(SOURCES) font/chr.asm
	$(BUILD) -D MAPPER=0

robotfindskitten-nrom.prg: robotfindskitten-nrom.nes
	dd if=$< of=$@ bs=1 skip=16 count=32768 2>/dev/null

robotfindskitten-nrom.chr: robotfindskitten-nrom.nes
	dd if=$< of=$@ bs=1 skip=32784 count=8192 2>/dev/null

font/chr.asm: font/makeglyphs
	$< chr > $@

font/glyphs.asm: font/makeglyphs
	$< glyphs > $@

font/makeglyphs: font/makeglyphs.c font/font.h

nki/%.asm: nki/%.nki nki/maketable.py
	nki/maketable.py < $< > $@

.PHONY: clean
clean:
	@rm -f robotfindskitten.nes robotfindskitten.lst
	@rm -f robotfindskitten-nrom.nes robotfindskitten-nrom.lst
	@rm -f robotfindskitten-nrom.prg robotfindskitten-nrom.chr
	@rm -f font/chr.asm font/glyphs.asm font/makeglyphs
	@rm -f nki/test.asm nki/vanilla.asm
