all: rfk.nes

SOURCES = \
	chr/chr.asm \
	nki/vanilla.asm \
	src/board.asm \
	src/nes.asm \
	src/nki.asm \
	src/nmi.asm \
	src/rfk.asm \
	src/robot.asm \
	src/string.asm

rfk.nes rfk.lst: $(SOURCES)
	64tass --flat --quiet -o "$@" -L "$(@:.nes=.lst)" src/rfk.asm

chr/chr.asm: chr/chr.png chr/makechr.py
	chr/makechr.py $< > $@

chr/chr.png: font/font.ppm chr/colorize.py
	chr/colorize.py $< $@

font/font.ppm: font/makeimg
	$< > $@

font/makeimg: font/makeimg.c font/font.h

nki/vanilla.asm: nki/vanilla.nki nki/maketable.py
	nki/maketable.py < $< > $@

.PHONY: clean
clean:
	@rm -f rfk.nes rfk.lst
	@rm -f chr/chr.asm chr/chr.png
	@rm -f font/font.ppm font/makeimg
	@rm -f nki/vanilla.asm
