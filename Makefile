all: rfk.nes

SOURCES = \
	chr/chr.asm \
	nki/test.asm \
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

chr/chr.asm: chr/background.png chr/sprite.png chr/makechr.py
	chr/makechr.py $(filter %.png,$^) > $@

chr/%.png: font/font.ppm chr/colorize.py
	chr/colorize.py $< $@ $(notdir $(@:.png=))

font/font.ppm: font/makeimg
	$< > $@

font/makeimg: font/makeimg.c font/font.h

nki/%.asm: nki/%.nki nki/maketable.py
	nki/maketable.py < $< > $@

.PHONY: clean
clean:
	@rm -f rfk.nes rfk.lst
	@rm -f chr/chr.asm chr/background.png chr/sprite.png
	@rm -f font/font.ppm font/makeimg
	@rm -f nki/test.asm nki/vanilla.asm
