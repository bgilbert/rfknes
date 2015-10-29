all: rfk.nes

rfk.nes rfk.lst: src/rfk.asm src/nes.asm src/string.asm chr/chr.asm
	64tass --flat --quiet -o "$@" -L "$(@:.nes=.lst)" "$<"

chr/chr.asm: chr/chr.png chr/makechr.py
	chr/makechr.py $< > $@

chr/chr.png: font/font.ppm chr/colorize.py
	chr/colorize.py $< $@

font/font.ppm: font/makeimg
	$< > $@

font/makeimg: font/makeimg.c font/font.h

.PHONY: clean
clean:
	@rm -f rfk.nes rfk.lst
	@rm -f chr/chr.asm chr/chr.png
	@rm -f font/font.ppm font/makeimg
