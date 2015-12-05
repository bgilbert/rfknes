;
; chr - CHR RAM initialization
;
; Copyright (C) 2015 Benjamin Gilbert
;
; This program is free software; you can redistribute it and/or modify
; it under the terms of the GNU General Public License as published by
; the Free Software Foundation; either version 2 of the License, or
; (at your option) any later version.
;
; This program is distributed in the hope that it will be useful,
; but WITHOUT ANY WARRANTY; without even the implied warranty of
; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
; GNU General Public License for more details.
;
; You should have received a copy of the GNU General Public License along
; with this program; if not, write to the Free Software Foundation, Inc.,
; 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.
;

.include "../font/glyphs.asm"

.section zeropage
glyph_addr	.word ?
glyph_color	.byte ?
glyph_count	.byte ?
.send

.section fixed
; Initialize CHR RAM.
init_chr .proc
	; write sprite special glyphs
	.cp2 #glyphs_sprite, glyph_addr ; sprite glyphs start address
	.cp #num_glyphs_sprite, glyph_count ; glyph count
	bit PPUSTATUS	; clear address latch
	.cp #0, PPUADDR	; high byte
	.cp #0, PPUADDR	; low byte
	jsr write_sprite_glyphs

	; write background special glyphs
	.cp2 #glyphs_background, glyph_addr ; background glyphs start address
	.cp #num_glyphs_background, glyph_count ; glyph count
	bit PPUSTATUS	; clear address latch
	.cp #1, PPUADDR	; high byte
	.cp #0, PPUADDR ; low byte
	jsr write_background_glyphs

	; write background text glyphs
	.cp2 #glyphs_text, glyph_addr ; text glyphs start address
	.cp #num_glyphs_text, glyph_count ; glyph count
	bit PPUSTATUS	; clear address latch
	.cp #2, PPUADDR	; high byte
	.cp #0, PPUADDR ; low byte
	jsr write_background_glyphs

	; write sprite text glyphs
	.cp2 #glyphs_text, glyph_addr ; text glyphs start address
	.cp #num_glyphs_text, glyph_count ; glyph count
	bit PPUSTATUS	; clear address latch
	.cp #8, PPUADDR ; high byte
	.cp #0, PPUADDR	; low byte
	jmp write_sprite_glyphs
	.pend

; Write multiple sprite glyphs to PPUDATA.
; glyph_addr - starting address of the glyph array
; glyph_count - glyphs to write
; Clobbers: A, X, Y, glyph_addr, glyph_count
write_sprite_glyphs .proc
	.cp #1, glyph_color ; color
-	jsr write_sprite_glyph ; write glyph
	jsr next_glyph	; increment glyph
	ldx glyph_color	; get curent color
	inx		; increment
	cpx #4		; limit check
	bne +		; continue unless at limit
	ldx #1		; at limit; cycle
+	stx glyph_color	; store back
	dec glyph_count	; decrement count
	bne -		; continue until done
	rts
	.pend

; Write multiple background glyphs to PPUDATA.
; glyph_addr - starting address of the glyph array
; glyph_count - glyphs to write
; Clobbers: A, Y, glyph_addr, glyph_count
write_background_glyphs .proc
-	jsr write_background_glyph ; write glyph
	jsr next_glyph	; increment glyph
	dec glyph_count	; decrement count
	bne -		; continue until done
	rts
	.pend

; Write a sprite glyph to PPUDATA.
; glyph_addr - starting address of the glyph
; glyph_color - two-bit glyph color
; Clobbers: A, X, Y
write_sprite_glyph .proc
	ldx #1		; color bit to test
outer	txa		; get current bit
	bit glyph_color	; test whether this plane should be copied
	beq fill	; jump if not

	; copy
	ldy #0		; load starting offset
-	lda (glyph_addr),y ; get row pixels
	sta PPUDATA	; write them
	iny		; increment offset
	cpy #8		; compare to max
	bne -		; continue until done
	beq next	; and continue outer loop

	; fill
fill	ldy #8		; counter
	lda #0		; no pixels
-	sta PPUDATA	; write
	dey		; decrement counter
	bne -		; continue until done

next	inx		; next bit
	cpx #3		; see if we're done
	bne outer	; no; continue
	rts
	.pend

; Write a background glyph to PPUDATA.
; glyph_addr - starting address of the glyph
; Clobbers: A, Y
write_background_glyph .proc
	; copy low plane
	ldy #0		; load starting offset
-	lda (glyph_addr),y ; get row pixels
	sta PPUDATA	; write them
	iny		; increment offset
	cpy #8		; compare to max
	bne -		; continue until done

	; fill high plane
	ldy #8		; counter
	lda #$ff	; all pixels
-	sta PPUDATA	; write
	dey		; decrement counter
	bne -		; continue until done

	rts
	.pend

; Increment glyph_addr to point to next glyph.
; Clobbers: A
next_glyph .proc
	lda glyph_addr	; get low byte
	clc		; clear carry
	adc #8		; add one glyph
	sta glyph_addr	; store back
	bcc +		; need to carry?
	inc glyph_addr + 1 ; yes; carry
+	rts
	.pend
.send
