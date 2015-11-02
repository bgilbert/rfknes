;
; rfk - robotfindskitten for NES
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

.include "nes.asm"
.include "string.asm"
.include "../nki/nki.asm"
.include "../chr/chr.asm"

.section zeropage
; Persistent state
nmi_done	.byte ?
.send

.section fixed
start	.proc
	.cp #$80, PPUCTRL ; configure PPU; enable NMI

	; set up arbitrary palette
	bit PPUSTATUS	; clear address latch
	.cp #$3f, PPUADDR
	.cp #$00, PPUADDR
	ldx #$10
-	stx PPUDATA
	inx
	cpx #$30
	bne -

	; print NKI
	.cp2 #690, tempA
	ldx #$a0
	jsr print_nki

	; reset scroll after update
	bit PPUSTATUS	; clear address latch
	lda #0
	sta PPUSCROLL
	sta PPUSCROLL

main
-	lda nmi_done	; wait until after NMI
	beq -		; or loop
	.cp #0, nmi_done ; clear nmi_done
	jmp main	; NMI done; next iteration of main loop
	.pend

nmi	.proc
	pha		; Push A
	txa		; <- X
	pha		; Push X
	tya		; <- Y
	pha		; Push Y

	.cp #$1e, PPUMASK ; enable rendering
	.cp #1, nmi_done ; report NMI complete

	pla		; Pop Y
	tay		; -> Y
	pla		; Pop X
	tax		; -> X
	pla		; Pop A
	rti
	.pend

irq	.proc
	rti
	.pend

; Print an NKI
; X - high byte of nametable address; set high bit to draw at bottom
; tempA - NKI number
print_nki .proc
	; get string address and bank number
	asl tempA	; get table offset by doubling nki_num: low
	rol tempA + 1	; and high
	.cp #<nki_table, tempB ; copy low byte of table base to tempB.L
	lda #>nki_table	; load high byte
	clc		; clear carry
	adc tempA + 1	; add high byte of table offset
	sta tempB + 1	; and store it to tempB.H
	ldy tempA	; get low byte of offset for indirect indexed
	lda (tempB),y	; load low byte of string address
	sta tempA	; store to tempA.L
	iny		; increment offset for entry.H
	lda (tempB),y	; load entry.H
	tay		; copy to Y
	lsr		; shift to recover high bits of string address
	lsr
	ora #$80	; fix the top two bits
	sta tempA + 1	; and store to tempA.H
	tya		; get entry.H back again
	and #$03	; and extract the bank
	tay		; copy to Y
	sta banknums,y	; switch bank, avoiding bus conflicts

	; seek nametable to address
	ldy #0		; index of line count in string
	lda (tempA),y	; get number of lines
	tay		; put in Y
	bit PPUSTATUS	; clear latch
	txa		; move arg to A; test high bit
	bmi bottom	; branch if drawing at bottom
	.cerror >nki_offset_top > 0 ; assume high byte is 0
	sta PPUADDR	; write high byte
	.cp #<nki_offset_top, PPUADDR ; copy low byte
	jmp +		; done
bottom	ora nki_off_hi - 1,y ; OR high byte into arg
	and #$7f	; drop high bit
	sta PPUADDR	; write high byte
	lda nki_off_lo - 1,y ; load low byte
	sta PPUADDR	; write low byte

	; draw top row of border
+	.cp #0, PPUDATA	; write space
	.cp #218, PPUDATA ; write top-left corner
	lda #196	; load horizontal line
	ldx #28		; initialize counter
-	sta PPUDATA	; write line
	dex		; decrement counter
	bne -		; loop until done
	.cp #191, PPUDATA ; write top-right corner
	lda #0		; load space
	sta PPUDATA	; write twice
	sta PPUDATA

	; set up indexes
	tya		; get number of lines
	tax		; put into X
	ldy tempA	; low byte of address
	iny		; increment for line count
	sty tempA	; store
	bne next	; need to increment high byte?
	ldy tempA + 1	; yes; load,
	iny		; increment,
	sty tempA + 1	; store

	; draw line header
next	.cp #179, PPUDATA ; vertical line

	; print line
	ldy #0		; point to first char of string
	jmp +		; start loop
-	sta PPUDATA	; store character
	iny		; increment index
+	lda (tempA),y	; load character
	bne -		; continue until NUL
	dex		; decrement lines remaining
	sty temp1	; save line length

	; fill, draw trailer
	tya		; copy count to accumulator
	eor #$ff	; complement for negation
	clc		; clear carry
	adc #29		; add max characters per line, plus 1 for negation
	beq +		; skip fill if line is full-width
	tay		; move fill count to Y
	lda #0		; load space
-	sta PPUDATA	; write space
	dey		; decrement count
	bne -		; loop until done
+	.cp #179, PPUDATA ; vertical line
	lda #0		; load space
	sta PPUDATA	; write twice
	sta PPUDATA

	; break if done
	cpx #0		; are there any lines remaining?
	beq footer

	; update string pointer
	lda temp1	; get line length
	adc tempA	; add to string address.  carry must be set to
			; account for null byte; already set by cpx #0 above
	sta tempA	; write it back
	bcc next	; need to update high byte?
	ldy tempA + 1	; yes; load,
	iny		; increment,
	sty tempA + 1	; and write back
	jmp next	; loop

	; draw line footer
footer	.cp #192, PPUDATA ; write bottom-left corner
	lda #196	; load horizontal line
	ldy #28		; initialize counter
-	sta PPUDATA	; write it
	dey		; decrement counter
	bne -		; loop until done
	.cp #217, PPUDATA ; write bottom-right corner
	.cp #0, PPUDATA	; write space

	rts
	.pend

nki_offset_top = 2 * 32
nki_offset_bot = 21 * 32
nki_off_lo
	.byte <(nki_offset_bot + 32 * 4)
	.byte <(nki_offset_bot + 32 * 3)
	.byte <(nki_offset_bot + 32 * 2)
	.byte <(nki_offset_bot + 32 * 1)
	.byte <(nki_offset_bot + 32 * 0)
nki_off_hi
	.byte >(nki_offset_bot + 32 * 4)
	.byte >(nki_offset_bot + 32 * 3)
	.byte >(nki_offset_bot + 32 * 2)
	.byte >(nki_offset_bot + 32 * 1)
	.byte >(nki_offset_bot + 32 * 0)

.send
