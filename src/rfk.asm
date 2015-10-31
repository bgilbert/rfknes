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
	sta temp1	; save it

	; load nametable address
	tay		; copy bank to Y
	sta banknums,y	; switch bank, avoiding bus conflicts
	ldy #0		; index of line count in string
	lda (tempA),y	; get number of lines
	tay		; put in Y
	txa		; move arg to A; test high bit
	bmi bottom	; branch if drawing at bottom
	ldx #<nki_offset_top ; load low byte
	stx tempB	; and store
	.cerror >nki_offset_top > 0 ; assume high byte is 0
	jmp +		; done
bottom	ldx nki_off_lo - 1,y ; load low byte
	stx tempB	; and store
	ora nki_off_hi - 1,y ; OR high byte into arg
	and #$7f	; drop high bit
+	sta tempB + 1	; store high byte

	lda temp1	; recover bank number
	jmp do_print	; tail call
	.pend

nki_offset_top = 2 + 3 * 32
nki_offset_bot = 2 + 22 * 32
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
