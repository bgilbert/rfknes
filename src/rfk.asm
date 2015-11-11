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
.include "nmi.asm"
.include "../nki/nki.asm"
.include "../chr/chr.asm"

.section zeropage
; Persistent state
nmi_ready	.byte ?
cmd_ptr		.word ?
ppu_addr	.word ?
.send

.section bss
.cerror * % 256 != 0, "Buffer not page-aligned"
cmd_buffer	.fill $100
.send

.section fixed
start	.proc
	.cp2 #cmd_buffer, cmd_ptr ; initialize command pointer
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

	; enable render
	ldy #0		; buffer index
	.ccmd #CMD_ENABLE_RENDER ; enable render
	jsr resync_cmd_ptr ; resync

	; print NKI
	.cp #$a0, temp1
	.cp2 #690, tempA
	jsr print_nki

main	jsr run_nmi	; wait for NMI
	jmp main	; continue main loop
	.pend

; Tell NMI handler we're ready, then wait for it to complete
run_nmi .proc
	; terminate buffer
	ldy #0		; buffer index
	.ccmd #CMD_EOF	; write command

	; enable NMI and wait for it
	.cp #1, nmi_ready ; tell NMI handler to proceed
-	lda nmi_ready	; wait until after NMI
	bne -		; or loop
	.cp2 #cmd_buffer, cmd_ptr ; reset buffer pointer
	rts
	.pend

irq	.proc
	rti
	.pend

; Store A to command buffer indexed by Y; increment Y
cmd	.macro
	sta (cmd_ptr),y
	iny
	.endm

; Copy value to command buffer indexed by Y; increment Y
; args: value
ccmd	.macro
	lda \1
	.cmd
	.endm

; Add Y to cmd_ptr
; clobbers: A
resync_cmd_ptr .proc
	tya		; get offset into cmd_ptr
	clc		; clear carry
	adc cmd_ptr	; add to pointer
	sta cmd_ptr	; and write back
	rts
	.pend

; Print an NKI
; temp1 - high byte of nametable address; set high bit to draw at bottom
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

	; store command
	ldy #0		; index of line count in string
	lda (tempA),y	; get number of lines
	sta temp2	; put in temp2
	tax		; and X
	lda temp1	; move combined arg to A; test high bit
	bmi bottom	; branch if drawing at bottom
	.cerror >nki_offset_top > 0 ; assume high byte is 0
	sta ppu_addr + 1; write high byte
	.cp #<nki_offset_top, ppu_addr ; copy low byte
	jmp +		; done
bottom	ora nki_off_hi - 1,x ; OR high byte into arg
	and #$7f	; drop high bit
	sta ppu_addr + 1; write high byte
	lda nki_off_lo - 1,x ; load low byte
	sta ppu_addr	; write low byte
+	txa		; retrieve number of lines
	clc		; clear carry
	adc #2		; add top and bottom borders
	asl		; multiply by 32
	asl
	asl
	asl
	asl
	tax		; put in X
	jsr write_draw_buf_header ; write header

	; draw top row of border and header of first line
	.ccmd #0	; write space
	.ccmd #218	; write top-left corner
	lda #196	; load horizontal line
	ldx #28		; initialize counter
-	.cmd		; write line
	dex		; decrement counter
	bne -		; loop until done
	.ccmd #191	; write top-right corner
	lda #0		; load space
	.cmd		; write twice
	.cmd
	.ccmd #179	; vertical line

	; set up addresses
	jsr resync_cmd_ptr ; update cmd_ptr
	ldy tempA	; low byte of string address
	iny		; increment for line count
	sty tempA	; store
	bne next	; need to increment high byte?
	ldy tempA + 1	; yes; load,
	iny		; increment,
	sty tempA + 1	; store

	; print line
next	ldy #0		; first char of string, first byte of cmd_ptr
	bpl +		; start loop
-	.cmd		; store character; increment index
+	lda (tempA),y	; load character
	bne -		; continue until NUL
	sty temp1	; save line length

	; fill, draw trailer
	tya		; copy count to accumulator
	eor #$ff	; complement for negation
	clc		; clear carry
	adc #29		; add max characters per line, plus 1 for negation
	beq +		; skip fill if line is full-width
	tax		; move fill count to X
	lda #0		; load space
-	.cmd		; write space
	dex		; decrement count
	bne -		; loop until done
+	.ccmd #179	; vertical line
	lda #0		; load space
	.cmd		; write twice
	.cmd

	; decrement lines remaining; break if done
	ldx temp2	; get lines remaining
	dex		; decrement
	beq footer	; break if zero
	stx temp2	; store back

	; draw prefix for next line
	.ccmd #179	; vertical line

	; update cmd_ptr
	jsr resync_cmd_ptr

	; update string pointer
	lda temp1	; get line length
	sec		; set carry to account for null byte
	adc tempA	; add to string address
	sta tempA	; write it back
	bcc next	; need to update high byte?
	ldy tempA + 1	; yes; load,
	iny		; increment,
	sty tempA + 1	; and write back
	jmp next	; loop

	; draw line footer
footer	.ccmd #192	; write bottom-left corner
	lda #196	; load horizontal line
	ldx #28		; initialize counter
-	.cmd		; write it
	dex		; decrement counter
	bne -		; loop until done
	.ccmd #217	; write bottom-right corner
	.ccmd #0	; write space

	 ; update cmd_ptr
	jsr resync_cmd_ptr

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

; X - number of characters
; Y [in/out] - offset into cmd_ptr
; ppu_addr - starting PPU address
; Clobbers: A
write_draw_buf_header .proc
	.ccmd #CMD_DRAW_BUF ; write draw command
	lda ppu_addr + 1 ; nametable base (high byte)
	.cmd		; write it
	lda ppu_addr	; nametable base (low byte)
	.cmd		; write it
	txa		; get count
	.cmd		; write it
	rts
	.pend

.send
