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
.include "board.asm"
.include "nmi.asm"
.include "../nki/nki.asm"
.include "../chr/chr.asm"

.section zeropage
; Persistent state
nmi_ready	.byte ?
cmd_ptr		.word ?
ppu_addr	.word ?
rand_state	.word ?
nki_ppu_addr	.word ?
nki_lines	.byte ?	; including leading/trailing border
.send

.section bss
.align $100
cmd_buffer	.fill $100
.send

.section fixed
start	.proc
	.cp2 #cmd_buffer, cmd_ptr ; initialize command pointer
	.cp2 #$c292, rand_state ; initialize random state
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

	jsr make_board
	jsr draw_board

main	jsr maybe_next_board
	jsr run_nmi	; wait for NMI
	jmp main	; continue main loop
	.pend

maybe_next_board .proc
	jsr input	; query buttons
	lda new_buttons	; get result
	bne +		; button pressed?
	rts		; no, return

+	jsr end_board	; clear board
	jsr run_nmi	; draw
	jsr make_board	; make a new one
	jmp draw_board	; draw it
	.pend

maybe_next_nki .proc
	jsr input	; query buttons
	lda new_buttons	; get result
	bne +		; button pressed?
	rts		; no, return

+	lda nki_lines	; see if we're already showing an NKI
	beq +		; test
	jsr clear_nki	; yes; clear it
+	jsr rand_nki	; get random NKI
	.cp #$a0, temp1	; draw at bottom
	jmp print_nki	; draw
	.pend

; Tell NMI handler we're ready, then wait for it to complete
; Clobbers: A, Y
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

; Pick a random NKI
; Return:
; tempA - the NKI
; Clobbers: A, X
rand_nki .proc
again	jsr rand	; randomize high byte
	and #>(nki_next_power_of_two - 1) ; mask off high bits
	sta tempA + 1	; store high byte
	cmp #>(nki_count - 1) ; compare to max index
	beq hard	; branch if outcome uncertain
	bpl again	; high byte too large?  try again
	jsr rand	; randomize low byte
	sta tempA	; store low byte
	rts

hard	jsr rand	; randomize low byte
	cmp #<(nki_count - 1) ; compare to max index
	beq +		; equal; we're safe
	bpl again	; greater; try again
+	sta tempA	; store low byte
	rts
	.pend

; Generate "random" number
; 16-bit Galois LFSR; polynomial fc00
; Return: number in A
; Clobbers: A, X
rand	.proc
	lda rand_state + 1 ; load high byte of state
	ldx #8		; initialize round counter

-	lsr		; shift high byte
	ror rand_state	; shift low byte
	bcc +		; see if output bit is 1
	eor #$fc	; yes, so xor poly
+	dex		; decrement round counter
	bne -		; continue if not done

	sta rand_state + 1 ; update high byte
	lda rand_state	; return low byte
	rts
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
; Returns:
; nki_ppu_addr - starting PPU address of NKI
; nki_lines - number of lines in NKI, including border
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

	; calculate PPU address
	ldy #0		; index of line count in string
	lda (tempA),y	; get number of lines
	sta temp2	; put in temp2
	tax		; and X
	clc		; clear carry
	adc #2		; add border lines
	sta nki_lines	; and put in nki_lines
	lda temp1	; move combined arg to A; test high bit
	bmi bottom	; branch if drawing at bottom
	.cerror >nki_offset_top > 0 ; assume high byte is 0
	sta ppu_addr + 1; write high byte
	sta nki_ppu_addr + 1 ; to two places
	lda #<nki_offset_top ; load low byte
	sta ppu_addr	; store to ppu_addr
	sta nki_ppu_addr; and nki_ppu_addr
	jmp +		; done
bottom	ora nki_off_hi - 1,x ; OR high byte into arg
	and #$7f	; drop high bit
	sta ppu_addr + 1; write high byte
	sta nki_ppu_addr + 1 ; twice
	lda nki_off_lo - 1,x ; load low byte
	sta ppu_addr	; write low byte
	sta nki_ppu_addr; twice

	; draw top row of border
+	ldx #32		; one line
	jsr write_copy_header ; write header
	.ccmd #0	; write space
	.ccmd #218	; write top-left corner
	lda #196	; load horizontal line
	ldx #28		; initialize counter
-	.cmd		; write line
	dex		; decrement counter
	bne -		; loop until done
	.ccmd #191	; write top-right corner
	.ccmd #0	; write space

	; prepare to increment string address for line count
	.cp #0, temp1	; increment by one character

	; update string pointer
next	lda temp1	; get line length
	sec		; set carry to account for null byte
	adc tempA	; add to string address
	sta tempA	; write it back
	bcc +		; need to update high byte?
	inc tempA + 1	; yes

	; print line
+	jsr next_line	; write header
	.ccmd #0	; write space
	.ccmd #179	; vertical line
	jsr resync_cmd_ptr ; update cmd_ptr
	ldy #0		; first char of string, first byte of cmd_ptr
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
	.ccmd #0	; write space

	; decrement lines remaining; break if done
	ldx temp2	; get lines remaining
	dex		; decrement
	stx temp2	; store back
	bne next	; loop until zero

	; draw line footer
	jsr next_line	; write header
	.ccmd #0	; write space
	.ccmd #192	; write bottom-left corner
	lda #196	; load horizontal line
	ldx #28		; initialize counter
-	.cmd		; write it
	dex		; decrement counter
	bne -		; loop until done
	.ccmd #217	; write bottom-right corner
	.ccmd #0	; write space

	 ; update cmd_ptr
	jmp resync_cmd_ptr
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

; Write a COPY header
; X - number of characters
; Y [in/out] - offset into cmd_ptr
; ppu_addr - starting PPU address
; Clobbers: A
write_copy_header .proc
	.ccmd #CMD_COPY ; write draw command
	lda ppu_addr + 1 ; nametable base (high byte)
	.cmd		; write it
	lda ppu_addr	; nametable base (low byte)
	.cmd		; write it
	txa		; get count
	.cmd		; write it
	rts
	.pend

; Resync cmd_ptr, wait for VBLANK, increment ppu_addr by 32, and write
; a COPY header for one line
; Y [in/out] - offset into cmd_ptr
; Clobbers: A, X
next_line .proc
	jsr resync_cmd_ptr ; resync cmd_ptr
	jsr run_nmi	; wait for frame
	lda ppu_addr	; nametable base (low byte)
	clc		; clear carry
	adc #32		; add one line
	sta ppu_addr	; write back
	bcc +		; need to carry?
	inc ppu_addr + 1 ; yes
+	ldx #32		; one line
	ldy #0		; reset cmd_buffer offset
	bpl write_copy_header ; write the header
	.pend

; Clear an NKI
; nki_ppu_addr - starting PPU address of NKI
; nki_lines - number of lines in NKI, including border
; Sets nki_lines to zero
; Clobbers: A, Y
clear_nki .proc
	ldy #0		; base of cmd_ptr
	.ccmd #CMD_FILL	; write draw command
	lda nki_ppu_addr + 1 ; nametable base (high byte)
	.cmd		; write it
	lda nki_ppu_addr ; nametable base (low byte)
	.cmd		; write it
	lda nki_lines	; get line count
	asl		; multiply by 32
	asl
	asl
	asl
	asl
	.cmd		; write it
	.ccmd #0	; write fill byte
	sta nki_lines	; zero out nki_lines
	jmp resync_cmd_ptr ; resync
	.pend
.send
