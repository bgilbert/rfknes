;
; nki - Non-kitten items
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

.section zeropage
nki_y		.byte ? ; Y coordinate of leading border
nki_lines	.byte ?	; including leading/trailing border
cur_nki		.word ?
.send

.section fixed
; Pick a random NKI
; Return:
; cur_nki - the NKI
; Clobbers: A, X
rand_nki .proc
again	jsr rand	; randomize high byte
	and #>(nki_next_power_of_two - 1) ; mask off high bits
	sta cur_nki + 1	; store high byte
	cmp #>(nki_count - 1) ; compare to max index
	beq hard	; branch if outcome uncertain
	bpl again	; high byte too large?  try again
	jsr rand	; randomize low byte
	sta cur_nki	; store low byte
	rts

hard	jsr rand	; randomize low byte
	cmp #<(nki_count - 1) ; compare to max index
	beq +		; equal; we're safe
	bpl again	; greater; try again
+	sta cur_nki	; store low byte
	rts
	.pend

; Print an NKI
; nametable - high byte of nametable address
; cur_nki - NKI number
; temp1 - $00 to draw at top; $80 to draw at bottom
; Returns:
; nki_y - Y coordinate of leading border
; nki_lines - number of lines in NKI, including border
print_nki .proc
	nki_first_row = 2
	nki_last_row = 26

	; get string address and bank number
	lda cur_nki	; get NKI number low byte
	asl		; double for table offset
	tay		; put in Y
	lda cur_nki + 1	; get NKI number high byte
	rol		; double for table offset
	adc #>nki_table	; add high byte of table base (assumes carry clear)
	sta tempA + 1	; and store it to tempA.H
	.cp #<nki_table, tempA ; copy low byte of table base to tempA.L
	lda (tempA),y	; load low byte of string address
	tax		; put in X
	iny		; increment offset for entry.H
	lda (tempA),y	; load entry.H
	tay		; copy to Y
	lsr		; shift to recover high bits of string address
	lsr
	ora #$80	; fix the top two bits
	sta tempA + 1	; and store to tempA.H
	stx tempA	; store entry.L to tempA.L
	tya		; get entry.H back again
	and #$03	; and extract the bank
	tay		; copy to Y
	sta banknums,y	; switch bank, avoiding bus conflicts

	; calculate PPU address and write header
	ldy #0		; index of line count in string
	lda (tempA),y	; get number of lines
	sta temp2	; put in temp2
	.ccmd #CMD_COPY ; write draw command
	bit temp1	; see whether we should draw at bottom
	bmi bottom	; branch if so
	lda #nki_first_row ; Y coord for top
	bne +		; done
bottom	lda #nki_last_row ; Y coord for bottom
	sec		; set carry
	sbc temp2	; subtract number of rows we need
+	sta cur_y	; store Y coordinate
	sta nki_y	; and copy to nki_y
	.cp #0, cur_x	; store X coordinate
	jsr write_nametable_addr ; write start address
	lda temp2	; get line count
	clc		; clear carry
	adc #2		; add border lines
	sta nki_lines	; and put in nki_lines
	asl		; multiply by 32
	asl
	asl
	asl
	asl
	.cmd		; write to cmd

	; draw top row of border
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
+	.ccmd #0	; write space
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
.send
