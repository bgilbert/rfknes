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
.include "robot.asm"
.include "nki.asm"
.include "nmi.asm"
.include "../nki/nki.asm"
.include "../chr/chr.asm"

.section zeropage
; Persistent state
nmi_ready	.byte ?
cmd_ptr		.word ?
rand_state	.word ?
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
	jsr place_robot

main	jsr maybe_move_robot
	jsr run_nmi	; wait for NMI
	jmp main	; continue main loop
	.pend

direction_mask .byte ((1 << BTN_UP) | (1 << BTN_DOWN) | (1 << BTN_LEFT) | (1 << BTN_RIGHT))

maybe_move_robot .proc
	; query buttons
	jsr input	; read input
	lda new_buttons	; get result
	bne +		; button pressed?
	rts		; no, return

	; early return if directional button not pressed
	bit direction_mask ; check buttons
	bne +		; OK if at least one pressed
	rts		; else bail

	; load coords
+	ldx robot_x	; X coord
	ldy robot_y	; Y coord

	; check left
	.cbit BTN_LEFT	; check bit
	beq +		; or continue
	dex		; decrement

	; check right
+	.cbit BTN_RIGHT	; check bit
	beq +		; or continue
	inx		; increment

	; check up
+	.cbit BTN_UP	; check bit
	beq +		; or continue
	dey		; decrement

	; check down
+	.cbit BTN_DOWN	; check bit
	beq +		; or continue
	iny		; increment

	; apply bounds
+	cpx #BOARD_X_OFFSET ; X coord < min?
	bpl +		; no
	ldx robot_x	; or reset
+	cpx #(BOARD_X_THRESHOLD + BOARD_X_OFFSET) ; X coord > max?
	bmi +		; no
	ldx robot_x	; or reset
+	cpy #BOARD_Y_OFFSET ; Y coord < min?
	bpl +		; no
	ldy robot_y	; or reset
+	cpy #(BOARD_Y_THRESHOLD + BOARD_Y_OFFSET) ; Y coord > max?
	bmi +		; no
	ldy robot_y	; or reset

	; clear NKI if showing
+	lda nki_lines	; see if an NKI is showing
	beq +		; no; continue
	stx temp1	; save X coord
	sty temp2	; save Y coord
	jsr clear_nki	; clear NKI
	jsr run_nmi	; draw
	jsr draw_board	; redraw board
	jsr run_nmi	; draw
	ldx temp1	; restore X coord
	ldy temp2	; retore Y coord

	; check for new NKI
+	stx cur_x	; X coord argument
	sty cur_y	; Y coord argument
	jsr get_bit_position ; get bitmap index
	lda nki_bitmap,x ; load bitmap byte
	bit bit_mask	; test bit
	beq +		; continue if clear
	jmp show_nki	; draw the NKI and return

	; update state
+	ldx cur_x	; get new X coord
	ldy cur_y	; get new Y coord
	lda robot_x	; get old X coord
	sta cur_x	; store argument
	lda robot_y	; get new Y coord
	sta cur_y	; store argument
	stx robot_x	; write new X coord
	sty robot_y	; write new Y coord

	; clear old robot
	ldx #0		; load empty glyph
	jsr draw_robot	; clear robot

	; show new robot
	lda robot_x	; get X coord back
	sta cur_x	; save argument
	lda robot_y	; get Y coord back
	sta cur_y	; save argument
	ldx #ROBOT	; robot glyph
	jmp draw_robot	; draw the robot
	.pend

maybe_next_board .proc
	jsr input	; query buttons
	lda new_buttons	; get result
	bne +		; button pressed?
	rts		; no, return

+	jsr end_board	; clear board
	lda robot_x	; load X coord
	sta cur_x	; store argument
	lda robot_y	; load Y coord
	sta cur_y	; store argument
	ldx #0		; load empty glyph
	jsr draw_robot	; clear robot
	jsr make_board	; make a new board
	jsr place_robot	; place the robot
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
.send
