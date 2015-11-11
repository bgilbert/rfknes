;
; nmi - NMI (vertical retrace) handler for robotfindskitten
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

; NMI commands
CMD_EOF			= 0	; no args
CMD_ENABLE_RENDER	= 1	; no args
CMD_DRAW_BUF		= 2	; nametable base (2, high byte first),
				; count (1), data

.section fixed
nmi_table
	.word nmi_enable_render - 1
	.word nmi_draw_buf - 1

nmi	.proc
	pha		; push A
	txa		; <- X
	pha		; push X
	tya		; <- Y
	pha		; push Y

	; see if we should run
	lda nmi_ready	; get ready flag
	beq done	; test
	.cp #0, nmi_ready ; clear ready flag

	; walk command buffer
	.cp2 #cmd_buffer, cmd_ptr ; initialize command ptr
	bne +		; start loop
-	jsr cmd_dispatch; dispatch
+	ldy #0		; load offset into buffer
	lda (cmd_ptr),y	; get command
	bne -		; continue until command 0

	; reset scroll after update
	bit PPUSTATUS	; clear address latch
	lda #0
	sta PPUSCROLL
	sta PPUSCROLL

	; return
done	pla		; pop Y
	tay		; -> Y
	pla		; pop X
	tax		; -> X
	pla		; pop A
	rti
	.pend

; dispatch a command
; A - command byte
cmd_dispatch .proc
	asl		; double command for table offset
	tax		; copy to X
	lda nmi_table - 1,x ; get high byte of subroutine addr
	pha		; push it
	lda nmi_table - 2,x ; get low byte
	pha		; push it
	rts		; call
	.pend

nmi_enable_render .proc
	.cp #$1e, PPUMASK ; enable rendering
	ldy #1		; size of command
	jsr resync_cmd_ptr
	rts
	.pend

nmi_draw_buf .proc
	; set nametable address
	bit PPUSTATUS	; clear address latch
	ldy #1		; cmd ptr offset
	lda (cmd_ptr),y	; get nametable.H
	sta PPUADDR	; write it
	iny		; increment offset
	lda (cmd_ptr),y	; get nametable.L
	sta PPUADDR	; write it
	iny		; increment offset

	; get counter
	lda (cmd_ptr),y	; get counter
	tax		; put in X
	iny		; increment offset

	; write data until multiple of 8 bytes remaining
	bpl +		; start loop
-	lda (cmd_ptr),y	; load byte
	sta PPUDATA	; write it
	dex		; decrement remaining count
	iny		; increment offset
	txa		; copy counter to A
+	and #$07	; continue until a multiple of 8 bytes
	bne -		; repeat until done

	; write data 8 bytes at a time
	txa		; get remaining count
	beq done	; skip if already done
	lsr		; divide by 8
	lsr
	lsr
	tax		; and put it back
-	lda (cmd_ptr),y	; load byte 1
	sta PPUDATA	; write it
	iny		; increment offset
	lda (cmd_ptr),y	; load byte 2
	sta PPUDATA	; write it
	iny		; increment offset
	lda (cmd_ptr),y	; load byte 3
	sta PPUDATA	; write it
	iny		; increment offset
	lda (cmd_ptr),y	; load byte 4
	sta PPUDATA	; write it
	iny		; increment offset
	lda (cmd_ptr),y	; load byte 5
	sta PPUDATA	; write it
	iny		; increment offset
	lda (cmd_ptr),y	; load byte 6
	sta PPUDATA	; write it
	iny		; increment offset
	lda (cmd_ptr),y	; load byte 7
	sta PPUDATA	; write it
	iny		; increment offset
	lda (cmd_ptr),y	; load byte 8
	sta PPUDATA	; write it
	iny		; increment offset
	dex		; decrement count of remaining blocks
	bne -		; repeat until done

	; point cmd_ptr after buffer
done	jsr resync_cmd_ptr

	rts
	.pend

.send
